#!/usr/bin/env Rscript

attach_required_packages <- function() {
  pkgs <- c("rio", "dplyr", "semanticprimeR", "purrr", "tidyr", "truncnorm", "psych",
            "udpipe", "reticulate", "tibble")

  for (pkg in pkgs) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
}

load_shared_functions <- function(path = "functions.R") {
  if (!file.exists(path)) {
    stop("Missing shared functions file: ", path)
  }
  source(path, local = .GlobalEnv)
}

LANG_MAP <- list(
  "chinese-gsd"  = "models/chinese-gsd-ud-2.5-191206.udpipe",
  "chinese-gsds" = "models/chinese-gsd-ud-2.5-191206.udpipe",
  "chinese"      = "models/chinese-gsd-ud-2.5-191206.udpipe",
  "dutch"        = "models/dutch-alpino-ud-2.5-191206.udpipe",
  "english"      = "models/english-ewt-ud-2.5-191206.udpipe",
  "french"       = "models/french-gsd-ud-2.5-191206.udpipe",
  "german"       = "models/german-gsd-ud-2.5-191206.udpipe",
  "indonesian"   = "models/indonesian-gsd-ud-2.5-191206.udpipe",
  "italian"      = "models/italian-isdt-ud-2.5-191206.udpipe",
  "spanish"      = "models/spanish-gsd-ud-2.5-191206.udpipe",
  "finnish"      = "models/finnish-tdt-ud-2.5-191206.udpipe",
  "croatian"     = "models/croatian-set-ud-2.5-191206.udpipe",
  "portuguese"   = "models/portuguese-bosque-ud-2.5-191206.udpipe",
  "russian"      = "models/russian-gsd-ud-2.5-191206.udpipe",
  "turkish"      = "models/turkish-imst-ud-2.5-191206.udpipe",
  "persian"      = "models/persian-seraji-ud-2.5-191206.udpipe"
)

item_col_to_lang <- function(item_col) {
  lang <- sub("^word_", "", item_col)
  lang <- gsub("_", "-", lang)
  lang <- gsub("-name$", "", lang)
  lang
}

load_udpipe_models <- function(jobs, log_file = "estimate_r_ss_batch.log") {
  langs <- unique(vapply(jobs$item_col, item_col_to_lang, character(1)))
  loaded <- list()
  for (lang in langs) {
    model_file <- LANG_MAP[[lang]]
    if (is.null(model_file) || !file.exists(model_file)) {
      log_line("no udpipe model for '", lang, "' - POS will be skipped", log_file = log_file)
      next
    }
    loaded[[lang]] <- udpipe_load_model(model_file)
    log_line("loaded udpipe model: ", lang, log_file = log_file)
  }
  loaded
}

log_line <- function(..., log_file = "estimate_r_ss_batch.log") {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

normalize_percent_below <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0 || all(is.na(x))) {
    return(x)
  }

  if (max(x, na.rm = TRUE) <= 1.5) {
    return(x * 100)
  }

  x
}

recommend_followup_window <- function(curve,
                                      lower_target = 70,
                                      upper_target = 95,
                                      min_start = 20,
                                      max_stop = 500,
                                      step = 5) {
  if (is.null(curve) || nrow(curve) == 0) {
    return(tibble::tibble(
      lower_target = lower_target,
      upper_target = upper_target,
      start_sample_size = NA_real_,
      stop_sample_size = NA_real_,
      lower_hit_sample_size = NA_real_,
      upper_hit_sample_size = NA_real_,
      max_percent_below = NA_real_
    ))
  }

  curve <- dplyr::arrange(
    dplyr::mutate(
      curve,
      percent_below = normalize_percent_below(percent_below),
      sample_size = as.numeric(sample_size)
    ),
    sample_size
  )

  lower_hit <- dplyr::slice_head(
    dplyr::filter(curve, percent_below >= lower_target),
    n = 1
  )
  upper_hit <- dplyr::slice_head(
    dplyr::filter(curve, percent_below >= upper_target),
    n = 1
  )

  lower_sample <- if (nrow(lower_hit) > 0) lower_hit$sample_size[[1]] else NA_real_
  upper_sample <- if (nrow(upper_hit) > 0) upper_hit$sample_size[[1]] else NA_real_

  start_sample_size <- if (!is.na(lower_sample)) {
    floor(lower_sample / step) * step
  } else {
    min_start
  }
  start_sample_size <- max(min_start, start_sample_size)

  stop_sample_size <- if (!is.na(upper_sample)) {
    ceiling(upper_sample / step) * step
  } else {
    max(curve$sample_size, na.rm = TRUE)
  }
  stop_sample_size <- min(max_stop, stop_sample_size)

  if (stop_sample_size < start_sample_size) {
    stop_sample_size <- min(
      max_stop,
      max(
        start_sample_size,
        ceiling(max(curve$sample_size, na.rm = TRUE) / step) * step
      )
    )
  }

  tibble::tibble(
    lower_target = lower_target,
    upper_target = upper_target,
    start_sample_size = start_sample_size,
    stop_sample_size = stop_sample_size,
    lower_hit_sample_size = lower_sample,
    upper_hit_sample_size = upper_sample,
    max_percent_below = max(curve$percent_below, na.rm = TRUE)
  )
}

extract_overall_curve <- function(run_obj) {
  if (is.list(run_obj) && !is.null(run_obj$overall_curve)) {
    return(run_obj$overall_curve)
  }
  if (is.list(run_obj) && !is.null(run_obj$proportion_summary)) {
    return(run_obj$proportion_summary)
  }
  NULL
}

build_followup_manifest <- function(jobs,
                                    out_dir,
                                    lower_target = 70,
                                    upper_target = 95,
                                    min_start = 20,
                                    max_stop = 500,
                                    step = 5) {
  purrr::pmap_dfr(
    jobs,
    function(name, data_file, item_col, mean_col, sd_col, n_per_item, min_score, max_score, output, status) {
      out_file <- file.path(out_dir, output)
      if (!file.exists(out_file)) {
        return(tibble::tibble(
          name = name,
          output = output,
          status = status,
          pilot_file = out_file,
          lower_target = lower_target,
          upper_target = upper_target,
          start_sample_size = NA_real_,
          stop_sample_size = NA_real_,
          lower_hit_sample_size = NA_real_,
          upper_hit_sample_size = NA_real_,
          max_percent_below = NA_real_
        ))
      }

      run_obj <- readRDS(out_file)
      curve <- extract_overall_curve(run_obj)
      window <- recommend_followup_window(
        curve = curve,
        lower_target = lower_target,
        upper_target = upper_target,
        min_start = min_start,
        max_stop = max_stop,
        step = step
      )

      dplyr::bind_cols(
        tibble::tibble(
          name = name,
          output = output,
          status = status,
          pilot_file = out_file
        ),
        window
      )
    }
  )
}

build_jobs <- function() {
  data.frame(
    name = c(
      "zechmeister",
      "gilhooly",
      "surprenant",
      "barca",
      "morrow",
      "morrow",
      "dedeyne",
      "ferrand",
      "janschewitz",
      "desrochers",
      "dellarosa",
      "desrochers10",
      "eilola",
      "eilola",
      "schroder",
      "ferre",
      "montefinese14",
      "moreno",
      "guasch",
      "hinojosa",
      "sianipar",
      "soares",
      "yee",
      "chedid",
      "bonin",
      "yao",
      "mikla",
      "scott",
      "verheyen",
      "lima",
      "liu",
      "petit",
      "ballot",
      "sarli",
      "raslescu",
      "su",
      "chan",
      "conca",
      "falcinelli",
      "falcinelli",
      "mahjou",
      "borsa",
      "kraljevic"
    ),
    data_file = c(
      "data/1975_Zechmeister_BRM.csv",
      "data/1977_Gilhooly_BRM.csv",
      "data/1999_Surprenant_BRM.csv",
      "data/2002_Barca_BRM.csv",
      "data/2005_Morrow_BRM.csv",
      "data/2005_Morrow_BRM.csv",
      "data/2008_De Deyne_BRM.csv",
      "data/2008_Ferrand_BRM.csv",
      "data/2008_Janschewitz_BRM.csv",
      "data/2009_Desrochers_BRM.csv",
      "data/2010_DellaRosa_BRM.csv",
      "data/2010_Desrochers_BRM.csv",
      "data/2010_Eilola_BRM.csv",
      "data/2010_Eilola_BRM.csv",
      "data/2011_Schroder_BRM.csv",
      "data/2012_Ferre_BRM.csv",
      "data/2014_Montefinese_BRM.csv",
      "data/2014_Moreno_BRM.csv",
      "data/2016_Guasch_BRM.csv",
      "data/2016_Hinojosa_BRM.csv",
      "data/2016_Sianipar_FrontPsychol.csv",
      "data/2017_Soares_BRM.csv",
      "data/2017_Yee_PLoSONE.csv",
      "data/2019_Chedid_BRM.csv",
      "data/2022_Bonin_BRM.csv",
      "data/2016_Yao_BRM.csv",
      "data/2018_Miklashevsky_JPsycholinguistRes.xlsx",
      "data/2019_Scott_BRM.csv",
      "data/2020_Verheyen_BRM.csv",
      "data/2021_de Lima_SAGEOpen.csv",
      "data/2021_Liu_FrontPsychol.csv",
      "data/2021_Peti-Stantic_BRM.xlsx",
      "data/2022_Ballot_BRM.xlsx",
      "data/2022_Sarli_BRM.xlsx",
      "data/2023_Raslescu_DIB.xlsx",
      "data/2023_Su_BRM_2.xlsx",
      "data/2024_Chan_BRM.xlsx",
      "data/2024_Conca_BRM.xlsx",
      "data/2024_Falcinellli_Collabra.xlsx",
      "data/2024_Falcinellli_Collabra.xlsx",
      "data/2024_Mahjoubnavaz_JPsycholinguistRes.xlsx",
      "data/2025_Borsa_BrainSci.xlsx",
      "data/2018_Kraljevic_Suvremena.csv"
    ),
    item_col = c(
      "word_english_name",
      "word_english",
      "word_english",
      "word_italian",
      "word_english",
      "word_english",
      "word_dutch",
      "word_french",
      "word_english",
      "word_french",
      "word_italian",
      "word_french",
      "word_finnish",
      "word_english",
      "word_german",
      "word_spanish",
      "word_italian",
      "word_spanish",
      "word_spanish",
      "word_spanish",
      "word_indonesian",
      "word_spanish",
      "word_chinese_gsd",
      "word_french",
      "word_french",
      "word_chinese-gsd",
      "word_russian",
      "word_english",
      "word_dutch",
      "word_portuguese",
      "word_chinese",
      "word_croatian",
      "word_french",
      "word_spanish",
      "word_english",
      "word_chinese-gsds",
      "word_chinese-gsd",
      "word_turkish",
      "word_italian",
      "word_italian",
      "word_persian",
      "word_italian",
      "word_croatian"
    ),
    mean_col = c(
      "frequency_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean_older",
      "familiar_mean_younger",
      "familiar_mean",
      "frequency_mean",
      "familiarity_mean",
      "frequency_sd",
      "familiar_mean",
      "frequency_mean",
      "familiar_mean_finnish",
      "familiar_mean_english",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "frequency_mean_all",
      "frequency_s_mean",
      "familiar_mean",
      "familiarity_mean_conceptual",
      "familiar_mean",
      "familiar_mean",
      "frequency_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean_portuguese",
      "familiarity_mean_all",
      "freq_subjective_mean_all",
      "freq_subjective_mean_all",
      "familiar_mean_all",
      "freq_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean",
      "familiar_mean_persian",
      "familiar_mean",
      "familiar_mean"
    ),
    sd_col = c(
      "frequency_sd",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd",
      "frequency_sd_older",
      "familiar_sd_younger",
      "familiar_sd",
      "frequency_sd",
      "familiarity_sd",
      "frequency_sd",
      "familiar_sd",
      "frequency_sd",
      "familiar_sd_finnish",
      "familiar_mean_english",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd",
      "frequency_sd_all",
      "frequency_s_sd",
      "familiar_sd",
      "familiarity_sd_conceptual",
      "familiar_sd",
      "familiar_sd",
      "frequency_sd",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd_portuguese",
      "familiarity_sd_all",
      "freq_subjective_sd_all",
      "freq_subjective_sd_all",
      "familiar_sd_all",
      "freq_sd",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd",
      "familiar_sd",
      "freq_subjective_sd",
      "familiar_sd_persian",
      "familiar_sd",
      "familiar_sd"
    ),
    n_per_item = c(
      40,
      40,
      63,
      44,
      54,
      53,
      28,
      28,
      78,
      56,
      35,
      102,
      152,
      68,
      20,
      25,
      20,
      15,
      20,
      30,
      61,
      42,
      32,
      23,
      28,
      48,
      22,
      22,
      13,
      18,
      20,
      19,
      137,
      23,
      198,
      21,
      20,
      8,
      20,
      20,
      50,
      20,
      33
    ),
    min_score = c(
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1
    ),
    max_score = c(
      9,
      7,
      7,
      7,
      7,
      7,
      5,
      7,
      9,
      7,
      7,
      7,
      9,
      9,
      5,
      9,
      9,
      5,
      7,
      9,
      9,
      7,
      5,
      100,
      5,
      9,
      7,
      7,
      7,
      7,
      9,
      5,
      7,
      9,
      100,
      7,
      7,
      7,
      7,
      7,
      5,
      7,
      5
    ),
    output = c(
      "zechmeister.rds",
      "gilhooly.rds",
      "surprenant.rds",
      "barca.rds",
      "morrow_old.rds",
      "morrow_young.rds",
      "dedeyne.rds",
      "ferrand.rds",
      "janschewitz.rds",
      "desrochers.rds",
      "dellarosa.rds",
      "desrochers10.rds",
      "eilola_finnish.rds",
      "eilola_english.rds",
      "schroder.rds",
      "ferre.rds",
      "montefinese14.rds",
      "moreno.rds",
      "guasch.rds",
      "hinojosa.rds",
      "sianipar.rds",
      "soares.rds",
      "yee.rds",
      "chedid.rds",
      "bonin.rds",
      "yao.rds",
      "mikla.rds",
      "scott.rds",
      "verheyen.rds",
      "lima.rds",
      "liu.rds",
      "petit.rds",
      "ballot.rds",
      "sarli.rds",
      "raslescu.rds",
      "su.rds",
      "chan.rds",
      "conca.rds",
      "falcinelli.rds",
      "falcinelli_freq.rds",
      "mahjou.rds",
      "borsa.rds",
      "kraljevic.rds"
    ),
    status = c(
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready",
      "ready"
    ),
    stringsAsFactors = FALSE
  )
}

run_job <- function(job, udpipe_models, stroke_tagger = NULL,
                    skip_existing = TRUE,
                    out_dir = "simulations",
                    start = 20,
                    stop = 100,
                    increase = 5,
                    nsim = 100,
                    power_levels = c(80, 85, 90, 95),
                    max_rows = 1000,
                    log_file = "estimate_r_ss_batch.log") {
  out_file <- file.path(out_dir, job$output)

  if (skip_existing && file.exists(out_file)) {
    log_line("skip ", job$name, " -> existing ", out_file, log_file = log_file)
    return(invisible(list(status = "skipped", file = out_file)))
  }

  log_line("start ", job$name, " -> ", out_file, log_file = log_file)

  df <- rio::import(job$data_file)
  on.exit({
    rm(df)
    gc()
  }, add = TRUE)

  log_line("imported ", job$name, " rows: ", nrow(df), log_file = log_file)

  if (!is.null(max_rows) && nrow(df) > max_rows) {
    log_line(
      "sampling ",
      max_rows,
      " rows for ",
      job$name,
      " test run",
      log_file = log_file
    )
    df <- dplyr::slice_sample(df, n = max_rows)
  }

  df <- tag_df_with_pos_length(
    df, job$item_col, udpipe_models, stroke_tagger
  )

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  saved_sim <- run_simulation_pipeline(
    df = df,
    item_col = job$item_col,
    mean_col = job$mean_col,
    sd_col = job$sd_col,
    n_per_item = job$n_per_item,
    min_score = job$min_score,
    max_score = job$max_score,
    start = start,
    stop = stop,
    increase = increase,
    nsim = nsim,
    power_levels = power_levels,
    out_file = out_file
  )

  save_data <- build_save_data(saved_sim)
  saveRDS(save_data, out_file)

  rm(saved_sim, save_data)
  gc()

  log_line("done ", job$name, " -> ", out_file, log_file = log_file)
  invisible(list(status = "done", file = out_file))
}

run_jobs_stage <- function(jobs,
                           udpipe_models,
                           stroke_tagger = NULL,
                           skip_existing = TRUE,
                           out_dir = "simulations",
                           start = 20,
                           stop = 100,
                           increase = 5,
                           nsim = 100,
                           power_levels = c(80, 85, 90, 95),
                           max_rows = 1000,
                           log_file = "estimate_r_ss_batch.log") {
  results <- vector("list", nrow(jobs))
  for (i in seq_len(nrow(jobs))) {
    job <- jobs[i, , drop = FALSE]
    results[[i]] <- tryCatch(
      run_job(
        job,
        udpipe_models,
        stroke_tagger,
        skip_existing = skip_existing,
        out_dir = out_dir,
        start = start,
        stop = stop,
        increase = increase,
        nsim = nsim,
        power_levels = power_levels,
        max_rows = max_rows,
        log_file = log_file
      ),
      error = function(e) {
        log_line("error ", job$name, ": ", conditionMessage(e), log_file = log_file)
        NULL
      }
    )
  }

  invisible(results)
}

main <- function() {
  setwd(this_dir())
  attach_required_packages()
  load_shared_functions("functions.R")
  dir.create("simulations", showWarnings = FALSE, recursive = TRUE)

  log_file <- "estimate_r_ss_batch.log"

  log_line("loading shared functions from functions.R", log_file = log_file)

  jobs <- build_jobs()
  jobs <- jobs[jobs$status != "done", , drop = FALSE]

  log_line("loading udpipe models", log_file = log_file)
  udpipe_models <- load_udpipe_models(jobs, log_file = log_file)

  local({
    py <- tryCatch(
      reticulate::conda_python("r-reticulate"),
      error = function(e) NULL
    )
    if (!is.null(py) && file.exists(py)) {
      reticulate::use_python(py, required = FALSE)
    }
  })

  stroke_tagger <- tryCatch(
    reticulate::import("strokes"),
    error = function(e) {
      log_line(
        "strokes Python package unavailable - stroke analysis skipped",
        log_file = log_file
      )
      NULL
    }
  )

  shard_total <- as.integer(Sys.getenv("SHARD_TOTAL", "1"))
  shard_index <- as.integer(Sys.getenv("SHARD_INDEX", "1"))

  if (!is.na(shard_total) && shard_total > 1) {
    if (is.na(shard_index) || shard_index < 1 || shard_index > shard_total) {
      stop("Set SHARD_INDEX to a value between 1 and SHARD_TOTAL.")
    }
    keep <- ((seq_len(nrow(jobs)) - 1L) %% shard_total) + 1L == shard_index
    jobs <- jobs[keep, , drop = FALSE]
    log_line(
      "using shard ",
      shard_index,
      "/",
      shard_total,
      " with ",
      nrow(jobs),
      " jobs",
      log_file = log_file
    )
  }

  if (nrow(jobs) == 0) {
    log_line("no jobs left to run", log_file = log_file)
    return(invisible(NULL))
  }

  run_jobs_stage(
    jobs = jobs,
    udpipe_models = udpipe_models,
    stroke_tagger = stroke_tagger,
    skip_existing = TRUE,
    out_dir = "simulations",
    start = 20,
    stop = 100,
    increase = 5,
    nsim = 100,
    power_levels = c(80, 85, 90, 95),
    max_rows = 1000,
    log_file = log_file
  )
}

if (sys.nframe() == 0L) main()
