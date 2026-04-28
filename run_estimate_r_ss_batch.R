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

LANG_MAP <- list(
  "chinese-gsd"  = "chinese-gsd-ud-2.5-191206.udpipe",
  "chinese-gsds" = "chinese-gsd-ud-2.5-191206.udpipe",
  "chinese"      = "chinese-gsd-ud-2.5-191206.udpipe",
  "dutch"        = "dutch-alpino-ud-2.5-191206.udpipe",
  "english"      = "english-ewt-ud-2.5-191206.udpipe",
  "french"       = "french-gsd-ud-2.5-191206.udpipe",
  "german"       = "german-gsd-ud-2.5-191206.udpipe",
  "indonesian"   = "indonesian-gsd-ud-2.5-191206.udpipe",
  "italian"      = "italian-isdt-ud-2.5-191206.udpipe",
  "spanish"      = "spanish-gsd-ud-2.5-191206.udpipe",
  "finnish"      = "finnish-tdt-ud-2.5-191206.udpipe",
  "croatian"     = "croatian-set-ud-2.5-191206.udpipe",
  "portuguese"   = "portuguese-bosque-ud-2.5-191206.udpipe",
  "russian"      = "russian-gsd-ud-2.5-191206.udpipe",
  "turkish"      = "turkish-imst-ud-2.5-191206.udpipe",
  "persian"      = "persian-seraji-ud-2.5-191206.udpipe"
)

item_col_to_lang <- function(item_col) {
  lang <- sub("^word_", "", item_col)
  lang <- gsub("_", "-", lang)
  lang <- gsub("-name$", "", lang)
  lang
}

merge_pos_categories <- function(upos) {
  dplyr::case_when(
    upos %in% c("NOUN", "PROPN", "PRON", "NUM") ~ "noun",
    upos %in% c("ADJ", "ADV") ~ "modifiers",
    upos %in% c("AUX", "VERB") ~ "verb",
    TRUE ~ "other"
  )
}

clean_udpipe_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x[is.na(x)] <- ""
  x
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

tag_df_with_pos_length <- function(df, item_col, udpipe_models,
                                   stroke_tagger = NULL,
                                   log_file = "estimate_r_ss_batch.log") {
  lang <- item_col_to_lang(item_col)
  model <- udpipe_models[[lang]]

  if (!is.null(model)) {
    udpipe_input <- clean_udpipe_text(df[[item_col]])
    anno_df <- tryCatch(
      {
        anno <- udpipe_annotate(model, x = udpipe_input, doc_id = seq_along(udpipe_input))
        as.data.frame(anno)
      },
      error = function(e) {
        warning(sprintf("UDPipe annotation failed for %s: %s", item_col, conditionMessage(e)))
        tibble::tibble(doc_id = seq_along(udpipe_input), upos = "other")
      }
    )

    if (nrow(anno_df) == 0) {
      pos_aggregated <- tibble::tibble(doc_id = seq_along(udpipe_input), upos = "other")
    } else {
      pos_aggregated <- anno_df %>%
        dplyr::group_by(doc_id) %>%
        dplyr::summarise(
          upos = dplyr::if_else(dplyr::n() > 1, "NOUN", dplyr::first(upos)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(doc_id = as.numeric(doc_id)) %>%
        dplyr::arrange(doc_id)
    }

    if (nrow(pos_aggregated) < nrow(df)) {
      pos_aggregated <- dplyr::bind_rows(
        pos_aggregated,
        tibble::tibble(
          doc_id = seq.int(nrow(pos_aggregated) + 1L, nrow(df)),
          upos = "other"
        )
      )
    }
    pos_aggregated <- pos_aggregated[seq_len(nrow(df)), , drop = FALSE]
    df$pos <- merge_pos_categories(pos_aggregated$upos)
  } else {
    log_line("skipping POS tagging for: ", item_col, log_file = log_file)
  }

  df$word_length <- nchar(clean_udpipe_text(df[[item_col]]))
  df$length_bucket <- dplyr::case_when(
    df$word_length >= 11 ~ "11+",
    df$word_length >= 3  ~ as.character(df$word_length),
    TRUE ~ NA_character_
  )

  if (grepl("chinese", lang, ignore.case = TRUE) && !is.null(stroke_tagger)) {
    df$stroke_count <- vapply(df[[item_col]], function(word) {
      tryCatch(sum(stroke_tagger$strokes(word)), error = function(e) NA_integer_)
    }, numeric(1))
    df$stroke_bucket <- dplyr::case_when(
      df$stroke_count >= 11 ~ "11+",
      df$stroke_count >= 1  ~ as.character(df$stroke_count),
      TRUE ~ NA_character_
    )
  }

  df
}

load_rmd_functions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  start_at <- which(grepl("^## Functions\\s*$", lines))[1]
  stop_at <- which(grepl("^## Data\\s*$", lines))[1]

  if (is.na(start_at) || is.na(stop_at) || stop_at <= start_at) {
    stop("Could not find the functions section in the Rmd.")
  }

  lines <- lines[(start_at + 1):(stop_at - 1)]

  in_chunk <- FALSE
  chunk_opts <- NULL
  chunk_code <- character()

  for (line in lines) {
    if (!in_chunk && grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      chunk_opts <- line
      chunk_code <- character()
      next
    }

    if (in_chunk && identical(trimws(line), "```")) {
      if (!grepl("eval\\s*=\\s*F", chunk_opts, ignore.case = TRUE)) {
        expr <- paste(chunk_code, collapse = "\n")
        if (nchar(trimws(expr)) > 0) {
          eval(parse(text = expr), envir = .GlobalEnv)
        }
      }
      in_chunk <- FALSE
      chunk_opts <- NULL
      chunk_code <- character()
      next
    }

    if (in_chunk) {
      chunk_code <- c(chunk_code, line)
    }
  }
}

log_line <- function(..., log_file = "estimate_r_ss_batch.log") {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
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
      "done",
      "done",
      "done",
      "done",
      "done",
      "done",
      "done",
      "done",
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
                    log_file = "estimate_r_ss_batch.log") {
  out_file <- file.path("simulations", job$output)

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

  df <- tag_df_with_pos_length(
    df, job$item_col, udpipe_models, stroke_tagger, log_file
  )

  saved_sim <- run_simulation_pipeline(
    df = df,
    item_col = job$item_col,
    mean_col = job$mean_col,
    sd_col = job$sd_col,
    n_per_item = job$n_per_item,
    min_score = job$min_score,
    max_score = job$max_score
  )

  save_data <- build_save_data(saved_sim)
  dir.create("simulations", showWarnings = FALSE, recursive = TRUE)
  saveRDS(save_data, out_file)

  rm(saved_sim, save_data)
  gc()

  log_line("done ", job$name, " -> ", out_file, log_file = log_file)
  invisible(list(status = "done", file = out_file))
}

main <- function() {
  rmd_path <- "estimate_r_ss.Rmd"
  if (!file.exists(rmd_path)) {
    stop("Missing Rmd file: ", rmd_path)
  }

  attach_required_packages()
  setwd(dirname(normalizePath(rmd_path)))
  dir.create("simulations", showWarnings = FALSE, recursive = TRUE)

  log_file <- "estimate_r_ss_batch.log"
  cat("", file = log_file)

  log_line("loading Rmd functions from ", rmd_path, log_file = log_file)
  load_rmd_functions(rmd_path)

  jobs <- build_jobs()
  jobs <- jobs[jobs$status != "done", , drop = FALSE]

  log_line("loading udpipe models", log_file = log_file)
  udpipe_models <- load_udpipe_models(jobs, log_file = log_file)

  reticulate::use_python(
    "/Users/erinbuchanan/Library/r-miniconda-arm64/envs/r-reticulate/bin/python",
    required = FALSE
  )

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

  results <- vector("list", nrow(jobs))
  for (i in seq_len(nrow(jobs))) {
    job <- jobs[i, , drop = FALSE]
    results[[i]] <- tryCatch(
      run_job(job, udpipe_models, stroke_tagger,
              skip_existing = TRUE, log_file = log_file),
      error = function(e) {
        log_line("error ", job$name, ": ", conditionMessage(e), log_file = log_file)
        NULL
      }
    )
  }

  invisible(results)
}

main()
