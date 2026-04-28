#!/usr/bin/env Rscript

attach_required_packages <- function() {
  pkgs <- c("rio", "dplyr", "semanticprimeR", "purrr", "tidyr", "truncnorm", "psych")

  for (pkg in pkgs) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
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

run_job <- function(job, skip_existing = TRUE, log_file = "estimate_r_ss_batch.log") {
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
      run_job(job, skip_existing = TRUE, log_file = log_file),
      error = function(e) {
        log_line("error ", job$name, ": ", conditionMessage(e), log_file = log_file)
        NULL
      }
    )
  }

  invisible(results)
}

main()
