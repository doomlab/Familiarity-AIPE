#!/usr/bin/env Rscript

this_dir <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd[grep("^--file=", cmd)])
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(file_arg[[1]])))
  }
  getwd()
}

setwd(this_dir())
source("run_estimate_r_ss_batch.R")

main <- function() {
  attach_required_packages()
  load_shared_functions("functions.R")
  dir.create("simulations/pilot", showWarnings = FALSE, recursive = TRUE)

  log_file <- "simulations/pilot/pilot.log"
  cat("", file = log_file)

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
    out_dir = "simulations/pilot",
    start = 20,
    stop = 500,
    increase = 10,
    nsim = 10,
    power_levels = c(70, 75, 80, 85, 90, 95),
    log_file = log_file
  )

  manifest <- build_followup_manifest(
    jobs = jobs,
    out_dir = "simulations/pilot",
    lower_target = 70,
    upper_target = 95,
    min_start = 20,
    max_stop = 500,
    step = 5
  )

  utils::write.csv(
    manifest,
    file = "simulations/pilot/pilot_grid_manifest.csv",
    row.names = FALSE
  )
  saveRDS(manifest, file = "simulations/pilot/pilot_grid_manifest.rds")

  log_line("pilot manifest written", log_file = log_file)
  invisible(manifest)
}

if (sys.nframe() == 0L) main()
