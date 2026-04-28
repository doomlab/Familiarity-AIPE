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
  dir.create("simulations/refined", showWarnings = FALSE, recursive = TRUE)

  log_file <- "simulations/refined/refine.log"

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

  manifest <- build_followup_manifest(
    jobs = jobs,
    out_dir = "simulations/pilot",
    lower_target = 70,
    upper_target = 95,
    min_start = 20,
    max_stop = 500,
    step = 5
  )

  manifest$start_sample_size[is.na(manifest$start_sample_size)] <- 20
  manifest$stop_sample_size[is.na(manifest$stop_sample_size)] <- 500

  results <- vector("list", nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    job <- jobs[i, , drop = FALSE]
    start_size <- manifest$start_sample_size[[i]]
    stop_size <- manifest$stop_sample_size[[i]]

    log_line(
      "refine ",
      job$name,
      " using start=",
      start_size,
      " stop=",
      stop_size,
      " nsim=500",
      log_file = log_file
    )

    results[[i]] <- tryCatch(
      run_job(
        job = job,
        udpipe_models = udpipe_models,
        stroke_tagger = stroke_tagger,
        skip_existing = TRUE,
        out_dir = "simulations/refined",
        start = start_size,
        stop = stop_size,
        increase = 5,
        nsim = 500,
        power_levels = c(70, 75, 80, 85, 90, 95),
        log_file = log_file
      ),
      error = function(e) {
        log_line("error ", job$name, ": ", conditionMessage(e), log_file = log_file)
        NULL
      }
    )
  }

  utils::write.csv(
    manifest,
    file = "simulations/refined/refine_grid_manifest.csv",
    row.names = FALSE
  )
  saveRDS(manifest, file = "simulations/refined/refine_grid_manifest.rds")

  invisible(results)
}

if (sys.nframe() == 0L) main()
