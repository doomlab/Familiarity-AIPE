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

  # Each dataset runs as its own fresh Rscript subprocess (see
  # run_single_estimate_r_ss_refine_job.R) rather than looping in-process,
  # so the OS fully reclaims memory when that job's process exits instead
  # of it accumulating across many sequential jobs inside one long-lived R
  # session - large datasets were hanging under the old in-process loop.
  # Each subprocess loads its own udpipe/stroke models, so there's no
  # shared setup to do here.

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

  rscript_bin <- file.path(R.home("bin"), "Rscript")
  single_job_script <- "run_single_estimate_r_ss_refine_job.R"
  out_dir <- "simulations/refined"
  max_rows <- 1000

  results <- vector("list", nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    job <- jobs[i, , drop = FALSE]
    start_size <- manifest$start_sample_size[[i]]
    stop_size <- manifest$stop_sample_size[[i]]

    # Estimate n_items against the same max_rows cap run_job()/
    # run_job_chunked() apply, so the chunked/normal decision matches what
    # actually gets simulated in the subprocess.
    n_items_est <- tryCatch({
      job_df <- rio::import(job$data_file)
      if (nrow(job_df) > max_rows) job_df <- job_df[seq_len(max_rows), , drop = FALSE]
      n_distinct(job_df[[job$item_col]])
    }, error = function(e) NA_real_)

    chunked <- needs_chunked_run(
      n_items = n_items_est, start = start_size, stop = stop_size,
      increase = 5, nsim = 100
    )
    run_mode <- if (isTRUE(chunked)) "chunked" else "normal"

    log_line(
      "refine ",
      job$name,
      " using start=",
      start_size,
      " stop=",
      stop_size,
      " nsim=100 (subprocess, ",
      run_mode,
      ")",
      log_file = log_file
    )

    # Each dataset runs in its own fresh Rscript subprocess rather than
    # looping in-process, so the OS fully reclaims memory when that job's
    # process exits instead of it accumulating across many sequential
    # jobs inside one long-lived R session.
    status <- tryCatch(
      system2(
        rscript_bin,
        args = c(
          shQuote(single_job_script),
          shQuote(job$output),
          shQuote(out_dir),
          as.character(start_size),
          as.character(stop_size),
          "100",
          shQuote(log_file),
          run_mode,
          "true"
        )
      ),
      error = function(e) {
        log_line("error ", job$name, ": ", conditionMessage(e), log_file = log_file)
        NA_integer_
      }
    )

    if (!identical(status, 0L) && !is.na(status)) {
      log_line("subprocess for ", job$name, " exited with status ", status, log_file = log_file)
    }

    results[[i]] <- list(
      status = if (identical(status, 0L)) "done" else "error",
      file = file.path(out_dir, job$output)
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
