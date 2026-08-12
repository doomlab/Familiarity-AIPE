#!/usr/bin/env Rscript
# Extends refined simulations for jobs that did not reach 95% power.
# Overwrites existing refined .rds files for these jobs only.

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

# Completed: guasch (2026-06-19), raslescu (2026-06-22), chan (2026-06-22).
extend_specs <- data.frame(
  output = "kraljevic.rds",
  start  = 60L,
  stop   = 300L,
  step   = 5L,
  stringsAsFactors = FALSE
)

main <- function() {
  attach_required_packages()
  load_shared_functions("functions.R")

  log_file <- "simulations/refined/extend.log"
  log_line(
    "starting extension run for ", nrow(extend_specs), " jobs",
    log_file = log_file
  )

  all_jobs <- build_jobs()
  jobs <- all_jobs[all_jobs$output %in% extend_specs$output, , drop = FALSE]

  if (nrow(jobs) == 0) stop("No matching jobs found.")

  # Each dataset runs as its own fresh Rscript subprocess (see
  # run_single_estimate_r_ss_refine_job.R) rather than in-process, so the
  # OS fully reclaims memory when that job's process exits, and so a large
  # extended window (like kraljevic's start=60/stop=300) can fall back to
  # the chunked pipeline instead of holding every sample-size step's
  # samples in memory at once - large datasets were hanging under the old
  # in-process, non-chunked path.
  rscript_bin <- file.path(R.home("bin"), "Rscript")
  single_job_script <- "run_single_estimate_r_ss_refine_job.R"
  out_dir <- "simulations/refined"
  max_rows <- 1000

  for (i in seq_len(nrow(jobs))) {
    job  <- jobs[i, , drop = FALSE]
    spec <- extend_specs[extend_specs$output == job$output, ]

    n_items_est <- tryCatch({
      job_df <- rio::import(job$data_file)
      if (nrow(job_df) > max_rows) job_df <- job_df[seq_len(max_rows), , drop = FALSE]
      n_distinct(job_df[[job$item_col]])
    }, error = function(e) NA_real_)

    chunked <- needs_chunked_run(
      n_items = n_items_est, start = spec$start, stop = spec$stop,
      increase = spec$step, nsim = 100
    )
    run_mode <- if (isTRUE(chunked)) "chunked" else "normal"

    log_line(
      "extend ", job$name,
      " (", job$output, ")",
      " start=", spec$start,
      " stop=",  spec$stop,
      " nsim=100 (subprocess, ", run_mode, ")",
      log_file = log_file
    )

    status <- tryCatch(
      system2(
        rscript_bin,
        args = c(
          shQuote(single_job_script),
          shQuote(job$output),
          shQuote(out_dir),
          as.character(spec$start),
          as.character(spec$stop),
          "100",
          shQuote(log_file),
          run_mode,
          "false"
        )
      ),
      error = function(e) {
        log_line(
          "error ", job$name, ": ", conditionMessage(e),
          log_file = log_file
        )
        NA_integer_
      }
    )

    if (!identical(status, 0L) && !is.na(status)) {
      log_line("subprocess for ", job$name, " exited with status ", status, log_file = log_file)
    }
  }

  log_line("extension run complete", log_file = log_file)
  invisible(NULL)
}

if (sys.nframe() == 0L) main()
