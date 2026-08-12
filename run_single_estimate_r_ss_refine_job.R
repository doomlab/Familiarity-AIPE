#!/usr/bin/env Rscript

# Runs exactly one refine/extend-stage dataset job, then exits.
# run_estimate_r_ss_refine.R and run_estimate_r_ss_extend.R spawn one of
# these as a fresh subprocess per dataset instead of looping many jobs
# inside a single long-lived R session - rm()/gc() only reclaims memory
# for R's own heap to reuse, it doesn't hand pages back to the OS, so a
# session working through dozens of sequential jobs can accumulate enough
# resident memory to make later, larger jobs hang or crash. A fresh
# process per job guarantees the OS reclaims everything when that job's
# process exits.
#
# Usage:
#   Rscript run_single_estimate_r_ss_refine_job.R <job_output> <out_dir> <start> <stop> <nsim> <log_file> [mode] [skip_existing]
#
# <job_output> is the job's unique output filename (e.g. "kraljevic.rds"),
# used to look it up in build_jobs() - job *names* are not unique (e.g.
# "morrow" covers both morrow_old.rds and morrow_young.rds).
#
# <mode> is "normal" (default, if omitted) or "chunked". "chunked" routes
# to run_job_chunked(), which simulates/summarizes one sample size at a
# time instead of holding the whole window in memory at once - for jobs
# whose window is large enough that the normal path risks exhausting
# memory. It still tags and reports POS/length subgroups; only the stroke
# breakdown is skipped. See needs_chunked_run() in run_estimate_r_ss_batch.R.
#
# <skip_existing> is "true" (default, if omitted) or "false". Extend runs
# pass "false" since they intentionally overwrite an existing refined
# output for a job that didn't reach target power.

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
attach_required_packages()
load_shared_functions("functions.R")

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 6:8) {
  stop("Usage: Rscript run_single_estimate_r_ss_refine_job.R <job_output> <out_dir> <start> <stop> <nsim> <log_file> [mode] [skip_existing]")
}

job_output    <- args[[1]]
out_dir       <- args[[2]]
start_size    <- as.numeric(args[[3]])
stop_size     <- as.numeric(args[[4]])
nsim          <- as.numeric(args[[5]])
log_file      <- args[[6]]
mode          <- if (length(args) >= 7) args[[7]] else "normal"
skip_existing <- if (length(args) >= 8) as.logical(args[[8]]) else TRUE

if (!mode %in% c("normal", "chunked")) {
  stop("mode must be 'normal' or 'chunked', got: ", mode)
}
if (is.na(skip_existing)) {
  stop("skip_existing must be 'true' or 'false', got: ", args[[8]])
}

jobs <- build_jobs()
job <- jobs[jobs$output == job_output, , drop = FALSE]

if (nrow(job) == 0) {
  stop("job with output '", job_output, "' not found in build_jobs()")
}

udpipe_models <- load_udpipe_models(job, log_file = log_file)

if (mode == "chunked") {
  run_job_chunked(
    job = job,
    udpipe_models = udpipe_models,
    skip_existing = skip_existing,
    out_dir = out_dir,
    start = start_size,
    stop = stop_size,
    increase = 5,
    nsim = nsim,
    power_levels = c(70, 75, 80, 85, 90, 95),
    log_file = log_file
  )
} else {
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

  run_job(
    job = job,
    udpipe_models = udpipe_models,
    stroke_tagger = stroke_tagger,
    skip_existing = skip_existing,
    out_dir = out_dir,
    start = start_size,
    stop = stop_size,
    increase = 5,
    nsim = nsim,
    power_levels = c(70, 75, 80, 85, 90, 95),
    log_file = log_file
  )
}
