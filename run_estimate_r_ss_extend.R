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

  for (i in seq_len(nrow(jobs))) {
    job   <- jobs[i, , drop = FALSE]
    spec  <- extend_specs[extend_specs$output == job$output, ]

    log_line(
      "extend ", job$name,
      " (", job$output, ")",
      " start=", spec$start,
      " stop=",  spec$stop,
      " nsim=100",
      log_file = log_file
    )

    tryCatch(
      run_job(
        job           = job,
        udpipe_models = udpipe_models,
        stroke_tagger = stroke_tagger,
        skip_existing = FALSE,
        out_dir       = "simulations/refined",
        start         = spec$start,
        stop          = spec$stop,
        increase      = spec$step,
        nsim          = 100,
        power_levels  = c(70, 75, 80, 85, 90, 95),
        log_file      = log_file
      ),
      error = function(e) {
        log_line(
          "error ", job$name, ": ", conditionMessage(e),
          log_file = log_file
        )
        NULL
      }
    )
  }

  log_line("extension run complete", log_file = log_file)
  invisible(NULL)
}

if (sys.nframe() == 0L) main()
