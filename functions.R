run_population_pipeline <- function(
  population,
  min_score = 0,
  max_score = 9,
  n_per_item = 68,
  start = 20,
  stop = 100,
  increase = 5,
  nsim = 500,
  power_levels = c(80, 85, 90, 95)
) {
  population <- population %>%
    filter(is.finite(.data[["score"]]))

  cutoff <- calculate_cutoff(
    population = population,
    grouping_items = "item",
    score = "score",
    minimum = min_score,
    maximum = max_score
  )

  samples <- simulate_samples(
    start = start,
    stop = stop,
    increase = increase,
    population = population,
    replace = TRUE,
    nsim = nsim,
    grouping_items = "item"
  )

  samples <- purrr::map(samples, function(sample_dat) {
    sample_dat %>%
      filter(is.finite(.data[["score"]]))
  })

  proportion_summary <- calculate_proportion(
    samples = samples,
    cutoff = cutoff$cutoff,
    grouping_items = "item",
    score = "score"
  )

  cat(sprintf("  cutoff: %.3f  prop_var: %.4f  n_items: %d\n",
              cutoff$cutoff, cutoff$prop_var, n_distinct(population$item)))
  cat(sprintf("  proportion_summary rows: %d  prop range: [%.3f, %.3f]\n",
              nrow(proportion_summary),
              if (nrow(proportion_summary) > 0) min(proportion_summary$percent_below, na.rm = TRUE) else NA,
              if (nrow(proportion_summary) > 0) max(proportion_summary$percent_below, na.rm = TRUE) else NA))

  corrected_summary <- calculate_correction(
    proportion_summary = proportion_summary,
    pilot_sample_size = n_per_item,
    proportion_variability = cutoff$prop_var,
    power_levels = power_levels
  )

  cat(sprintf("  corrected_summary rows: %d\n", nrow(corrected_summary)))

  list(
    cutoff = cutoff,
    samples = samples,
    proportion_summary = proportion_summary,
    corrected_summary = corrected_summary
  )
}

summarise_split_half <- function(dat) {
  if (is.null(dat) || nrow(dat) == 0) {
    return(NULL)
  }

  if ("group" %in% names(dat)) {
    dat %>%
      group_by(sample_size, group) %>%
      summarise(reliability_m = mean(reliability, na.rm = TRUE), .groups = "drop")
  } else {
    dat %>%
      group_by(sample_size) %>%
      summarise(reliability_m = mean(reliability, na.rm = TRUE), .groups = "drop")
  }
}

build_save_data <- function(saved_sim) {
  list(
    overall_power = saved_sim$corrected_summary,
    overall_curve = saved_sim$proportion_summary,
    overall_rel = summarise_split_half(saved_sim$split_half_rel),
    pos_power = purrr::map(saved_sim$pos_pipelines, "corrected_summary"),
    pos_curve = purrr::map(saved_sim$pos_pipelines, "proportion_summary"),
    pos_rel = summarise_split_half(saved_sim$split_half_by_pos),
    length_power = purrr::map(saved_sim$length_pipelines, "corrected_summary"),
    length_curve = purrr::map(saved_sim$length_pipelines, "proportion_summary"),
    length_rel = summarise_split_half(saved_sim$split_half_by_length),
    stroke_power = purrr::map(saved_sim$stroke_pipelines, "corrected_summary"),
    stroke_curve = purrr::map(saved_sim$stroke_pipelines, "proportion_summary"),
    stroke_rel = summarise_split_half(saved_sim$split_half_by_stroke)
  )
}

load_and_summarize_simulations <- function(sim_dir = "simulations") {
  if (!dir.exists(sim_dir)) {
    stop(sprintf("Simulation directory not found: %s", sim_dir))
  }

  sim_files <- list.files(sim_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(sim_files) == 0) {
    stop(sprintf("No .rds files found in %s", sim_dir))
  }

  run_ids <- tools::file_path_sans_ext(basename(sim_files))
  runs <- purrr::map(sim_files, readRDS)

  extract_overall_power <- function(run_obj) {
    if (is.list(run_obj) && !is.null(run_obj$overall_power)) {
      return(run_obj$overall_power)
    }
    if (is.list(run_obj) && !is.null(run_obj$corrected_summary)) {
      return(run_obj$corrected_summary)
    }
    NULL
  }

  extract_overall_rel <- function(run_obj) {
    if (is.list(run_obj) && !is.null(run_obj$overall_rel)) {
      return(run_obj$overall_rel)
    }
    if (is.list(run_obj) && !is.null(run_obj$split_half_rel)) {
      return(run_obj$split_half_rel)
    }
    NULL
  }

  extract_subgroup_power <- function(run_obj, field_name) {
    if (!is.list(run_obj) || is.null(run_obj[[field_name]])) {
      return(tibble::tibble())
    }

    purrr::imap_dfr(run_obj[[field_name]], function(tbl, group_name) {
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(tibble::tibble())
      }

      tbl %>%
        mutate(group = as.character(group_name))
    })
  }

  extract_subgroup_rel <- function(run_obj, field_name) {
    if (!is.list(run_obj) || is.null(run_obj[[field_name]])) {
      return(tibble::tibble())
    }

    run_obj[[field_name]]
  }

  ordered_group_levels <- function(x) {
    x <- unique(as.character(x))
    if (all(x %in% c("noun", "modifiers", "verb", "other"))) {
      return(c("noun", "modifiers", "verb", "other")[c("noun", "modifiers", "verb", "other") %in% x])
    }

    x_num <- suppressWarnings(as.numeric(gsub("\\+$", "", x)))
    if (all(!is.na(x_num))) {
      return(x[order(x_num, x)])
    }

    x[order(x)]
  }

  summarize_subgroup_targets <- function(power_tbl, rel_tbl, target_power = 80, target_rel = 0.80) {
    power_summary <- tibble::tibble()
    rel_summary <- tibble::tibble()

    if (!is.null(power_tbl) && nrow(power_tbl) > 0) {
      power_summary <- power_tbl %>%
        mutate(
          percent_below = as.numeric(percent_below),
          corrected_sample_size = as.numeric(corrected_sample_size)
        ) %>%
        filter(percent_below == target_power) %>%
        group_by(group) %>%
        summarise(
          n_runs = sum(!is.na(corrected_sample_size)),
          mean_recommended_sample_size = mean(corrected_sample_size, na.rm = TRUE),
          median_recommended_sample_size = median(corrected_sample_size, na.rm = TRUE),
          recommended_sample_size = ceiling(median_recommended_sample_size),
          sd_recommended_sample_size = sd(corrected_sample_size, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(target = paste0(target_power, "% power"))
    }

    if (!is.null(rel_tbl) && nrow(rel_tbl) > 0) {
      rel_summary <- rel_tbl %>%
        mutate(
          sample_size = as.numeric(sample_size),
          reliability_m = as.numeric(reliability_m)
        ) %>%
        group_by(group, sample_size) %>%
        summarise(
          median_reliability = median(reliability_m, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(group) %>%
        filter(median_reliability >= target_rel) %>%
        arrange(sample_size) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        mutate(
          target = paste0(target_rel * 100, "% reliability"),
          recommended_sample_size = sample_size
        ) %>%
        select(group, target, recommended_sample_size, median_reliability)
    }

    summary_parts <- list()
    if (nrow(power_summary) > 0) {
      summary_parts[[length(summary_parts) + 1]] <- power_summary %>%
        select(group, target, recommended_sample_size)
    }
    if (nrow(rel_summary) > 0) {
      summary_parts[[length(summary_parts) + 1]] <- rel_summary %>%
        select(group, target, recommended_sample_size)
    }

    summary_tbl <- if (length(summary_parts) > 0) {
      bind_rows(summary_parts) %>%
        mutate(group = as.character(group))
    } else {
      tibble::tibble(group = character(), target = character(), recommended_sample_size = numeric())
    }

    plot_tbl <- summary_tbl %>%
      mutate(group = factor(group, levels = ordered_group_levels(group)))

    plot_obj <- NULL
    if (nrow(plot_tbl) > 0) {
      plot_obj <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = group, y = recommended_sample_size, color = target)) +
        ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.4), size = 2.4) +
        ggplot2::geom_line(ggplot2::aes(group = target), position = ggplot2::position_dodge(width = 0.4), linewidth = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = NULL,
          y = "Recommended sample size",
          color = NULL
        ) +
        ggplot2::theme_minimal()
    }

    list(
      power_summary = power_summary,
      reliability_summary = rel_summary,
      summary = summary_tbl,
      plot = plot_obj
    )
  }

  overall_power <- purrr::imap_dfr(runs, function(run_obj, i) {
    power_tbl <- extract_overall_power(run_obj)
    if (is.null(power_tbl) || nrow(power_tbl) == 0) {
      return(tibble::tibble())
    }

    power_tbl %>%
      mutate(
        run_id = run_ids[[i]],
        source_file = sim_files[[i]]
      )
  })

  overall_rel <- purrr::imap_dfr(runs, function(run_obj, i) {
    rel_tbl <- extract_overall_rel(run_obj)
    if (is.null(rel_tbl) || nrow(rel_tbl) == 0) {
      return(tibble::tibble())
    }

    rel_tbl %>%
      mutate(
        run_id = run_ids[[i]],
        source_file = sim_files[[i]]
      )
  })

  pos_power <- purrr::imap_dfr(runs, function(run_obj, i) {
    power_tbl <- extract_subgroup_power(run_obj, "pos_power")
    if (nrow(power_tbl) == 0) return(tibble::tibble())
    power_tbl %>%
      mutate(run_id = run_ids[[i]], source_file = sim_files[[i]])
  })

  pos_rel <- purrr::imap_dfr(runs, function(run_obj, i) {
    rel_tbl <- extract_subgroup_rel(run_obj, "pos_rel")
    if (nrow(rel_tbl) == 0) return(tibble::tibble())
    rel_tbl %>%
      mutate(run_id = run_ids[[i]], source_file = sim_files[[i]])
  })

  length_power <- purrr::imap_dfr(runs, function(run_obj, i) {
    power_tbl <- extract_subgroup_power(run_obj, "length_power")
    if (nrow(power_tbl) == 0) return(tibble::tibble())
    power_tbl %>%
      mutate(run_id = run_ids[[i]], source_file = sim_files[[i]])
  })

  length_rel <- purrr::imap_dfr(runs, function(run_obj, i) {
    rel_tbl <- extract_subgroup_rel(run_obj, "length_rel")
    if (nrow(rel_tbl) == 0) return(tibble::tibble())
    rel_tbl %>%
      mutate(run_id = run_ids[[i]], source_file = sim_files[[i]])
  })

  stroke_power <- purrr::imap_dfr(runs, function(run_obj, i) {
    power_tbl <- extract_subgroup_power(run_obj, "stroke_power")
    if (nrow(power_tbl) == 0) return(tibble::tibble())
    power_tbl %>%
      mutate(run_id = run_ids[[i]], source_file = sim_files[[i]])
  })

  stroke_rel <- purrr::imap_dfr(runs, function(run_obj, i) {
    rel_tbl <- extract_subgroup_rel(run_obj, "stroke_rel")
    if (nrow(rel_tbl) == 0) return(tibble::tibble())
    rel_tbl %>%
      mutate(run_id = run_ids[[i]], source_file = sim_files[[i]])
  })

  if (nrow(overall_power) == 0 && nrow(overall_rel) == 0 &&
      nrow(pos_power) == 0 && nrow(pos_rel) == 0 &&
      nrow(length_power) == 0 && nrow(length_rel) == 0 &&
      nrow(stroke_power) == 0 && nrow(stroke_rel) == 0) {
    return(list(
      files = sim_files,
      runs = runs,
      overall_power = overall_power,
      overall_rel = overall_rel,
      recommendation_summary = tibble::tibble(),
      recommendation_plot = NULL
    ))
  }

  if (nrow(overall_power) > 0) {
    recommendation_summary <- overall_power %>%
      mutate(
        percent_below = as.numeric(percent_below),
        corrected_sample_size = as.numeric(corrected_sample_size)
      ) %>%
      group_by(percent_below) %>%
      summarise(
        n_runs = sum(!is.na(corrected_sample_size)),
        mean_recommended_sample_size = mean(corrected_sample_size, na.rm = TRUE),
        median_recommended_sample_size = median(corrected_sample_size, na.rm = TRUE),
        recommended_sample_size = ceiling(median_recommended_sample_size),
        sd_recommended_sample_size = sd(corrected_sample_size, na.rm = TRUE),
        min_recommended_sample_size = min(corrected_sample_size, na.rm = TRUE),
        max_recommended_sample_size = max(corrected_sample_size, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(percent_below) %>%
      mutate(recommended_sample_size = cummax(recommended_sample_size))

    recommendation_plot <- ggplot2::ggplot(
      overall_power %>% mutate(percent_below = factor(percent_below, levels = as.character(recommendation_summary$percent_below))),
      ggplot2::aes(x = percent_below, y = corrected_sample_size)
    ) +
      ggplot2::geom_boxplot(fill = "#d9e7ff", outlier.alpha = 0.2, width = 0.6) +
      ggplot2::geom_jitter(width = 0.12, alpha = 0.35, size = 1.4) +
      ggplot2::geom_line(
        data = recommendation_summary,
        ggplot2::aes(x = factor(percent_below, levels = as.character(recommendation_summary$percent_below)),
                     y = recommended_sample_size,
                     group = 1),
        color = "#b00020",
        linewidth = 0.9
      ) +
      ggplot2::geom_point(
        data = recommendation_summary,
        ggplot2::aes(x = factor(percent_below, levels = as.character(recommendation_summary$percent_below)),
                     y = recommended_sample_size),
        color = "#b00020",
        size = 2.5
      ) +
      ggplot2::labs(
        x = "Target Power",
        y = "Corrected Sample Size",
        title = "Recommended sample size by target power"
      ) +
      ggplot2::theme_minimal()
  } else {
    recommendation_summary <- tibble::tibble()
    recommendation_plot <- NULL
  }

  if (nrow(overall_rel) > 0) {
    reliability_summary <- overall_rel %>%
      mutate(
        sample_size = as.numeric(sample_size),
        reliability_m = as.numeric(reliability_m)
      ) %>%
      group_by(sample_size) %>%
      summarise(
        n_runs = sum(!is.na(reliability_m)),
        mean_reliability = mean(reliability_m, na.rm = TRUE),
        median_reliability = median(reliability_m, na.rm = TRUE),
        sd_reliability = sd(reliability_m, na.rm = TRUE),
        min_reliability = min(reliability_m, na.rm = TRUE),
        max_reliability = max(reliability_m, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(sample_size)

    reliability_targets <- c(0.80, 0.85, 0.90, 0.95)
    reliability_recommendation <- purrr::map_dfr(reliability_targets, function(target) {
      hit <- reliability_summary %>%
        filter(median_reliability >= target) %>%
        arrange(sample_size) %>%
        slice_head(n = 1)

      if (nrow(hit) == 0) {
        return(tibble::tibble(
          target_reliability = target,
          recommended_sample_size = NA_real_,
          median_reliability = NA_real_
        ))
      }

      tibble::tibble(
        target_reliability = target,
        recommended_sample_size = hit$sample_size[[1]],
        median_reliability = hit$median_reliability[[1]]
      )
    }) %>%
      mutate(recommended_sample_size = as.numeric(recommended_sample_size))

    reliability_plot <- ggplot2::ggplot(
      reliability_summary,
      ggplot2::aes(x = sample_size, y = median_reliability)
    ) +
      ggplot2::geom_line(color = "#1f4e79", linewidth = 0.9) +
      ggplot2::geom_point(color = "#1f4e79", size = 2) +
      ggplot2::geom_hline(
        data = reliability_recommendation,
        ggplot2::aes(yintercept = target_reliability),
        linetype = "dashed",
        color = "#b00020"
      ) +
      ggplot2::geom_vline(
        data = reliability_recommendation %>% filter(!is.na(recommended_sample_size)),
        ggplot2::aes(xintercept = recommended_sample_size),
        linetype = "dotted",
        color = "#b00020"
      ) +
      ggplot2::labs(
        x = "Sample Size",
        y = "Median Split-Half Reliability",
        title = "Sample size needed to reach target reliability"
      ) +
      ggplot2::theme_minimal()
  } else {
    reliability_summary <- tibble::tibble()
    reliability_recommendation <- tibble::tibble()
    reliability_plot <- NULL
  }

  pos_targets <- summarize_subgroup_targets(pos_power, pos_rel, target_power = 80, target_rel = 0.80)
  length_targets <- summarize_subgroup_targets(length_power, length_rel, target_power = 80, target_rel = 0.80)
  stroke_targets <- summarize_subgroup_targets(stroke_power, stroke_rel, target_power = 80, target_rel = 0.80)

  list(
    files = sim_files,
    runs = runs,
    overall_power = overall_power,
    overall_rel = overall_rel,
    recommendation_summary = recommendation_summary,
    recommendation_plot = recommendation_plot,
    reliability_summary = reliability_summary,
    reliability_recommendation = reliability_recommendation,
    reliability_plot = reliability_plot,
    pos_targets = pos_targets,
    length_targets = length_targets,
    stroke_targets = stroke_targets
  )
}

run_simulation_core <- function(
  df,
  item_col,
  mean_col,
  sd_col,
  n_per_item = 68,
  min_score = 0,
  max_score = 9,
  start = 20,
  stop = 100,
  increase = 5,
  nsim = 500,
  power_levels = c(80, 85, 90, 95)
) {
  cat(sprintf("Core run started: %s\n", item_col))
  cat(sprintf("  rows in input: %d\n", nrow(df)))

  items <- df %>%
    mutate(
      item = .data[[item_col]],
      mean = .data[[mean_col]],
      sd   = .data[[sd_col]],
      n    = n_per_item
    ) %>%
    select(item, mean, sd, n, everything()) %>%
    filter(!is.na(item) & !is.na(mean) & !is.na(sd))

  sim_data <- items %>%
    mutate(scores = pmap(list(mean, sd, n),
                         ~ rtruncnorm(..3,
                                      a = min_score,
                                      b = max_score,
                                      mean = ..1,
                                      sd = ..2))) %>%
    unnest(scores) %>%
    mutate(score = round(scores)) %>%
    filter(is.finite(.data[["score"]])) %>%
    select(item, score, any_of(c("pos", "length_bucket", "stroke_bucket")))

  cat(sprintf("  simulated data ready: %d rows\n", nrow(sim_data)))

  pipeline_results <- run_population_pipeline(
    population = sim_data,
    min_score = min_score,
    max_score = max_score,
    n_per_item = n_per_item,
    start = start,
    stop = stop,
    increase = increase,
    nsim = nsim,
    power_levels = power_levels
  )
  cutoff <- pipeline_results$cutoff
  samples <- pipeline_results$samples
  proportion_summary <- pipeline_results$proportion_summary
  corrected_summary <- pipeline_results$corrected_summary

  cat("Core run done\n")

  return(list(
    sim_data = sim_data,
    cutoff = cutoff,
    samples = samples,
    proportion_summary = proportion_summary,
    corrected_summary = corrected_summary
  ))
}

run_simulation_pipeline <- function(
  df,
  item_col,
  mean_col,
  sd_col,
  n_per_item = 68,
  min_score = 0,
  max_score = 9,
  start = 20,
  stop = 100,
  increase = 5,
  nsim = 500,
  power_levels = c(80, 85, 90, 95),
  length_col = "word_length"
) {
  cat(sprintf("Pipeline started: %s\n", item_col))

  core_results <- run_simulation_core(
    df = df,
    item_col = item_col,
    mean_col = mean_col,
    sd_col = sd_col,
    n_per_item = n_per_item,
    min_score = min_score,
    max_score = max_score,
    start = start,
    stop = stop,
    increase = increase,
    nsim = nsim,
    power_levels = power_levels
  )

  sim_data <- core_results$sim_data
  samples <- core_results$samples

  sample_sizes <- rep(seq(start, stop, by = increase), each = nsim)
  sample_sizes <- sample_sizes[1:length(samples)]

  split_half_rel <- data.frame(
    sample_size = sample_sizes,
    reliability = map_dbl(samples, function(sample_dat) {
      split_half_item_rel(
        dat = sample_dat,
        item_col = "item",
        score_col = "score"
      )
    })
  )
  cat("  split-half reliability done\n")

  if ("pos" %in% names(sim_data)) {
    split_half_by_pos <- split_half_by_subgroup_samples(
      samples = samples,
      sample_sizes = sample_sizes,
      item_col = "item",
      score_col = "score",
      group_col = "pos",
      min_items = 10
    )
    cat("  split-half by POS done\n")
  } else {
    warning("'pos' column not found in simulated data. Skipping POS split-half analysis.")
    split_half_by_pos <- NULL
  }

  if ("length_bucket" %in% names(sim_data)) {
    split_half_by_length <- split_half_by_subgroup_samples(
      samples = samples,
      sample_sizes = sample_sizes,
      item_col = "item",
      score_col = "score",
      group_col = "length_bucket",
      min_items = 10
    )
    cat("  split-half by length done\n")
  } else {
    warning("length_bucket column not found in simulated data. Skipping length split-half analysis.")
    split_half_by_length <- NULL
  }

  if ("pos" %in% names(df)) {
    pos_counts_check <- df %>%
      distinct(.data[[item_col]], pos) %>%
      group_by(pos) %>%
      summarise(n_items = n_distinct(.data[[item_col]]), .groups = "drop")
    valid_pos <- pos_counts_check %>% filter(n_items >= 10)

    pos_results <- list()
    for (pos in valid_pos$pos) {
      binned_pos <- pos
      cat(sprintf("  POS subgroup: %s\n", binned_pos))

      if (!(binned_pos %in% names(pos_results))) {
        df_pos <- sim_data %>%
          filter(.data[["pos"]] == .env$binned_pos)

        if (nrow(df_pos) > 0) {
          pos_pipeline <- run_population_pipeline(
            population = df_pos,
            min_score = min_score,
            max_score = max_score,
            n_per_item = n_per_item,
            start = start,
            stop = stop,
            increase = increase,
            nsim = nsim,
            power_levels = power_levels
          )

          pos_results[[binned_pos]] <- pos_pipeline
          cat(sprintf("  POS subgroup done: %s\n", binned_pos))
        }
      }
    }
  } else {
    warning("'pos' column not found in data. Skipping POS subgroup analysis.")
    pos_results <- NULL
  }

  if ("length_bucket" %in% names(df)) {
    length_counts_check <- df %>%
      filter(!is.na(.data[["length_bucket"]])) %>%
      distinct(.data[[item_col]], length_bucket) %>%
      group_by(length_bucket) %>%
      summarise(n_items = n_distinct(.data[[item_col]]), .groups = "drop")
    valid_length <- length_counts_check %>% filter(n_items >= 10)

    length_results <- list()
    for (len in valid_length$length_bucket) {
      binned_len <- as.character(len)
      cat(sprintf("  length subgroup: %s\n", binned_len))

      if (!(binned_len %in% names(length_results))) {
        df_len <- sim_data %>%
          filter(.data[["length_bucket"]] == .env$len)

        if (nrow(df_len) > 0) {
          length_pipeline <- run_population_pipeline(
            population = df_len,
            min_score = min_score,
            max_score = max_score,
            n_per_item = n_per_item,
            start = start,
            stop = stop,
            increase = increase,
            nsim = nsim,
            power_levels = power_levels
          )

          length_results[[binned_len]] <- length_pipeline
          cat(sprintf("  length subgroup done: %s\n", binned_len))
        }
      }
    }
  } else {
    warning("length_bucket column not found in data. Skipping length subgroup analysis.")
    length_results <- NULL
  }

  if ("stroke_bucket" %in% names(sim_data)) {
    split_half_by_stroke <- split_half_by_subgroup_samples(
      samples = samples,
      sample_sizes = sample_sizes,
      item_col = "item",
      score_col = "score",
      group_col = "stroke_bucket",
      min_items = 10
    )
    cat("  split-half by stroke done\n")
  } else {
    warning("stroke_bucket column not found in simulated data. Skipping stroke split-half analysis.")
    split_half_by_stroke <- NULL
  }

  if ("stroke_bucket" %in% names(df)) {
    stroke_counts_check <- df %>%
      filter(!is.na(.data[["stroke_bucket"]])) %>%
      distinct(.data[[item_col]], stroke_bucket) %>%
      group_by(stroke_bucket) %>%
      summarise(n_items = n_distinct(.data[[item_col]]), .groups = "drop")
    valid_stroke <- stroke_counts_check %>% filter(n_items >= 10)

    stroke_results <- list()
    for (stroke in valid_stroke$stroke_bucket) {
      binned_stroke <- as.character(stroke)
      cat(sprintf("  stroke subgroup: %s\n", binned_stroke))

      if (!(binned_stroke %in% names(stroke_results))) {
        df_stroke <- sim_data %>%
          filter(.data[["stroke_bucket"]] == .env$stroke)

        if (nrow(df_stroke) > 0) {
          stroke_pipeline <- run_population_pipeline(
            population = df_stroke,
            min_score = min_score,
            max_score = max_score,
            n_per_item = n_per_item,
            start = start,
            stop = stop,
            increase = increase,
            nsim = nsim,
            power_levels = power_levels
          )

          stroke_results[[binned_stroke]] <- stroke_pipeline
          cat(sprintf("  stroke subgroup done: %s\n", binned_stroke))
        }
      }
    }
  } else {
    warning("stroke_bucket column not found in data. Skipping stroke subgroup analysis.")
    stroke_results <- NULL
  }

  cat("Pipeline done\n")

  return(list(
    sim_data = sim_data,
    cutoff = core_results$cutoff,
    samples = samples,
    proportion_summary = core_results$proportion_summary,
    corrected_summary = core_results$corrected_summary,
    split_half_rel = split_half_rel,
    split_half_by_pos = split_half_by_pos,
    split_half_by_length = split_half_by_length,
    split_half_by_stroke = split_half_by_stroke,
    pos_pipelines = pos_results,
    length_pipelines = length_results,
    stroke_pipelines = stroke_results
  ))
}

split_half_item_rel <- function(dat, item_col = "item", score_col = "score") {
  dat <- dat %>%
    group_by(.data[[item_col]]) %>%
    mutate(id = row_number()) %>%
    ungroup()

  ids <- unique(dat$id)
  half1_ids <- sample(ids, length(ids) / 2)

  half1 <- dat %>%
    filter(id %in% half1_ids) %>%
    group_by(.data[[item_col]]) %>%
    summarise(mean1 = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop")

  half2 <- dat %>%
    filter(!id %in% half1_ids) %>%
    group_by(.data[[item_col]]) %>%
    summarise(mean2 = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop")

  merged <- inner_join(half1, half2, by = item_col)
  r <- cor(merged$mean1, merged$mean2, use = "complete.obs")
  (2 * r) / (1 + r)
}

split_half_by_subgroup_samples <- function(samples,
                                           sample_sizes,
                                           item_col = "item",
                                           score_col = "score",
                                           group_col = "pos",
                                           min_items = 10) {
  sample_sizes <- sample_sizes[seq_along(samples)]
  results <- tibble()

  for (i in seq_along(samples)) {
    sample_dat <- samples[[i]]
    sample_size <- sample_sizes[i]

    if (!group_col %in% names(sample_dat)) {
      warning(sprintf("'%s' column not found in sample %d. Skipping subgroup split-half.", group_col, sample_size))
      next
    }

    group_counts <- sample_dat %>%
      distinct(.data[[item_col]], .data[[group_col]]) %>%
      group_by(.data[[group_col]]) %>%
      summarise(n_items = n(), .groups = "drop")
    valid_groups <- group_counts %>%
      filter(n_items >= min_items)

    if (nrow(valid_groups) == 0) {
      warning(sprintf("Skipping subgroup split-half for sample size %s because no %s buckets meet the %d-item minimum.", sample_size, group_col, min_items))
      next
    }

    for (grp in valid_groups[[group_col]]) {
      grp_data <- sample_dat %>% filter(.data[[group_col]] == .env$grp)
      if (n_distinct(grp_data[[item_col]]) > 1) {
        rel <- split_half_item_rel(grp_data, item_col, score_col)

        results <- bind_rows(results, tibble(
          sample_size = sample_size,
          group = as.character(grp),
          n_items = n_distinct(grp_data[[item_col]]),
          n_observations = nrow(grp_data),
          reliability = rel
        ))
      }
    }
  }

  results
}

split_half_by_subgroup <- function(dat,
                                   item_col = "item",
                                   score_col = "score",
                                   group_col = "pos",
                                   min_items = 8) {
  group_counts <- dat %>%
    distinct(.data[[item_col]], .data[[group_col]]) %>%
    group_by(.data[[group_col]]) %>%
    summarise(n_items = n(), .groups = "drop")

  results <- tibble()
  for (grp in unique(group_counts[[group_col]])) {
    grp_n <- group_counts$n_items[group_counts[[group_col]] == grp][1]
    if (is.na(grp_n) || grp_n < min_items) {
      warning(
        sprintf(
          "Skipping %s bucket '%s' because it has %d items (< %d).",
          group_col,
          as.character(grp),
          grp_n,
          min_items
        )
      )
      next
    }

    grp_data <- dat %>% filter(.data[[group_col]] == .env$grp)

    if (n_distinct(grp_data[[item_col]]) > 1) {
      rel <- split_half_item_rel(grp_data, item_col, score_col)

      results <- bind_rows(results, tibble(
        group = as.character(grp),
        n_items = n_distinct(grp_data[[item_col]]),
        n_observations = nrow(grp_data),
        reliability = rel
      ))
    }
  }

  results
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

tag_df_with_pos_length <- function(df, item_col, udpipe_models, stroke_tagger = NULL) {
  lang <- sub("^word_", "", item_col)
  lang <- gsub("_", "-", lang)
  lang <- gsub("-name$", "", lang)

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
    message(sprintf("No udpipe model for '%s' - POS tagging skipped", lang))
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
