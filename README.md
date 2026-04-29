## Analytic Methods

### Overview

We applied the accuracy in parameter estimation (AIPE) and Monte Carlo simulation procedure described by [Buchanan et al. (2026)](https://link.springer.com/article/10.3758/s13428-025-02860-7) to estimate sample sizes for a set of published psycholinguistic norming datasets. The procedure uses pilot or existing norming data to simulate the number of participants needed for items to be considered "well measured" — that is, for their standard errors (SEs) to fall below a data-driven criterion. All analyses were carried out in R using the `semanticprimeR` package.

------------------------------------------------------------------------

### Step 1: Simulating from Known Population Norms

Because individual trial-level data were not available for all datasets, we used each dataset's reported item-level means and standard deviations to simulate representative pilot data. For each dataset, we drew `n_per_item` observations per item from a truncated normal distribution (via `simulate_population()`), bounded by the minimum and maximum possible scale values for that measure. This approach, described in Buchanan et al., allows the procedure to be applied to any norming study for which item means and SDs are available, even when raw trial-level data have not been archived.

A total of 43 datasets were processed (see `build_jobs()` in `run_estimate_r_ss_batch.R`), spanning norming studies published between 1975 and 2025 across 16 languages including English, French, Spanish, Italian, Dutch, German, Chinese, and others. Measures included familiarity ratings (as either familiarity or subjective frequency).

------------------------------------------------------------------------

### Step 2: Estimating Measurement Quality and Recommended Sample Sizes

For each dataset, we ran the full AIPE simulation pipeline (`run_simulation_pipeline()`). Beginning at a minimum of 20 simulated participants per item and increasing in steps of 5 up to 100, we drew 100 bootstrap samples with replacement at each step and calculated the SE for each item. We then determined the proportion of items whose SE fell below the 4th-decile cutoff — the threshold recommended by Buchanan et al. as the optimal balance between precision and feasibility.

Sample size recommendations were derived at four power-analogue thresholds: 80%, 85%, 90%, and 95% of items meeting the SE criterion. These values parallel conventional power levels and allow researchers to designate a minimum sample size (e.g., 80% of items well measured) and a maximum or stopping-rule sample size (e.g., 95% of items well measured). A correction factor was applied to account for the known upward bias introduced by small pilot samples, following the exponential decay correction formula described in Buchanan et al.

A **pilot run** (`run_estimate_r_ss_pilot.R`) was first conducted to identify the appropriate sample size search window for each dataset. The pilot used a broader simulation range (20 to 500 participants, in steps of 10) with 10 simulations per step and power levels of 70%, 75%, 80%, 85%, 90%, and 95%. The resulting power curves were used to determine the start and stop sample sizes for the batch run, targeting the window in which the proportion of well-measured items transitions from 70% to 95%. These windows were saved as a manifest file (`pilot_grid_manifest.csv`) for use in subsequent full-scale analysis.

------------------------------------------------------------------------

### Step 3: Reliability

In addition to precision-based sample size estimation, we calculated split-half reliability for each dataset following the recommendation in Buchanan et al. that reliability and precision be considered jointly. Reliability was estimated as the correlation between two randomly drawn halves of each dataset's participants (per item), corrected to full-test length using the Spearman-Brown formula. This provides an index of the consistency of item-level estimates across independent groups of raters, complementing the SE-based precision metric from Step 2.

------------------------------------------------------------------------

### Step 4: Part-of-Speech and Word Length Splits

To examine whether sample size recommendations varied systematically with lexical properties, we additionally split each dataset by **part of speech (POS)** and **word length** prior to running the simulation pipeline. POS tags were assigned automatically using language-specific UDPipe models (Universal Dependencies 2.5 pre-trained models; `udpipe` R package). Language-to-model mappings covered 16 languages; datasets for languages without an available model were processed without POS tagging. Word length was computed in characters for alphabetic scripts. Where applicable, Chinese items were additionally tagged by stroke count using the `strokes` Python package via `reticulate`. Subgroup-specific simulations were then conducted separately within each POS category and word-length bin, allowing us to assess whether items of different grammatical classes or orthographic lengths require different participant sample sizes to achieve the same level of measurement precision.

------------------------------------------------------------------------

### Software and Reproducibility

All analyses were conducted in R. Key packages included `semanticprimeR`, `udpipe`, `dplyr`, `purrr`, `tidyr`, `truncnorm`, `psych`, and `rio`. Data import supported both `.csv` and `.xlsx` formats. Jobs were parallelizable across shards via `SHARD_TOTAL` / `SHARD_INDEX` environment variables. Simulation outputs were saved as `.rds` files and logs were written to timestamped batch log files for reproducibility.
