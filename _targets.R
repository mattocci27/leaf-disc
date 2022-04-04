library(targets)
library(tarchetypes)
library(furrr)

source("R/util.R")
source("R/data_clean.R")
source("R/sp_analysis.R")

options(clustermq.scheduler = "multiprocess")
plan(multisession, workers = parallel::detectCores())
tar_option_set(packages = c(
  "tidyverse",
  "patchwork",
  "ggsma",
  "ggpubr",
  "smatr",
  "janitor"
))



list(
  # data cleaning ----------------------------------
  tar_target(
    lma_raw,
    "data-raw/lma_raw.csv",
    format = "file"
  ),
  tar_target( 
    lma_la,
    "data-raw/lma_la.csv",
    format = "file"
  ),
  tar_target(
    lma_yaku,
    "data-raw/leaf_traits_for_katabuchi.csv",
    format = "file"
  ),
  tar_target(
    lma_raw_re,
    clean_lma_raw(lma_raw)
  ),
  tar_target(
    lma_la_re,
    clean_lma_la(lma_la)
  ),
  tar_target(
    lma_yaku_re,
    clean_lma_yaku(lma_yaku)
  ),
  tar_target(
    full_data_csv,
    data_clean(lma_raw_re, lma_la_re, lma_yaku_re)
  ),
  tar_target(
    full_data_cv_csv,
    data_clean_cv(lma_raw_re, lma_yaku_re)
  ),
  tar_target(
    tree_data_csv,
    data_clean_tree(lma_raw_re, lma_yaku_re)
  ),
  tar_target(
    sp_mean,
    sp_dat_mean(full_data_csv)
  ),
  tar_target(
    sp_cv,
    sp_dat_cv(full_data_cv_csv)
  ),
  tar_target(
    tree_dat,
    tree_dat_clean(tree_data_csv)
  ),
  tar_target(
    la_lt_grid_plot,
    la_lt_grid(sp_mean)
  )
)
