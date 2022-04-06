library(targets)
library(tarchetypes)
library(furrr)
library(tidyverse)
library(parallel)

source("R/util.R")
source("R/data_clean.R")
source("R/sp_analysis.R")
source("R/ind_analysis.R")
source("R/fig_write.R")

options(clustermq.scheduler = "multiprocess")
options(mc.cores = detectCores()) # 4
plan(multisession, workers = parallel::detectCores())
tar_option_set(packages = c(
  "tidyverse",
  "patchwork",
  "ggsma",
  "ggpubr",
  "smatr",
  "janitor",
  "extrafont",
  "parallel"
))

theme_set(theme_bw())
theme_update(text = element_text(family = "Arial"))

#
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
    tree,
    tree_dat_clean(tree_data_csv)
  ),
  tar_target(
    yaku_sp,
    create_yaku_sp(lma_yaku_re)
  ),

  # analyses and figs ---------------------------------------------

  tar_target(
    lalt_pool_grid_plot,
    lalt_pool_grid_point(sp_mean)
  ),

  tar_target(
    lalt_pool_grid_png,
    ggsave(
      "figs/lalt_pool_grid.png",
      lalt_pool_grid_plot,
      dpi = 300,
      width = 6,
      height = 6
    )
  ),
  tar_target(
    lalt_pool_grid_pdf,
    ggsave(
      "figs/lalt_pool_grid.pdf",
      lalt_pool_grid_plot,
      width = 6,
      height = 6
    )
  ),

  tar_target(
    lalt_sep_grid_plot,
    lalt_sep_grid_point(sp_mean)
  ),
  tar_target(
    lalt_sep_grid_png,
    ggsave(
      "figs/lalt_sep_grid.png",
      lalt_sep_grid_plot,
      dpi = 300,
      width = 6,
      height = 6
    )
  ),
  tar_target(
    lalt_sep_grid_pdf,
    ggsave(
      "figs/lalt_sep_grid.pdf",
      lalt_sep_grid_plot,
      width = 6,
      height = 6
    )
  ),

  tar_target(
    sma_tab,
    generate_sma_tab(sp_mean)
  ),

  tar_target(
    lalt_tree_grid_plot,
    lalt_tree_grid(tree)
  ),

  tar_target(
    ratio_plot,
    ratio_combine(tree)
  ),

  tar_target(
    lma_ld_plot,
    lma_ld_wrap_point(sp_mean)
  ),

  tar_target(
    cv_pool_plot,
    cv_pool_point(sp_cv)
  ),

  tar_target(
    cv_sep_plot,
    cv_sep_point(sp_cv)
  ),
  tar_target(
    petiole_plot,
    petiole_point(yaku_sp)
  ),

  # tar_target(
  #   test,
  #   list(
  #     lalt_pool_grid_plot,
  #     lalt_sep_grid_plot),
  #   pattern = map(
  #     lalt_pool_grid_plot,
  #     lalt_sep_grid_plot)
  # ),

  # tar_target(
  #   plot_dat,
  #   create_plot_dat(
  #     list(
  #       lalt_pool_grid_plot = lalt_pool_grid_plot,
  #       lalt_sep_grid_plot = lalt_sep_grid_plot,
  #       lalt_tree_grid_plot = lalt_tree_grid_plot,
  #       ratio_plot = ratio_plot,
  #       lma_ld_plot = lma_ld_plot,
  #       cv_pool_plot = cv_pool_plot,
  #       cv_sep_plot = cv_sep_plot,
  #       petiole_plot = petiole_plot
  #     )) |>
  #      mutate(width = c(6, 6, 6, 9, 6, 3, 6, 8)) |>
  #      mutate(height = c(6, 6, 6, 3, 3, 3, 3, 5))
  # ),

  # tar_target(
  #   test,
  #   mcmapply(function(filename, plot, width, height)ggsave(filename = filename, plot = plot, width = width, height = height),
  #     list(
  #       "figs/lalt_pool_grid.tiff",
  #       "figs/lalt_sep_grid.tiff",
  #       "figs/lalt_tree_grid.tiff",
  #       "figs/ratio.tiff",
  #       "figs/lma_ld.tiff",
  #       "figs/cv_pool.tiff",
  #       "figs/cv_sep.tiff",
  #       "figs/petiole.tiff"),
  #     list(
  #       lalt_pool_grid_plot,
  #       lalt_sep_grid_plot,
  #       lalt_tree_grid_plot,
  #       ratio_plot,
  #       lma_ld_plot,
  #       cv_pool_plot,
  #       cv_sep_plot,
  #       petiole_plot),
  #     list(6, 6, 6, 9, 6, 3, 6, 8),
  #     list(6, 6, 6, 6, 3, 3, 3, 5)
  # )),


  tar_render(
    report,
    "report.Rmd"
  )
)
