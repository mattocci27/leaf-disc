library(targets)
library(tarchetypes)
library(tidyverse)

source("R/util.R")
source("R/data_clean.R")
source("R/sp_analysis.R")
source("R/ind_analysis.R")
source("R/fig_write.R")
source("R/yml.R")

options(clustermq.scheduler = "multiprocess")

tar_option_set(packages = c(
  "tidyverse",
  "patchwork",
  "ggsma",
  "ggpubr",
  "smatr",
  "janitor",
  "extrafont"
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
    tree,
    tree_dat_clean(tree_data_csv)
  ),
  tar_target(
    yaku_sp,
    create_yaku_sp(lma_yaku_re)
  ),
  tar_target(
    yml,
    write_yml("values.yml",
      sp_mean, full_data_cv_csv, tree, lma_yaku_re)
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
    ),
    format = "file"
  ),
  tar_target(
    lalt_pool_grid_pdf,
    ggsave(
      "figs/lalt_pool_grid.pdf",
      lalt_pool_grid_plot,
      device = cairo_pdf,
      width = 6,
      height = 6
    ),
    format = "file"
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
    ),
    format = "file"
  ),
  tar_target(
    lalt_sep_grid_pdf,
    ggsave(
      "figs/lalt_sep_grid.pdf",
      lalt_sep_grid_plot,
      device = cairo_pdf,
      width = 6,
      height = 6
    ),
    format = "file"
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
    lalt_tree_grid_png,
    ggsave(
      "figs/lalt_tree_grid.png",
      lalt_tree_grid_plot,
      dpi = 300,
      width = 6,
      height = 6
    ),
    format = "file"
  ),
  tar_target(
    lalt_tree_grid_pdf,
    ggsave(
      "figs/lalt_tree_grid.pdf",
      lalt_tree_grid_plot,
      device = cairo_pdf,
      width = 6,
      height = 6
    ),
    format = "file"
  ),

  tar_target(
    ratio_plot,
    ratio_combine(tree)
  ),
  tar_target(
    ratio_png,
    ggsave(
      "figs/ratio.png",
      ratio_plot,
      dpi = 300,
      width = 9,
      height = 3
    ),
    format = "file"
  ),
  tar_target(
    ratio_pdf,
    ggsave(
      "figs/ratio.pdf",
      ratio_plot,
      device = cairo_pdf,
      width = 9,
      height = 3
    ),
    format = "file"
  ),

  tar_target(
    lma_ld_plot,
    lma_ld_wrap_point(sp_mean)
  ),
  tar_target(
    lma_ld_png,
    ggsave(
      "figs/lma_ld.png",
      lma_ld_plot,
      dpi = 300,
      width = 6,
      height = 3
    ),
    format = "file"
  ),
  tar_target(
    lma_ld_pdf,
    ggsave(
      "figs/lma_ld.pdf",
      lma_ld_plot,
      device = cairo_pdf,
      width = 6,
      height = 3
    ),
    format = "file"
  ),

  tar_target(
    cv_pool_plot,
    cv_pool_point(sp_cv)
  ),
  tar_target(
    cv_pool_png,
    ggsave(
      "figs/cv_pool.png",
      cv_pool_plot,
      dpi = 300,
      width = 3,
      height = 3
    ),
    format = "file"
  ),
  tar_target(
    cv_pool_pdf,
    ggsave(
      "figs/cv_pool.pdf",
      cv_pool_plot,
      device = cairo_pdf,
      width = 3,
      height = 3
    ),
    format = "file"
  ),

  tar_target(
    cv_sep_plot,
    cv_sep_point(sp_cv)
  ),
  tar_target(
    cv_sep_png,
    ggsave(
      "figs/cv_sep.png",
      cv_sep_plot,
      dpi = 300,
      width = 6,
      height = 3
    ),
    format = "file"
  ),
  tar_target(
    cv_sep_pdf,
    ggsave(
      "figs/cv_sep.pdf",
      cv_sep_plot,
      device = cairo_pdf,
      width = 6,
      height = 3
    ),
    format = "file"
  ),

  tar_target(
    petiole_plot,
    petiole_point(yaku_sp)
  ),
  tar_target(
    petiole_png,
    ggsave(
      "figs/petiole.png",
      petiole_plot,
      dpi = 300,
      width = 8,
      height = 4
    ),
    format = "file"
  ),
  tar_target(
    petiole_pdf,
    ggsave(
      "figs/petiole.pdf",
      petiole_plot,
      device = cairo_pdf,
      width = 8,
      height = 4
    ),
    format = "file"
  ),

  tar_target(
    dm_lm_tab,
    dm_lm(tree)
  ),
  tar_render(
    report,
    "report.Rmd"
   ),
  tar_render(
    si,
    "ms/SI.Rmd",
    output_format = "html_document"
   )
)
