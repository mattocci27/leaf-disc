library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)

source("R/util.R")
source("R/data_clean.R")
source("R/sp_analysis.R")
source("R/ind_analysis.R")
source("R/fig_write.R")
source("R/yml.R")
source("R/stan.R")

options(clustermq.scheduler = "multiprocess")

tar_option_set(packages = c(
  "tidyverse",
  "patchwork",
  "ggsma",
  "ggpubr",
  "smatr",
  "parallel",
  "janitor",
  "extrafont",
  "loo",
  "modelr",
  "performance"
))

# check if it's inside a container
if (file.exists("/.dockerenv") | file.exists("/.singularity.d/startscript")) {
  Sys.setenv(CMDSTAN = "/opt/cmdstan/cmdstan-2.29.0")
  set_cmdstan_path("/opt/cmdstan/cmdstan-2.29.0")
}

cmdstan_version()

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
    stan_sim_dat,
    create_dummy_data(100)
  ),
  tar_target(
    stan_sp_dat,
    clean_stan_data(sp_mean, dry_mass = FALSE)
  ),
  # tar_target(
  #   stan_sp_dat_noint,
  #   clean_stan_data(sp_mean, interaction = FALSE)
  # ),
  tar_stan_mcmc(
    fit_sim,
    "stan/model.stan",
    data = stan_sim_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1000,
    iter_sampling = 1000,
    seed = 123
   ),
  tar_stan_mcmc(
    fit_tree_1,
    "stan/model.stan",
    data = clean_stan_data(tree,
      dry_mass = FALSE, scale = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1000,
    iter_sampling = 1000,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_tree_2,
    "stan/sma.stan",
    data = clean_stan_data(tree,
      dry_mass = FALSE, scale = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 4000,
    iter_sampling = 4000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_tree_3,
    "stan/sma.stan",
    data = clean_stan_data(tree,
      dry_mass = FALSE, scale = TRUE, ld = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 4000,
    iter_sampling = 4000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_tree_4,
    "stan/sma.stan",
    data = clean_stan_data(tree,
      dry_mass = FALSE, scale = FALSE, ld = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123
   ),
  # tar_stan_mcmc(
  #   fit_tree_3,
  #   "stan/punch.stan",
  #   data = clean_stan_data(tree,
  #     dry_mass = TRUE, scale = TRUE),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 1,
  #   iter_sampling = 1,
  #   seed = 123
  #  ),
  tar_stan_mcmc(
    fit_sp_1,
    "stan/model.stan",
    data = clean_stan_data(sp_mean,
      dry_mass = FALSE, scale = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1000,
    iter_sampling = 1000,
    seed = 123
   ),
  tar_stan_mcmc(
    fit_sp_2,
    "stan/sma.stan",
    data = clean_stan_data(sp_mean,
      dry_mass = FALSE, scale = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1000,
    iter_sampling = 1000,
    seed = 123
   ),

  # coef for OLS stan
  tar_target(
    coef_tree_tab,
    create_stan_tab(fit_tree_1_draws_model)
  ),
  tar_target(
    coef_tree_plot,
    coef_pointrange(coef_tree_tab)
  ),
  tar_target(
    coef_tree_png,
    ggsave(
      "figs/coef_tree.png",
      coef_tree_plot,
      dpi = 300,
      width = 6,
      height = 6
    ),
    format = "file"
  ),
  tar_target(
    coef_tree_pdf,
    ggsave(
      "figs/coef_tree.pdf",
      coef_tree_plot,
      device = cairo_pdf,
      width = 6,
      height = 6
    ),
    format = "file"
  ),

  # coef for SMA stan
  tar_target(
    coef_tree_sma_tab,
    create_stan_tab(fit_tree_2_draws_sma)
  ),
  tar_target(
    coef_tree_sma_plot,
    coef_pointrange(coef_tree_sma_tab)
  ),
  tar_target(
    coef_tree_sma_png,
    ggsave(
      "figs/coef_tree_sma.png",
      coef_tree_sma_plot,
      dpi = 300,
      width = 6,
      height = 6
    ),
    format = "file"
  ),
  tar_target(
    coef_tree_sma_pdf,
    ggsave(
      "figs/coef_tree_sma.pdf",
      coef_tree_sma_plot,
      device = cairo_pdf,
      width = 6,
      height = 6
    ),
    format = "file"
  ),
  tar_target(
    coef_tree_ld_sma_tab,
    create_stan_tab(fit_tree_3_draws_sma)
  ),
  tar_target(
    coef_tree_ld_sma_plot,
    coef_pointrange(coef_tree_ld_sma_tab, ld = TRUE)
  ),
  tar_target(
    coef_tree_ld_sma_png,
    ggsave(
      "figs/coef_tree_ld_sma.png",
      coef_tree_ld_sma_plot,
      dpi = 300,
      width = 6,
      height = 6
    ),
    format = "file"
  ),
  # tar_target(
  #   coef_tree_sma_pdf,
  #   ggsave(
  #     "figs/coef_tree_sma.pdf",
  #     coef_tree_sma_plot,
  #     device = cairo_pdf,
  #     width = 6,
  #     height = 6
  #   ),
  #   format = "file"
  # ),
  tar_target(
    coef_tree_ld_ori_sma_tab,
    create_stan_tab(fit_tree_4_draws_sma)
  ),
  tar_target(
    coef_tree_ld_ori_sma_plot,
    coef_pointrange(coef_tree_ld_ori_sma_tab, ld = TRUE)
  ),
  tar_target(
    coef_tree_ld_ori_sma_png,
    ggsave(
      "figs/coef_tree_ld_ori_sma.png",
      coef_tree_ld_ori_sma_plot,
      dpi = 300,
      width = 6,
      height = 6
    ),
    format = "file"
  ),

  tar_target(
    cv_tree,
    create_cv_fit(tree, k = 10, seed = 123)
  ),
  tar_target(
    cv_sp,
    create_cv_fit(sp_mean, k = 10, seed = 123)
  ),

  # coef for SMA stan for sp
  tar_target(
    coef_sp_sma_tab,
    create_stan_tab(fit_sp_2_draws_sma)
  ),
  tar_target(
    coef_sp_sma_plot,
    coef_pointrange(coef_sp_sma_tab)
  ),
  # tar_stan_mcmc(
  # tar_stan_mcmc(
  #   fit_sp_1,
  #   "stan/model.stan",
  #   data = stan_sp_dat_noint,
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   seed = 123
  #  ),
  # tar_stan_mcmc(
  #   fit_sp_2,
  #   "stan/model.stan",
  #   data = stan_sp_dat,
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   seed = 123
  #  ),

  tar_target(
    loo_,
    mclapply(
      list(
        fit_tree_1 = fit_tree_1_mcmc_model,
        fit_tree_2 = fit_tree_2_mcmc_sma,
        fit_tree_3 = fit_tree_3_mcmc_sma,
        fit_tree_4 = fit_tree_4_mcmc_sma
        ),
    \(x)x$loo(cores = parallel::detectCores())
    )
  ),

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
    sma_sp_tab,
    generate_sma_tab(sp_mean)
  ),

  tar_target(
    sma_tree_tab,
    generate_sma_tab(tree)
  ),

  tar_target(
    sma_tree_ld_tab,
    generate_sma_ld_tab(tree)
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
    lalt_tree_grid_plot_dense,
    lalt_tree_grid_dense(tree)
  ),
  tar_target(
    lalt_tree_grid_dense_png,
    ggsave(
      "figs/lalt_tree_grid_dense.png",
      lalt_tree_grid_plot_dense,
      dpi = 300,
      width = 12,
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
