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
source("R/site_info.R")
source("R/predictdf.R")

options(clustermq.scheduler = "multicore")

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
  "performance",
  "jsonlite",
  "lmtest"
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
    site_info_raw,
    "data-raw/site_info_raw.json",
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
    data_clean(lma_raw_re, lma_la_re, lma_yaku_re),
    format = "file"
  ),
  tar_target(
    full_data,
    read_csv(full_data_csv)
  ),
  tar_target(
    full_data_cv_csv,
    data_clean_cv(lma_raw_re, lma_yaku_re),
    format = "file"
  ),
  tar_target(
    full_data_cv,
    read_csv(full_data_cv_csv)
  ),
  tar_target(
    tree_data_csv,
    data_clean_tree(lma_raw_re, lma_yaku_re),
    format = "file"
  ),
  tar_target(
    tree_data,
    read_csv(tree_data_csv)
  ),
  tar_target(
    sp_mean,
    sp_dat_mean(full_data)
  ),
  tar_target(
    sp_cv,
    sp_dat_cv(full_data_cv)
  ),
  tar_target(
    tree,
    tree_dat_clean(tree_data)
  ),
  tar_target(
    yaku_sp,
    create_yaku_sp(lma_yaku_re)
  ),

  tar_target(
    boot_fit_dat,
    boot_fit(sp_mean)
  ),

  tar_target(
    yml,
    write_yml("values.yml",
      sp_mean, full_data_cv, tree, lma_raw_re, lma_yaku_re, sp_cv, boot_fit_dat),
    format = "file"
  ),
  tar_target(
    site_info,
    update_site_info(site_info_raw, yml),
    format = "file"
  ),

  # analyses and figs ---------------------------------------------
  tar_target(
    stan_sim_dat,
    create_dummy_data(100)
  ),

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
    fit_sim_sma,
    "stan/test.stan",
    data = create_dummy_sma_data(300),
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
  #   fit_tree_1,
  #   "stan/model.stan",
  #   data = clean_stan_data(tree,
  #     dry_mass = FALSE, scale = TRUE),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 1000,
  #   iter_sampling = 1000,
  #   seed = 123
  #  ),
  #  tar_stan_mcmc(
  #   fit_tree_2,
  #   "stan/sma.stan",
  #   data = clean_stan_data(tree,
  #     dry_mass = FALSE, scale = TRUE),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 4000,
  #   iter_sampling = 4000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),
  #  tar_stan_mcmc(
  #   fit_tree_3,
  #   "stan/sma.stan",
  #   data = clean_stan_data(tree,
  #     dry_mass = FALSE, scale = TRUE, ld = TRUE),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 4000,
  #   iter_sampling = 4000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),
  #  tar_stan_mcmc(
  #   fit_tree_4,
  #   "stan/sma.stan",
  #   data = clean_stan_data(tree,
  #     dry_mass = FALSE, scale = FALSE, ld = TRUE),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),

   tar_stan_mcmc(
    fit_sp_no_lma,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "no"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),

   tar_stan_mcmc(
    fit_sp_no_lma_int,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "no", int = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),

   tar_stan_mcmc(
    fit_sp_lma,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "LMA"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_sp_lma2,
    "stan/model2.stan",
    data = clean_stan_data(sp_mean, model = "LMA"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),

   tar_stan_mcmc(
    fit_sp_ld,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "LD"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),

   tar_stan_mcmc(
    fit_sp_ld3,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "LD3"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_sp_punch,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "punch"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),
   # LD_leaf, LA, and LT (how whole-leaf traits affects?)
   tar_stan_mcmc(
    fit_sp_punch_ori,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "punch", scale = FALSE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),
   # LMA_disc, LA, and LT (just for predictions)
   tar_stan_mcmc(
    fit_sp_punch2,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "punch2"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_sp_punch3,
    "stan/model2.stan",
    data = clean_stan_data(sp_mean, model = "punch3"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_sp_punch4,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "punch4"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),

   tar_stan_mcmc(
    fit_sp_ld3_yaku,
    "stan/model.stan",
    data = clean_stan_data_sep(sp_mean, yaku = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_sp_ld3_noyaku,
    "stan/model.stan",
    data = clean_stan_data_sep(sp_mean, yaku = FALSE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),

   tar_stan_mcmc(
    fit_sp_ld2,
    "stan/model2.stan",
    data = clean_stan_data(sp_mean, model = "LD2"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123
   ),
   tar_stan_mcmc(
    fit_sp_lm,
    "stan/model.stan",
    data = clean_stan_data(sp_mean, model = "LM"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.99,
    # max_treedepth = 15,
    seed = 123
   ),


   tar_stan_mcmc(
    fit_sp_lma0,
    "stan/simple.stan",
    data = clean_stan_data(sp_mean, model = "LMA"),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.95,
    # max_treedepth = 15,
    seed = 123
   ),

   tar_stan_mcmc(
    fit_sp_cv,
    "stan/simple.stan",
    data = clean_stan_data_cv(sp_mean, sp_cv),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    # adapt_delta = 0.95,
    # max_treedepth = 15,
    seed = 123
   ),

  # sma works but error models do not work
  #  tar_stan_mcmc(
  #   fit_sp_4,
  #   "stan/sma.stan",
  #   data = clean_stan_data(sp_mean, model = "no"),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),

  #  tar_stan_mcmc(
  #   fit_sp_5,
  #   "stan/sma.stan",
  #   data = clean_stan_data(sp_mean, model = "LMA"),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),

  #  tar_stan_mcmc(
  #   fit_sp_6,
  #   "stan/sma_err.stan",
  #   data = clean_stan_data(sp_mean, model = "LD"),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),

  tar_target(
    coef_sp_tab_punch3,
    create_stan_tab(fit_sp_punch3_draws_model2)
  ),
  tar_target(
    coef_sp_tab_cv,
    create_stan_tab(fit_sp_cv_draws_simple)
  ),

  tar_target(
    coef_sp_tab_punch1,
    create_stan_tab(fit_sp_punch_draws_model)
  ),
  tar_target(
    coef_sp_tab_punch2,
    create_stan_tab(fit_sp_punch2_draws_model)
  ),

  tar_target(
    coef_sp_tab_punch4,
    create_stan_tab(fit_sp_punch4_draws_model)
  ),

  tar_target(
    coef_sp_tab_punch1_add,
    create_stan_tab_add(fit_sp_punch_draws_model)
  ),
  tar_target(
    coef_sp_tab_punch2_add,
    create_stan_tab_add(fit_sp_punch2_draws_model)
  ),

  # tar_target(
  #   coef_sp_tab_punch2_add,
  #   create_stan_tab_add_simple(fit_sp_punch2_draws_simple)
  # ),

  tar_target(
    pred_mcmc_plot,  {
      p <- pred_mcmc(fit_sp_punch_draws_model, sp_mean)
      ggsave(
        "figs/pred_mcmc.png",
        p,
        dpi = 300,
        width = 9,
        height = 3
      )
      paste0("figs/pred_mcmc", c(".png"))
    },
    format = "file"
  ),


  tar_target(
    coef_sp_punch1_plot, {
      p <- coef_pointrange3(coef_sp_tab_punch1)
      ggsave(
        "figs/coef_sp_punch1.png",
        p,
        dpi = 300,
        width = 5,
        height = 6)
      ggsave(
        "figs/coef_sp_punch1.pdf",
        p,
        device = cairo_pdf,
        width = 5,
        height = 6)
      paste0("figs/coef_sp_punch1", c(".png", ".pdf"))
    },
    format = "file"
  ),

  tar_target(
    coef_sp_punch1_add_plot, {
      p <- coef_pointrange4(coef_sp_tab_punch1_add)
      ggsave(
        "figs/coef_sp_punch1_add.png",
        p,
        dpi = 300,
        width = 5,
        height = 6)
      ggsave(
        "figs/coef_sp_punch1_add.pdf",
        p,
        device = cairo_pdf,
        width = 5,
        height = 6)
      paste0("figs/coef_sp_punch1_add", c(".png", ".pdf"))
    },
    format = "file"
  ),

  tar_target(
    coef_sp_punch2_plot, {
      p <- coef_pointrange3(coef_sp_tab_punch2)
      ggsave(
        "figs/coef_sp_punch2.png",
        p,
        dpi = 300,
        width = 5,
        height = 6)
      ggsave(
        "figs/coef_sp_punch2.pdf",
        p,
        device = cairo_pdf,
        width = 5,
        height = 6)
      paste0("figs/coef_sp_punch2", c(".png", ".pdf"))
    },
    format = "file"
  ),

  tar_target(
    coef_sp_punch2_add_plot, {
      p <- coef_pointrange4(coef_sp_tab_punch2_add)
      ggsave(
        "figs/coef_sp_punch2_add.png",
        p,
        dpi = 300,
        width = 5,
        height = 6)
      ggsave(
        "figs/coef_sp_punch2_add.pdf",
        p,
        device = cairo_pdf,
        width = 5,
        height = 6)
      paste0("figs/coef_sp_punch2_add", c(".png", ".pdf"))
    },
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


  tar_target(
    loo_model,
    mclapply(
      list(
        fit_sp_no_lma_mcmc_model = fit_sp_no_lma_mcmc_model,
        fit_sp_no_lma_int_mcmc_model = fit_sp_no_lma_int_mcmc_model,
        fit_sp_lma_mcmc_model = fit_sp_lma_mcmc_model,
        fit_sp_lma2_mcmc_model2 = fit_sp_lma2_mcmc_model2,
        fit_sp_ld_mcmc_model = fit_sp_ld_mcmc_model,
        fit_sp_ld2_mcmc_model2 = fit_sp_ld2_mcmc_model2,
        fit_sp_ld3_mcmc_model = fit_sp_ld3_mcmc_model,
        fit_sp_lm_mcmc_model = fit_sp_lm_mcmc_model,
        fit_sp_lma0_mcmc_simple = fit_sp_lma0_mcmc_simple,
        fit_sp_punch_mcmc_model = fit_sp_punch_mcmc_model,
        fit_sp_punch2_mcmc_model = fit_sp_punch2_mcmc_model,
        fit_sp_punch3_mcmc_model2 = fit_sp_punch3_mcmc_model2,
        fit_sp_punch4_mcmc_model = fit_sp_punch4_mcmc_model
        ),
    \(x)x$loo(cores = parallel::detectCores())
    )
  ),

  tar_target(
    sma_plot, {
      p <- sma_point(sp_mean)
      ggsave(
        "figs/sma.png",
        p,
        dpi = 300,
        width = 4,
        height = 4
      )
      paste0("figs/sma", c(".png"))
    },
    format = "file"
  ),


  tar_target(
    sma_grid_col3_plot, {
      p <- sma_grid_col3(sp_mean)
      ggsave(
        "figs/sma_grid_col3.png",
        p,
        dpi = 300,
        width = 6,
        height = 9.5
      )
      paste0("figs/sma_grid_col3", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    lalt_pool_grid_plot, {
      p <- lalt_pool_grid_point(sp_mean)
      ggsave(
        "figs/lalt_pool_grid.png",
        p,
        dpi = 300,
        width = 6,
        height = 6)
      paste0("figs/lalt_pool_grid", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    lalt_sep_grid_plot, {
      p <- lalt_sep_grid_point(sp_mean)
      ggsave(
        "figs/lalt_sep_grid.png",
        p,
        dpi = 300,
        width = 6,
        height = 6)
      paste0("figs/lalt_sep_grid", c(".png"))
    },
    format = "file"
  ),


  tar_target(
    lalt_tree_grid_plot, {
      p <- lalt_tree_grid(tree)
      ggsave(
        "figs/lalt_tree_grid.png",
        p,
        dpi = 300,
        width = 6,
        height = 6
      )
      paste0("figs/lalt_tree_grid", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    lalt_tree_grid_plot_dense, {
      p <- lalt_tree_grid_dense(tree)
      ggsave(
        "figs/lalt_tree_grid_dense.png",
        p,
        dpi = 300,
        width = 12,
        height = 6)
      paste0("figs/lalt_tree_grid_dense", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    lalt_sp_grid_plot_dense, {
      p <- lalt_sp_grid_dense(sp_mean)
      ggsave(
        "figs/lalt_sp_grid_dense.png",
        p,
        dpi = 300,
        width = 12,
        height = 6
      )
      paste0("figs/lalt_sp_grid_dense", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    ratio_plot, {
      p <- ratio_combine(tree)
      ggsave(
        "figs/ratio.png",
        p,
        dpi = 300,
        width = 9,
        height = 3
      )
      paste0("figs/ratio", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    ratio_dm_plot, {
      p <- ratio_dm(tree)
      ggsave(
        "figs/ratio_dm.png",
        p,
        dpi = 300,
        width = 3,
        height = 3
      )
      paste0("figs/ratio_dm", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    lma_ld_plot, {
      p <- lma_ld_wrap_point(sp_mean)
      ggsave(
        "figs/lma_ld.png",
        p,
        dpi = 300,
        width = 9,
        height = 3
      )
      paste0("figs/lma_ld", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    cv_pool_plot, {
      p <- cv_pool_point(sp_cv)
      ggsave(
        "figs/cv_pool.png",
        p,
        dpi = 300,
        width = 3,
        height = 3
      )
      ggsave(
        "figs/cv_pool.pdf",
        p,
        device = cairo_pdf,
        width = 3,
        height = 3
      )
      paste0("figs/cv_pool", c(".png", ".pdf"))
    },
    format = "file"
  ),
  tar_target(
    cv_pool_plot_rm, {
      p <- cv_pool_point(sp_cv, remove_outliers = TRUE)
      ggsave(
        "figs/cv_pool_rm.png",
        p,
        dpi = 300,
        width = 3,
        height = 3
      )
      ggsave(
        "figs/cv_pool_rm.pdf",
        p,
        device = cairo_pdf,
        width = 3,
        height = 3
      )
      paste0("figs/cv_pool_rm", c(".png", ".pdf"))
    },
    format = "file"
  ),

  # tar_target(
  #   cv_sep_plot, {
  #     p <- cv_sep_point(sp_cv)
  #     ggsave(
  #       "figs/cv_sep.png",
  #       p,
  #       dpi = 300,
  #       width = 6,
  #       height = 3
  #     )
  #     ggsave(
  #       "figs/cv_sep.pdf",
  #       p,
  #       device = cairo_pdf,
  #       width = 6,
  #       height = 3
  #     )
  #     paste0("figs/cv_sep", c(".png", ".pdf"))
  #   },
  #   format = "file"
  # ),

  tar_target(
    petiole_plot, {
      p <- petiole_point(yaku_sp)
      ggsave(
        "figs/petiole.png",
        p,
        dpi = 300,
        width = 8,
        height = 4
      )
      ggsave(
        "figs/petiole.pdf",
        p,
        device = cairo_pdf,
        width = 8,
        height = 4)
        paste0("figs/cv_pool", c(".png", ".pdf"))
    },
    format = "file"
  ),

  tar_target(
    dm_glmm_tab,
    dm_glmm(tree)
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
