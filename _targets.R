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
  "jsonlite",
  "lmtest",
  "modelr"
))

# check if it's inside a container
if (file.exists("/.dockerenv")) {
  Sys.setenv(CMDSTAN = "/opt/cmdstan/cmdstan-2.29.2")
  set_cmdstan_path("/opt/cmdstan/cmdstan-2.29.2")
}

cmdstan_version()

list(
  # data cleaning ----------------------------------
  tar_target(
    lma_yaku,
    "data-raw/yakushima_raw.csv",
    format = "file"
  ),
  tar_target(
    lma_raw,
    "data-raw/yunnan_lma.csv",
    format = "file"
  ),
  tar_target(
    lma_la,
    "data-raw/yunnan_la.csv",
    format = "file"
  ),
  # tar_target(
  #   lma_yaku,
  #   "data-raw/leaf_traits_for_katabuchi.csv",
  #   format = "file"
  # ),
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
    lma_yaku_re,
    clean_lma_yaku(lma_yaku)
  ),
  tar_target(
    tree_data_csv,
    data_clean_tree(lma_raw_re, lma_yaku_re),
    format = "file"
  ),
  tar_target(
    tree,
    read_csv(tree_data_csv)
  ),

  tar_target(
    sp_mean_file,
    data_clean_mean(tree),
    format = "file"
  ),
  tar_target(
    sp_mean,
    read_csv(sp_mean_file)
  ),

  tar_target(
    sp_cv_file,
    data_clean_cv(tree),
    format = "file"
  ),
  tar_target(
    sp_cv,
    read_csv(sp_cv_file)
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
      sp_mean, sp_cv, tree, lma_raw_re, lma_yaku_re, boot_fit_dat),
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
  #  tar_stan_mcmc(
  #   fit_sp_punch_ori,
  #   "stan/model.stan",
  #   data = clean_stan_data(sp_mean, model = "punch", scale = FALSE),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   # adapt_delta = 0.99,
  #   # max_treedepth = 15,
  #   seed = 123
  #),
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
  #  tar_stan_mcmc(
  #   fit_sp_ld2,
  #   "stan/model2.stan",
  #   data = clean_stan_data(sp_mean, model = "LD2"),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   # adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),
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


  tar_target(
    coef_sp_tab_punch3,
    create_stan_tab(fit_sp_punch3_draws_model2)
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


  tar_target(
    pred_mcmc_plot,  {
      p <- pred_mcmc(fit_sp_punch_draws_model, sp_mean)
      ggsave(
        "figs/pred_mcmc.png",
        p,
        dpi = 300,
        width = 7.25,
        height = 2.5
      )
      ggsave(
        "figs/pred_mcmc.pdf",
        p,
        width = 7.25,
        height = 2.5
      )
      paste0("figs/pred_mcmc", c(".png", ".pdf"))
    },
    format = "file"
  ),


  tar_target(
    coef_sp_punch1_plot, {
      p <- coef_pointrange(coef_sp_tab_punch1, add = FALSE)
      ggsave(
        "figs/coef_sp_punch1.png",
        p,
        dpi = 300,
        width = 5,
        height = 6)
      ggsave(
        "figs/coef_sp_punch1.pdf",
        p,
        width = 5,
        height = 6)
      paste0("figs/coef_sp_punch1", c(".png", ".pdf"))
    },
    format = "file"
  ),

  tar_target(
    coef_sp_punch1_add_plot, {
      p <- coef_pointrange(coef_sp_tab_punch1_add, add = TRUE)
      ggsave(
        "figs/coef_sp_punch1_add.png",
        p,
        dpi = 300,
        width = 5,
        height = 6)
      ggsave(
        "figs/coef_sp_punch1_add.pdf",
        p,
        width = 5,
        height = 6)
      paste0("figs/coef_sp_punch1_add", c(".png", ".pdf"))
    },
    format = "file"
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
        fit_sp_ld3_mcmc_model = fit_sp_ld3_mcmc_model,
        fit_sp_lm_mcmc_model = fit_sp_lm_mcmc_model,
        fit_sp_lma0_mcmc_simple = fit_sp_lma0_mcmc_simple,
        fit_sp_punch_mcmc_model = fit_sp_punch_mcmc_model,
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
      ggsave(
        "figs/sma.pdf",
        p,
        width = 3.5,
        height = 3.5
      )
      paste0("figs/sma", c(".png", ".pdf"))
    },
    format = "file"
  ),
  tar_target(
    sma_sep_plot, {
      p <- sma_point_sep(sp_mean)
      ggsave(
        "figs/sma_sep.png",
        p,
        dpi = 300,
        width = 3.5,
        height = 3.5
      )
      ggsave(
        "figs/sma_sep.pdf",
        p,
        width = 3.5,
        height = 3.5
      )
      paste0("figs/sma_sep", c(".png", ".pdf"))
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
        width = 3.5,
        height = 3.5
      )
      ggsave(
        "figs/ratio_dm.pdf",
        p,
        width = 3.5,
        height = 3.5
      )
      paste0("figs/ratio_dm", c(".png", ".pdf"))
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
        width = 6,
        height = 3
      )
      ggsave(
        "figs/lma_ld.pdf",
        p,
        width = 6,
        height = 3
      )
      paste0("figs/lma_ld", c(".png", ".pdf"))
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
        width = 3.5,
        height = 3.5
      )
      ggsave(
        "figs/cv_pool.pdf",
        p,
        width = 3.5,
        height = 3.5
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
        width = 3,
        height = 3
      )
      paste0("figs/cv_pool_rm", c(".png", ".pdf"))
    },
    format = "file"
  ),

  tar_target(
    petiole_plot, {
      p <- petiole_point(yaku_sp)
      ggsave(
        "figs/petiole.png",
        p,
        dpi = 300,
        width = 6,
        height = 3
      )
      ggsave(
        "figs/petiole.pdf",
        p,
        width = 6,
        height = 3)
        paste0("figs/petiole", c(".png", ".pdf"))
    },
    format = "file"
  ),

  tar_target(
    dm_glmm_tab,
    dm_glmm(tree),
    format = "file"
  ),
  tar_render(
    report,
    "report.Rmd"
   )
  # tar_render(
  #   si,
  #   "ms/SI.Rmd",
  #   output_format = "pdf_document"
  #  )
)
