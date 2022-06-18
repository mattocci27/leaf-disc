#' @title Create dummy data for OLS like analysis
create_dummy_data <- function(n) {
  xx1 <- rnorm(n)
  xx2 <- rnorm(n)
  xx3 <- rnorm(n)
  gamma <- c(-1.4, -0.5, 0.5)
  beta <- c(0, 0.5, 0.5)
  x <- cbind(rep(1, n), xx2, xx3)
  log_mu <- as.numeric(x %*% beta) + xx1
  log_sig_hat <- as.numeric(x %*% gamma)
  log_sig <- rnorm(n, log_sig_hat, 0.2)
  yy <- rnorm(n, xx1, exp(log_sig))
  list(
    N = n,
    K = ncol(x),
    log_y = yy,
    log_lma_disc = xx1,
    x = x)
}

#' @title Create dummy data for SMA like analysis
create_dummy_sma_data <- function(n, seed = 123) {
  set.seed(seed)
  beta1 <- 1
  beta2 <- 1.5
  x_true <- rnorm(n)
  sig_x <- 0.4
  sig <- 0.8
  x <- rnorm(n, x_true, sig_x)
  y <- rnorm(n, beta1 + beta2 * x_true, sig)

  list(
    N = n,
    y = y,
    x = x,
    x_true = x_true
    )
}

#' @title Create stan data from the observed data
clean_stan_data <- function(sp_mean, model = c("no", "LMA", "LD", "LD2"),
  int = FALSE, scale = TRUE) {
  if (model == "no") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      la = sp_mean$la,
      lt = sp_mean$lt
    )
  } else if (model == "LMA") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      lma_disc = sp_mean$lma_disc,
      la = sp_mean$la,
      lt = sp_mean$lt
    )
  } else if (model == "LD") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      lma_disc = sp_mean$ld_leaf,
      la = sp_mean$la,
      lt = sp_mean$lt
   )
  } else if (model == "LD3" | model == "punch" | model == "punch3" | model == "punch4") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      # lma_disc = sp_mean$lma_disc / sp_mean$lt,
      # lma_disc = sp_mean$lma_disc,
      lma_disc = sp_mean$ld_leaf,
      la = sp_mean$la,
      lt = sp_mean$lt
    )
  } else if (model == "punch2") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      # lma_disc = sp_mean$lma_disc / sp_mean$lt,
      lma_disc = sp_mean$lma_disc,
      #lma_disc = sp_mean$ld_leaf,
      la = sp_mean$la,
      lt = sp_mean$lt
    )
  } else if (model == "LD2") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      lma_disc = sp_mean$ld_disc,
      la = sp_mean$la,
      lt = sp_mean$lt,
      # int1 = sp_mean$ld_disc * sp_mean$la,
      # int2 = sp_mean$ld_disc * sp_mean$lt,
      int3 = sp_mean$la * sp_mean$lt
    )
  } else if (model == "LM") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      lma_disc = sp_mean$ld_leaf * sp_mean$lt,
      la = sp_mean$la,
      lt = sp_mean$lt
    )
  }
  x2 <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      lma_disc = sp_mean$ld_leaf,
      la = sp_mean$la,
      lt = sp_mean$lt
    )

  if (scale) {
    x2[, -1] <- apply(x2[, -1], 2, \(x)scale(log(x)))
    x[, -1] <- apply(x[, -1], 2, \(x)scale(log(x)))
  }

  if (model == "punch" | model == "punch2") {
    yaku <- ifelse(sp_mean$location == "Yakushima", 0, 1)
    #yaku <- ifelse(sp_mean$location == "Yakushima", 0.5^2 * pi, 0.3^2*pi)
    #yaku <- scale(yaku) |> as.numeric()
    x <- cbind(x,
    punch = yaku,
    int1 = x[,2] * yaku,
    int2 = x[,3] * yaku,
    int3 = x[,4] * yaku
    )
  } else if (model == "punch3") {
    yaku <- ifelse(sp_mean$location == "Yakushima", 0, 1)
    # yaku <- ifelse(sp_mean$location == "Yakushima", 0.5^2 * pi, 0.3^2*pi)
    # yaku <- scale(yaku) |> as.numeric()
    x <- cbind(x,
    punch = yaku
    )
    x2 <- cbind(x,
    int1 = x[,2] * yaku,
    int2 = x[,3] * yaku,
    int3 = x[,4] * yaku
    )
  } else if (model == "punch4") {
    yaku <- ifelse(sp_mean$location == "Yakushima", 0, 1)
    # yaku <- ifelse(sp_mean$location == "Yakushima", 0.5^2 * pi, 0.3^2*pi)
    # yaku <- scale(yaku) |> as.numeric()
    # remove densitt
    x <- x[,-2]
    x <- cbind(x,
    punch = yaku,
    int1 = x[,2] * yaku,
    int2 = x[,3] * yaku
    )
  }

  if (int) {
   x <- cbind(x, lalt = x[,2] * x[,3])
  }

  # use non-scaled value for LMA
  log_lma_disc <- sp_mean$lma_disc |> log()
  log_lma_leaf <- sp_mean$lma_leaf |> log()

  list(
    N = nrow(sp_mean),
    K = ncol(x),
    K2 = ncol(x2),
    J = unique(sp_mean$species) |> length(),
    sp = sp_mean$species |> as.factor() |> as.numeric(),
    log_y = log_lma_leaf,
    log_lma_disc = log_lma_disc,
    x = x,
    x2 = x2
    )
}

#' @title Create summary table for posteriors
create_stan_tab <- function(draws) {
  tmp <- draws |>
    janitor::clean_names() |>
    dplyr::select(contains(c("beta", "gamma")))
  mean_ <- apply(tmp, 2, mean)
  lwr2_5 <- apply(tmp, 2, \(x)(quantile(x, 0.025)))
  lwr5 <- apply(tmp, 2, \(x)(quantile(x, 0.05)))
  upr97_5 <- apply(tmp, 2, \(x)(quantile(x, 0.975)))
  upr95 <- apply(tmp, 2, \(x)(quantile(x, 0.9)))
  tibble(para = names(mean_), mean_, lwr2_5, lwr5, upr95, upr97_5)
}

#' @title Create summary table for posteriors (effects are added)
create_stan_tab_add <- function(draws) {
  tmp <- draws |>
    janitor::clean_names() |>
    dplyr::select(contains(c("beta", "gamma"))) |>
    mutate(beta_6 = beta_6 + beta_2) |>
    mutate(beta_7 = beta_7 + beta_3) |>
    mutate(beta_8 = beta_8 + beta_4) |>
    mutate(gamma_6 = gamma_6 + gamma_2) |>
    mutate(gamma_7 = gamma_7 + gamma_3) |>
    mutate(gamma_8 = gamma_8 + gamma_4)
  mean_ <- apply(tmp, 2, mean)
  lwr2_5 <- apply(tmp, 2, \(x)(quantile(x, 0.025)))
  lwr5 <- apply(tmp, 2, \(x)(quantile(x, 0.05)))
  upr97_5 <- apply(tmp, 2, \(x)(quantile(x, 0.975)))
  upr95 <- apply(tmp, 2, \(x)(quantile(x, 0.9)))
  tibble(para = names(mean_), mean_, lwr2_5, lwr5, upr95, upr97_5)
}

#' @title use mean estiamte
#' @param ld use leaf density (TRUE) or LMAd (FALSE)
#' @param add use oringal coef (FALSE) or added coef (TRUE)
coef_pointrange <- function(data, ld = TRUE, add = FALSE) {
  data <- data |>
    mutate(sig = ifelse(lwr2_5 * upr97_5 > 0, "sig", "ns")) |>
    mutate(ci_sig = case_when(
      lwr2_5 * upr97_5 > 0 ~ "sig95",
      lwr5 * upr95 > 0 ~ "sig90",
      TRUE ~ "ns"
      ))
  # beta (mean)
  data1 <- data |>
    filter(str_detect(para, "beta")) |>
    filter(para != "beta_1") |>
    mutate(para = factor(para, levels = rev(para)))
  # gamma (sd)
  data2 <- data |>
    filter(str_detect(para, "gamma")) |>
    filter(para != "gamma_1") |>
    mutate(para = factor(para, levels = rev(para)))

  # plot function without title and y-lab
  plot_fun <- function(data) {
    data |>
      ggplot(aes(y = para)) +
      geom_vline(xintercept = 0, lty  = 2, color = "grey60") +
      geom_linerange(
        aes(xmin = lwr2_5, xmax = upr97_5),
        color = "#3366FF") +
      geom_linerange(
        aes(xmin = lwr5, xmax = upr95),
        size = 1.5,
        color = "#3366FF") +
      geom_point(
        aes(x = mean_, fill = sig),
        color = "#3366FF",
        shape = 21,
        size = 3) +
      ylab("") +
      ggtitle("Effects on mean") +
      scale_fill_manual(
        values = c(
          "sig" = "#33CCFF",
          # "sig95" = "#33CCFF",
          # "sig90" = "grey",
          "ns" = "#FFFFFF"
        )) +
      xlab("Standardized coefficients")
  }

 beta_lab <- c(
      "beta_2" = expression(Leaf~tissue~density~(beta[1])),
      "beta_3" = expression(Leaf~area~(beta[2])),
      "beta_4" = expression(Leaf~thickness~(beta[3])),
      "beta_5" = expression(Small~punch~(beta[4])),
      "beta_6" = expression(Leaf~tissue~density%*%small~punch~(beta[5])),
      "beta_7" = expression(Leaf~area%*%small~punch~(beta[6])),
      "beta_8" = expression(Leaf~thickness%*%small~punch~(beta[7]))
      )

 gamma_lab <- c(
      "gamma_2" = expression(Leaf~tissue~density~(gamma[1])),
      "gamma_3" = expression(Leaf~area~(gamma[2])),
      "gamma_4" = expression(Leaf~thickness~(gamma[3])),
      "gamma_5" = expression(Small~punch~(gamma[4])),
      "gamma_6" = expression(Leaf~tissue~density%*%small~punch~(gamma[5])),
      "gamma_7" = expression(Leaf~area%*%small~punch~(gamma[6])),
      "gamma_8" = expression(Leaf~thickness%*%small~punch~(gamma[7]))
      )


  if (add) {
    beta_lab[5:7] <- c(
        "beta_6" = expression(paste("Leaf tissue density (small punch: ", beta[1]+beta[5], ")")),
        "beta_7" = expression(paste("Leaf area (small punch: ", beta[2]+beta[6], ")")),
        "beta_8" = expression(paste("Leaf thickness (small punch: ", beta[3]+beta[7], ")"))
    )
    gamma_lab[5:7] <- c(
        "gamma_6" = expression(paste("Leaf tissue density (small punch: ", gamma[1]+gamma[5], ")")),
        "gamma_7" = expression(paste("Leaf area (small punch: ", gamma[2]+gamma[6], ")")),
        "gamma_8" = expression(paste("Leaf thickness (small punch: ", gamma[3]+gamma[7], ")"))
    )
  }

  if (!ld) {
    beta_lab[1] <- expression(LMA~(beta[1]))
    beta_lab[5] <- expression(LMA%*%small~punch~(beta[5]))
    gamma_lab[1] <- expression(LMA~(gamma[1]))
    gamma_lab[5] <- expression(LMA%*%small~punch~(gamma[5]))
  }

  p1 <- plot_fun(data1) +
      ggtitle("Effects on mean") +
      scale_y_discrete(labels = beta_lab)
  p2 <- plot_fun(data2) +
      ggtitle("Effects on variance") +
      scale_y_discrete(labels = gamma_lab)

  p1 / p2 +
  plot_annotation(tag_levels = "a") &
  theme_bw() &
  theme(
    legend.position = "none",
    plot.tag = element_text(face = "bold"),
    text = element_text(family = "Arial"))
}


#' @title Prepare dataframe for pred_mcmc
pred_each <- function(draws,
                      beta_int, beta_slope = NULL,
                      gamma_int, gamma_slope = NULL,
                      trait,
                      punch = c("0.6-cm", "1.0-cm"),
                      n = 80) {
  x_lt <- seq(-2, 2, length = n)
  draws2 <- janitor::clean_names(draws)

  # e.g. beta_0 (number in MS) -> beta_1 (number in stan)
  update_para <- function(x) {
    tmp1 <- str_split_fixed(x, "_", 2)[, 1]
    tmp2 <- str_split_fixed(x, "_", 2)[, 2] |> as.numeric()
    paste(tmp1, tmp2 + 1, sep = "_")
  }

  beta_int2 <- update_para(beta_int)
  beta_slope2 <- update_para(beta_slope)
  gamma_int2 <- update_para(gamma_int)
  gamma_slope2 <- update_para(gamma_slope)

  mu_mat <- exp(rep(1, n) %*% t(apply(draws2[, beta_int2], 1, sum)))
  if (!is.null(beta_slope)) {
  mu_mat <- exp(rep(1, n) %*% t(apply(draws2[, beta_int2], 1, sum)) +
     x_lt %*% t(apply(draws2[, beta_slope2], 1, sum)))
  }

  sig_mat <- exp(rep(1, n) %*% t(apply(draws2[, gamma_int2], 1, sum)))
  if (!is.null(gamma_slope)) {
  sig_mat <- exp(rep(1, n) %*% t(apply(draws2[, gamma_int2], 1, sum)) +
     x_lt %*% t(apply(draws2[, gamma_slope2], 1, sum)))
  }

  mean_sig <- apply(sig_mat, 1, mean)
  mean_mu <- apply(mu_mat, 1, mean)

  pred_up <- mean_mu + mean_sig
  pred_lo <- mean_mu - mean_sig
  x_bar <- log(trait) |> mean()
  x_s <- log(trait) |> sd()
  tibble(pred = mean_mu, pred_up, pred_lo,
         x = exp(x_bar + x_s * x_lt), punch = punch)
}

#' @title Prepare dataframe for pred_mcmc (use 95% CI)
pred_each2 <- function(draws,
                      beta_int, beta_slope = NULL,
                      gamma_int, gamma_slope = NULL,
                      trait,
                      punch = c("0.6-cm", "1.0-cm"),
                      n = 80) {
  x_lt <- seq(-2, 2, length = n)
  draws2 <- janitor::clean_names(draws)

  update_para <- function(x) {
    tmp1 <- str_split_fixed(x, "_", 2)[, 1]
    tmp2 <- str_split_fixed(x, "_", 2)[, 2] |> as.numeric()
    paste(tmp1, tmp2 + 1, sep = "_")
  }

  beta_int2 <- update_para(beta_int)
  beta_slope2 <- update_para(beta_slope)
  gamma_int2 <- update_para(gamma_int)
  gamma_slope2 <- update_para(gamma_slope)

  mu_mat <- exp(rep(1, n) %*% t(apply(draws2[, beta_int2], 1, sum)))
  if (!is.null(beta_slope)) {
  mu_mat <- exp(rep(1, n) %*% t(apply(draws2[, beta_int2], 1, sum)) +
     x_lt %*% t(apply(draws2[, beta_slope2], 1, sum)))
  }

  sig_mat <- exp(rep(1, n) %*% t(apply(draws2[, gamma_int2], 1, sum)))
  if (!is.null(gamma_slope)) {
  sig_mat <- exp(rep(1, n) %*% t(apply(draws2[, gamma_int2], 1, sum)) +
     x_lt %*% t(apply(draws2[, gamma_slope2], 1, sum)))
  }

  pred_up0 <- mu_mat + sig_mat
  pred_up <- apply(pred_up0, 1, \(x)quantile(x, 0.975))

  pred_lo0 <- mu_mat - sig_mat
  pred_lo <- apply(pred_lo0, 1, \(x)quantile(x, 0.025))

  mean_mu <- apply(mu_mat, 1, mean)

  x_bar <- log(trait) |> mean()
  x_s <- log(trait) |> sd()
  tibble(pred = mean_mu, pred_up, pred_lo,
         x = exp(x_bar + x_s * x_lt), punch = punch)
}


#' @title Figure for predictions
pred_mcmc <- function(draws, sp_mean, n = 80) {
  large_ld <- pred_each(draws,
            beta_int = "beta_0",
            beta_slope = "beta_1",
            gamma_int = "gamma_0",
            trait = sp_mean$ld_leaf,
            punch = "1.0-cm")
  large_la <- pred_each(draws,
            beta_int = "beta_0",
            beta_slope = "beta_2",
            gamma_int = "gamma_0",
            trait = sp_mean$la,
            punch = "1.0-cm")
  large_lt <- pred_each(draws,
            beta_int = "beta_0",
            #beta_slope = "beta_3",
            gamma_int = "gamma_0",
            trait = sp_mean$lt,
            punch = "1.0-cm")

  small_ld <- pred_each(draws,
            beta_int = c("beta_0", "beta_4"),
            gamma_int = c("gamma_0", "gamma_4"),
            gamma_slope = c("gamma_1", "gamma_5"),
            trait = sp_mean$ld_leaf,
            punch = "0.6-cm")
  small_la <- pred_each(draws,
            beta_int = c("beta_0", "beta_4"),
            gamma_int = c("gamma_0", "gamma_4"),
            trait = sp_mean$la,
            punch = "0.6-cm")
  small_lt <- pred_each(draws,
            beta_int = c("beta_0", "beta_4"),
            gamma_int = c("gamma_0", "gamma_4"),
            gamma_slope = c("gamma_3", "gamma_7"),
            trait = sp_mean$lt,
            punch = "0.6-cm")

  d_lt <- bind_rows(large_lt, small_lt)
  d_la <- bind_rows(large_la, small_la)
  d_ld <- bind_rows(large_ld, small_ld)

  x_lt <- seq(-2, 2, length = n)
  my_col <- RColorBrewer::brewer.pal(4, "RdBu")

  plot_fun <- function(data, xlab) {
    ggplot(data, aes(x = x, fill = punch)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_ribbon(aes(ymax = pred_up, ymin = pred_lo), alpha = 0.6) +
    geom_line(aes(y = pred, col = punch)) +
    #xlab("Leaf thickness (mm)") +
    xlab(xlab) +
    ylab("Whole-leaf / leaf disc LMA ratio") +
    scale_x_log10() +
    scale_color_manual(
      values = my_col[c(2, 4)],
      name = "Leaf punch diameter"
    ) +
    scale_fill_manual(
      values = my_col[c(2, 4)],
      name = "Leaf punch diameter"
    ) +
    coord_cartesian(ylim = c(0.5, 1.5)) +
    theme_bw() +
    theme(legend.position = "none")
  }

  p_lt <- plot_fun(d_lt, xlab = "Leaf thickness (mm)")
  p_la <- plot_fun(d_la, xlab = expression(paste("Leaf area (", cm^2,")")))
  p_ld <- plot_fun(d_ld, xlab = expression(paste("Leaf tissue density (g ", cm^-3,")"))) +
      theme(
      text = element_text(family = "Arial"),
      legend.position = c(0.6, 0.21),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.2, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8)
    )

  p_ld + p_la + p_lt +
      plot_annotation(tag_levels = "a") &
      theme(
        text = element_text(family = "Arial", size = 8.5),
        plot.tag = element_text(face = "bold")
      )
}

#' @title Check divergence
div_check <- function(diags) {
  n1 <- diags |>
    filter(divergent__ == 1) |>
    nrow()
  n2 <- diags |>
    nrow()
  print(paste(n1, "of", n2, "iterations ended with a divergence", n1/n2 * 100, "%" ))
}


#' @title Create stan data from the observed data for
#' Yakushimia and Yunnan separtely
clean_stan_data_sep <- function(sp_mean, yaku = TRUE) {
  if (yaku) {
    sp_mean <- sp_mean |> filter(location == "Yakushima")
  } else {
    sp_mean <- sp_mean |> filter(location != "Yakushima")
  }

  x <- cbind(
    intercept = rep(1, nrow(sp_mean)),
    #lma_disc = sp_mean$lma_disc / sp_mean$lt,
    lma_disc = sp_mean$lma_leaf,
    la = sp_mean$la,
    lt = sp_mean$lt
  )

  x[, -1] <- apply(x[, -1], 2, \(x)scale(log(x)))

  # use non-scaled value for LMA
  log_lma_disc <- sp_mean$lma_disc |> log()
  log_lma_leaf <- sp_mean$lma_leaf |> log()

  list(
    N = nrow(sp_mean),
    K = ncol(x),
    log_y = log_lma_leaf,
    log_lma_disc = log_lma_disc,
    x = x
  )
}

# clean_stan_data_cv <- function(sp_mean, sp_cv) {
#   sp_mean <- left_join(sp_cv, sp_mean)
#   x <- cbind(
#     intercept = rep(1, nrow(sp_mean)),
#     lma_disc_cv = sp_mean$lma_disc_cv,
#     lma_disc = sp_mean$ld_leaf,
#     la = sp_mean$la,
#     lt = sp_mean$lt
#   )

#   x[, -1] <- apply(x[, -1], 2, \(x)scale(log(x)))

#   list(
#     N = nrow(sp_cv),
#     K = ncol(x),
#     log_y = sp_mean$lma_leaf_cv,
# #    log_lma_disc = sp_mean$lma_disc_cv,
#     x = x
#   )
# }
