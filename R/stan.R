clean_stan_data <- function(sp_mean, dry_mass = TRUE, scale = FALSE, ld = FALSE) {
  x <- cbind(
    intercept = rep(1, nrow(sp_mean)),
    lma_disc = sp_mean$lma_disc,
    la = sp_mean$la,
    lt = sp_mean$lt
  )
  if (ld) {
    # unit doesn't matter after scaling but I'll use /1000 anyway
    x[,2] <- sp_mean$lma_disc / sp_mean$lt / 1000
  }

  x[, 2:4] <- apply(x[, 2:4], 2, \(x)log(x))

  if (scale) {
    x[, 2:4] <- apply(x[, 2:4], 2, \(x)scale(x))
  }

  if (dry_mass) {
    x2 <- cbind(x,
      dry_mass = sp_mean$dry_mass_disc |> log()
      )
  }

  if (scale && dry_mass) {
    x2[, 5] <- x2[, 5] |> exp() |> log() |> scale() |> as.numeric()
 }

  # use non-scaled value for LMA
  log_lma_disc <- sp_mean$lma_disc |> log()
  log_lma_leaf <- sp_mean$lma_leaf |> log()

  if (dry_mass) {
    list(
      N = nrow(sp_mean),
      K = ncol(x),
      K2 = ncol(x2),
      J = unique(sp_mean$species) |> length(),
      sp = sp_mean$species |> as.factor() |> as.numeric(),
      log_y = log_lma_leaf,
      log_lma_disc = log_lma_disc,
      x = x,
      x2 = x2)
  } else {
    list(
      N = nrow(sp_mean),
      K = ncol(x),
      J = unique(sp_mean$species) |> length(),
      sp = sp_mean$species |> as.factor() |> as.numeric(),
      log_y = log_lma_leaf,
      log_lma_disc = log_lma_disc,
      x = x)
  }
}

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

create_stan_tab <- function(draws) {
  tmp <- draws |>
    dplyr::select(contains(c("beta", "gamma")))
  mean_ <- apply(tmp, 2, mean)
  lwr2_5 <- apply(tmp, 2, \(x)(quantile(x, 0.025)))
  lwr5 <- apply(tmp, 2, \(x)(quantile(x, 0.05)))
  upr97_5 <- apply(tmp, 2, \(x)(quantile(x, 0.975)))
  upr95 <- apply(tmp, 2, \(x)(quantile(x, 0.9)))
  tibble(para = names(mean_), mean_, lwr2_5, lwr5, upr95, upr97_5)
}

coef_pointrange <- function(data, ld = FALSE) {
  data2 <- data |>
    filter(para != "gamma[1]") |>
    mutate(para = case_when(
      para == "beta[1]" ~ "Intercept for mean",
      para == "beta[2]" ~ "Effect of disc LMA on mean",
      para == "beta[3]" ~ "Effect of LA on mean",
      para == "beta[4]" ~ "Effect of LT on mean",
      para == "gamma[2]" ~ "Effect of disc LMA on variance",
      para == "gamma[3]" ~ "Effect of LA on variance",
      para == "gamma[4]" ~ "Effect of LT on variance"
    )) |>
    mutate(para = factor(para,
      levels = c(
      "Intercept for mean",
      "Effect of disc LMA on mean",
      "Effect of LA on mean",
      "Effect of LT on mean",
      "Effect of disc LMA on variance",
      "Effect of LA on variance",
      "Effect of LT on variance"
    ) |> rev()))

    p <- ggplot(data2, aes(y = para)) +
      geom_vline(xintercept = 0, lty  = 2, color = "grey60") +
      geom_linerange(
        aes(xmin = lwr2_5, xmax = upr97_5),
        color = "#3366FF") +
      geom_linerange(
        aes(xmin = lwr5, xmax = upr95),
        size = 1.5,
        color = "#3366FF") +
      geom_point(
        aes(x = mean_),
        color = "#3366FF",
        fill =  "#33CCFF",
        shape = 21,
        size = 3) +
      ylab("") +
      xlab("Standardized coefficients") +
      scale_y_discrete(labels = c(
        "Intercept for mean" = expression(Intercept~of~mean~(beta[0])),
        "Effect of disc LMA on mean" = expression(Effect~of~LMA~on~mean~(beta[1])),
        "Effect of LA on mean" = expression(Effect~of~LA~on~mean~(beta[2])),
        "Effect of LT on mean" = expression(Effect~of~LT~on~mean~(beta[3])),
        "Effect of disc LMA on variance" = expression(Effect~of~LMA~on~variance~(gamma[1])),
        "Effect of LA on variance" = expression(Effect~of~LA~on~variance~(gamma[2])),
        "Effect of LT on variance" = expression(Effect~of~LT~on~variance~(gamma[3]))
      )) +
      theme_bw() +
      theme(
        text = element_text(family = "Arial"))
    if(ld) {
      p <- p + scale_y_discrete(labels = c(
        "Intercept for mean" = expression(Intercept~of~mean~(beta[0])),
        "Effect of disc LMA on mean" = expression(Effect~of~LD~on~mean~(beta[1])),
        "Effect of LA on mean" = expression(Effect~of~LA~on~mean~(beta[2])),
        "Effect of LT on mean" = expression(Effect~of~LT~on~mean~(beta[3])),
        "Effect of disc LMA on variance" = expression(Effect~of~LD~on~variance~(gamma[1])),
        "Effect of LA on variance" = expression(Effect~of~LA~on~variance~(gamma[2])),
        "Effect of LT on variance" = expression(Effect~of~LT~on~variance~(gamma[3]))
      ))
    }
    p
}