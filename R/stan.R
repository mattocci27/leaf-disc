clean_stan_data <- function(sp_mean, model = c("no", "LMA", "LD", "LD2"), int = FALSE) {
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
  } else if (model == "LD3" | model == "punch" | model == "punch2") {
    x <- cbind(
      intercept = rep(1, nrow(sp_mean)),
      #lma_disc = sp_mean$lma_disc / sp_mean$lt,
      lma_disc = sp_mean$ld_disc,
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

  x2[, -1] <- apply(x2[, -1], 2, \(x)scale(log(x)))
  x[, -1] <- apply(x[, -1], 2, \(x)scale(log(x)))

  if (model == "punch") {
    yaku <- ifelse(sp_mean$location == "Yakushima", 0, 1)
    #yaku <- ifelse(sp_mean$location == "Yakushima", 0.5^2 * pi, 0.3^2*pi)
    #yaku <- scale(yaku) |> as.numeric()
    x <- cbind(x,
    punch = yaku,
    int1 = x[,2] * yaku,
    int2 = x[,3] * yaku,
    int3 = x[,4] * yaku
    )
  } else if (model == "punch2") {
    yaku <- ifelse(sp_mean$location == "Yakushima", 0, 1)
    # yaku <- ifelse(sp_mean$location == "Yakushima", 0.5^2 * pi, 0.3^2*pi)
    # yaku <- scale(yaku) |> as.numeric()
    x <- cbind(x,
    punch = yaku
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

clean_stan_data_sep <- function(sp_mean, yaku = TRUE) {
  if (yaku) {
    sp_mean <- sp_mean |> filter(location == "Yakushima")
  } else {
    sp_mean <- sp_mean |> filter(location != "Yakushima")
  }

  x <- cbind(
    intercept = rep(1, nrow(sp_mean)),
    #lma_disc = sp_mean$lma_disc / sp_mean$lt,
    lma_disc = sp_mean$ld_disc,
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

clean_stan_data2 <- function(sp_mean, dry_mass = TRUE, scale = FALSE, ld = FALSE) {
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

create_stan_tab_add <- function(draws) {
  tmp <- draws |>
    dplyr::select(contains(c("beta", "gamma"))) |>
    mutate(`beta[6]` = `beta[6]` + `beta[2]`) |>
    mutate(`beta[7]` = `beta[7]` + `beta[3]`) |>
    mutate(`beta[8]` = `beta[8]` + `beta[4]`) |>
    mutate(`gamma[6]` = `gamma[6]` + `gamma[2]`) |>
    mutate(`gamma[7]` = `gamma[7]` + `gamma[3]`) |>
    mutate(`gamma[8]` = `gamma[8]` + `gamma[4]`)
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

coef_pointrange2 <- function(data, ld = FALSE, lm = FALSE, lalt = FALSE) {
  data1 <- data |>
    filter(para != "gamma[1]") |>
    filter(para != "beta[1]") |>
    filter(str_detect(para, "beta")) |>
    mutate(para = case_when(
    #  para == "beta[1]" ~ "Intercept for mean",
      para == "beta[2]" ~ "Effect of disc LMA on mean",
      para == "beta[3]" ~ "Effect of LA on mean",
      para == "beta[4]" ~ "Effect of LT on mean"
    )) |>
    mutate(para = factor(para,
      levels = c(
    #  "Intercept for mean",
      "Effect of disc LMA on mean",
      "Effect of LA on mean",
      "Effect of LT on mean"
    ) |> rev()))

  data2 <- data |>
    filter(para != "gamma[1]") |>
    filter(para != "beta[1]") |>
    filter(str_detect(para, "gamma")) |>
    mutate(para = case_when(
      para == "gamma[2]" ~ "Effect of disc LMA on variance",
      para == "gamma[3]" ~ "Effect of LA on variance",
      para == "gamma[4]" ~ "Effect of LT on variance"
    )) |>
    mutate(para = factor(para,
      levels = c(
      "Effect of disc LMA on variance",
      "Effect of LA on variance",
      "Effect of LT on variance"
    ) |> rev()))


    p1 <- data1 |>
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
        aes(x = mean_),
        color = "#3366FF",
        fill =  "#33CCFF",
        shape = 21,
        size = 3) +
      ylab("") +
      xlab("Standardized coefficients") +
      scale_y_discrete(labels = c(
    #    "Intercept for mean" = expression(Intercept~of~mean~(beta[0])),
        "Effect of disc LMA on mean" = expression(Effect~of~LMA~on~mean~(beta[1])),
        "Effect of LA on mean" = expression(Effect~of~LA~on~mean~(beta[2])),
        "Effect of LT on mean" = expression(Effect~of~LT~on~mean~(beta[3]))
      )) +
      theme_bw() +
      theme(
        text = element_text(family = "Arial"))

    p2 <- data2 |>
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
        aes(x = mean_),
        color = "#3366FF",
        fill =  "#33CCFF",
        shape = 21,
        size = 3) +
      ylab("") +
      xlab("Standardized coefficients") +
      scale_y_discrete(labels = c(
        "Effect of disc LMA on variance" = expression(Effect~of~LMA~on~variance~(gamma[1])),
        "Effect of LA on variance" = expression(Effect~of~LA~on~variance~(gamma[2])),
        "Effect of LT on variance" = expression(Effect~of~LT~on~variance~(gamma[3]))
      )) +
      theme_bw() +
      theme(
        text = element_text(family = "Arial"))

    if(ld) {
      p1 <- p1 + scale_y_discrete(labels = c(
    #    "Intercept for mean" = expression(Intercept~of~mean~(beta[0])),
        "Effect of disc LMA on mean" = expression(Effect~of~LD~on~mean~(beta[1])),
        "Effect of LA on mean" = expression(Effect~of~LA~on~mean~(beta[2])),
        "Effect of LT on mean" = expression(Effect~of~LT~on~mean~(beta[3]))
      ))
      p2 <- p2 + scale_y_discrete(labels = c(
        "Effect of disc LMA on variance" = expression(Effect~of~LD~on~variance~(gamma[1])),
        "Effect of LA on variance" = expression(Effect~of~LA~on~variance~(gamma[2])),
        "Effect of LT on variance" = expression(Effect~of~LT~on~variance~(gamma[3]))
      ))
    }
    if(lm) {
      p1 <- p1 + scale_y_discrete(labels = c(
    #    "Intercept for mean" = expression(Intercept~of~mean~(beta[0])),
        "Effect of disc LMA on mean" = expression(Effect~of~LM~on~mean~(beta[1])),
        "Effect of LA on mean" = expression(Effect~of~LA~on~mean~(beta[2])),
        "Effect of LT on mean" = expression(Effect~of~LT~on~mean~(beta[3]))
      ))
      p2 <- p2 + scale_y_discrete(labels = c(
        "Effect of disc LMA on variance" = expression(Effect~of~LM~on~variance~(gamma[1])),
        "Effect of LA on variance" = expression(Effect~of~LA~on~variance~(gamma[2])),
        "Effect of LT on variance" = expression(Effect~of~LT~on~variance~(gamma[3]))
      ))
    }
    if(lalt) {
      # label is correct
      p1 <- p1 + scale_y_discrete(labels = c(
        "Effect of disc LMA on mean" = expression(Effect~of~LA~on~mean~(beta[1])),
        "Effect of LA on mean" = expression(Effect~of~LT~on~mean~(beta[2]))
      ))
      # label is correct
      p2 <- p2 + scale_y_discrete(labels = c(
        "Effect of disc LMA on variance" = expression(Effect~of~LA~on~variance~(gamma[1])),
        "Effect of LA on variance" = expression(Effect~of~LT~on~variance~(gamma[2]))
      ))
    }

    p1 / p2 +
    plot_annotation(tag_levels = "a") &
    theme(
      text = element_text(family = "Arial"))
}

coef_pointrange3 <- function(data, ld = FALSE, lm = FALSE, lalt = FALSE) {
  data <- data |>
    mutate(ci_sig = case_when(
      lwr2_5 * upr97_5 > 0 ~ "sig95",
      lwr5 * upr95 > 0 ~ "sig90",
      TRUE ~ "ns"
      ))

  data1 <- data |>
    filter(str_detect(para, "beta")) |>
    filter(para != "beta[1]") |>
    mutate(para = factor(para, levels = rev(para)))

  data2 <- data |>
    filter(str_detect(para, "gamma")) |>
    filter(para != "gamma[1]") |>
    mutate(para = factor(para, levels = rev(para)))

  p1 <- data1 |>
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
      aes(x = mean_, fill = ci_sig),
      color = "#3366FF",
      shape = 21,
      size = 3) +
    ylab("") +
    scale_fill_manual(
      values = c(
        "sig95" = "#33CCFF",
        "sig90" = "grey",
        "ns" = "#FFFFFF"
      )) +
    xlab("Standardized coefficients") +
    scale_y_discrete(labels = c(
      "beta[2]" = expression(Effect~of~LD~on~mean~(beta[1])),
      "beta[3]" = expression(Effect~of~LA~on~mean~(beta[2])),
      "beta[4]" = expression(Effect~of~LT~on~mean~(beta[3])),
      "beta[5]" = expression(Effect~of~punch~size~on~mean~(beta[4])),
      "beta[6]" = expression(Effect~of~LD%*%punch~size~on~mean~(beta[5])),
      "beta[7]" = expression(Effect~of~LA%*%punch~size~on~mean~(beta[6])),
      "beta[8]" = expression(Effect~of~LT%*%punch~size~on~mean~(beta[7]))
    ))

  p2 <- data2 |>
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
      aes(x = mean_, fill = ci_sig),
      color = "#3366FF",
      shape = 21,
      size = 3) +
    scale_fill_manual(
      values = c(
        "sig95" = "#33CCFF",
        "sig90" = "grey",
        "ns" = "#FFFFFF"
      )) +
    ylab("") +
    xlab("Standardized coefficients") +
    scale_y_discrete(labels = c(
      "gamma[2]" = expression(Effect~of~LD~on~variance~(gamma[1])),
      "gamma[3]" = expression(Effect~of~LA~on~variance~(gamma[2])),
      "gamma[4]" = expression(Effect~of~LT~on~variance~(gamma[3])),
      "gamma[5]" = expression(Effect~of~punch~size~on~variance~(gamma[4])),
      "gamma[6]" = expression(Effect~of~LD%*%punch~size~on~variance~(gamma[5])),
      "gamma[7]" = expression(Effect~of~LA%*%punch~size~on~variance~(gamma[6])),
      "gamma[8]" = expression(Effect~of~LT%*%punch~size~on~variance~(gamma[7]))
    ))

    p1 / p2 +
    plot_annotation(tag_levels = "a") &
    theme_bw() &
    theme(
      legend.position = "none",
      text = element_text(family = "Arial"))
}

#' @title use mean estiamte
coef_pointrange4 <- function(data) {

  data <- data |>
    mutate(sig = ifelse(lwr2_5 * upr97_5 > 0, "sig", "ns")) |>
    mutate(ci_sig = case_when(
      lwr2_5 * upr97_5 > 0 ~ "sig95",
      lwr5 * upr95 > 0 ~ "sig90",
      TRUE ~ "ns"
      ))

  data1 <- data |>
    filter(str_detect(para, "beta")) |>
    filter(para != "beta[1]") |>
    mutate(para = factor(para, levels = rev(para)))

  data2 <- data |>
    filter(str_detect(para, "gamma")) |>
    filter(para != "gamma[1]") |>
    mutate(para = factor(para, levels = rev(para)))

  p1 <- data1 |>
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
    scale_fill_manual(
      values = c(
        "sig" = "#33CCFF",
        # "sig95" = "#33CCFF",
        # "sig90" = "grey",
        "ns" = "#FFFFFF"
      )) +
    xlab("Standardized coefficients") +
    scale_y_discrete(labels = c(
      "beta[2]" = expression(paste("Effect of LD on mean (large punch; ", beta[1], ")")),
      "beta[3]" = expression(paste("Effect of LA on mean (large punch; ", beta[2], ")")),
      "beta[4]" = expression(paste("Effect of LT on mean (large punch; ", beta[3], ")")),
      "beta[5]" = expression(Effect~of~small~punch~on~mean~(beta[4])),
      "beta[6]" = expression(paste("Effect of LD on mean (small punch; ", beta[1]+beta[5], ")")),
      "beta[7]" = expression(paste("Effect of LA on mean (small punch; ", beta[2]+beta[6], ")")),
      "beta[8]" = expression(paste("Effect of LT on mean (small punch; ", beta[3]+beta[7], ")"))
    ))

  p2 <- data2 |>
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
    scale_fill_manual(
      values = c(
        "sig" = "#33CCFF",
        # "sig95" = "#33CCFF",
        # "sig90" = "grey",
        "ns" = "#FFFFFF"
      )) +
    ylab("") +
    xlab("Standardized coefficients") +
    scale_y_discrete(labels = c(
      "gamma[2]" = expression(paste("Effect of LD on variance (large punch; ", gamma[1], ")")),
      "gamma[3]" = expression(paste("Effect of LA on variance (large punch; ", gamma[2], ")")),
      "gamma[4]" = expression(paste("Effect of LT on variance (large punch; ", gamma[3], ")")),
      "gamma[5]" = expression(Effect~of~small~punch~on~variance~(gamma[4])),
      "gamma[6]" = expression(paste("Effect of LD on variance (small punch; ", gamma[1]+gamma[5], ")")),
      "gamma[7]" = expression(paste("Effect of LA on variance (small punch; ", gamma[2]+gamma[6], ")")),
      "gamma[8]" = expression(paste("Effect of LT on variance (small punch; ", gamma[3]+gamma[7], ")"))
    ))

    p1 / p2 +
    plot_annotation(tag_levels = "a") &
    theme_bw() &
    theme(
      legend.position = "none",
      text = element_text(family = "Arial"))
}

div_check <- function(diags) {
  n1 <- diags |>
    filter(divergent__ == 1) |>
    nrow()
  n2 <- diags |>
    nrow()
  print(paste(n1, "of", n2, "iterations ended with a divergence", n1/n2 * 100, "%" ))
}