clean_stan_data <- function(sp_mean, interaction = TRUE) {
  x <- cbind(
    intercept = rep(1, nrow(sp_mean)),
    #lma_disc = sp_mean$lma_disc |> log10() |> scale(),
    #la = sp_mean$la |> log() |> scale() |> as.numeric(),
    #lt = sp_mean$lt |> log() |> scale() |> as.numeric()
    la = sp_mean$la |> log() |> as.numeric(),
    lt = sp_mean$lt |> log() |> as.numeric()
  )
  if (interaction){
    x <- cbind(x, x[, 2] * x[, 3])
    colnames(x) <- c("intercept", "la", "lt", "lalt")
  }
  #log_lma_disc <- sp_mean$lma_disc |> log() |> scale() |> as.numeric()
  # log_lma_leaf <- sp_mean$lma_leaf |> log() |> scale() |> as.numeric()
  log_lma_disc <- sp_mean$lma_disc |> log() |> as.numeric()
  log_lma_leaf <- sp_mean$lma_leaf |> log() |> as.numeric()
  list(
    N = nrow(sp_mean),
    K = ncol(x),
    log_y = log_lma_leaf,
    log_lma_disc = log_lma_disc,
    x = x)
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