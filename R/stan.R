clean_stan_data <- function(sp_mean, interaction = TRUE) {
  x <- cbind(
    intercept = rep(1, nrow(sp_mean)),
    #lma_disc = sp_mean$lma_disc |> log10() |> scale(),
    la = sp_mean$la |> log() |> scale(),
    lt = sp_mean$lt |> log() |> scale()
  )
  if (interaction) {
    x <- cbind(x, x[, 2] * x[, 3])
    colnames(x) <- c("intercept", "la", "lt", "lalt")
  } else {
    colnames(x) <- c("intercept", "la", "lt")
  }
  list(
    N = nrow(sp_mean),
    K = ncol(x),
    log_y = log(sp_mean$lma_leaf),
    log_lma_disc = sp_mean$lma_disc |> log() |> scale() |> as.numeric(),
    x = x)
}

create_dummy_data <- function(n) {
  xx1 <- rnorm(n)
  xx2 <- rnorm(n)
  xx3 <- rnorm(n)
  x <- cbind(rep(1, n), xx2, xx3)
  sig <- 1.41 + 1 * xx2 - 1 * xx3
  sig <- rnorm(n, sig, 0.2)
  yy <- rnorm(n, 0.3 + 0.9 * xx1, exp(sig))
  #lm(yy ~ xx1) |> summary()
  list(
    N = n,
    K = ncol(x),
    log_y = yy,
    log_lma_disc = xx1,
    x = x)
}