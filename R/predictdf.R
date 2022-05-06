# https://github.com/mattocci27/ggsma/blob/main/R/predictdf.R
#
# Prediction data frame
# Get predictions with standard errors into data frame
sma_fun <- function(data) {
  data <- as_tibble(data)
  sign <- ifelse(cor(data$y, data$x) >= 0, 1, -1)
  slope <- sign * sd(data$y) / sd(data$x)
  intercept <- mean(data$y) - slope * mean(data$x)
  tibble(intercept, slope)
}

boot_fit <- function(data, nboot = 2000) {
  data <- tibble(x = log(data$lma_disc), y = log(data$lma_leaf))
  xseq <- mean(data$x)
  level <- 0.95
  boot_fit_dat <- modelr::bootstrap(data, n = nboot, id = 'boot_num') %>%
      group_by(boot_num) %>%
      mutate(map_dfc(strap, sma_fun)) %>%
      do(data.frame(fitted = .$intercept + .$slope * xseq, xseq)) %>%
      ungroup %>%
      group_by(xseq) %>%
      dplyr::summarise(., conf_low = quantile(fitted, 0.5 - level / 2),
                         conf_mean = mean(fitted),
                       conf_high = quantile(fitted, level / 2 + 0.5))
  exp(boot_fit_dat)
}
