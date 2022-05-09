write_yml <- function(path, sp_mean, full_data_cv_csv, tree, lma_yaku_re, boot_fit_dat) {
  r2_lma <- cor.test(log(sp_mean$lma_leaf), log(sp_mean$lma_disc))$estimate^2 |> round(2)
  r2_ld <- cor.test(log(sp_mean$ld_leaf), log(sp_mean$ld_disc))$estimate^2 |> round(2)

  la_mid <- median(sp_mean$la, na.rm = TRUE) |> round(1)
  lt_mid <- median(sp_mean$lt, na.rm = TRUE) |> round(2)
  ld_mid <- median(sp_mean$ld_leaf, na.rm = TRUE) |> round(2)
  la_mid2 <- median(tree$la, na.rm = TRUE) |> round(1)
  lt_mid2 <- median(tree$lt, na.rm = TRUE) |> round(2)

  # LA-LT
  res <- cor.test(log(sp_mean$lt), log(sp_mean$la))
  la_lt_p <- res$p.value
  la_lt_p <- ifelse(la_lt_p < 0.05, "< 0.05", la_lt_p)

  pet <- lma_yaku_re |>
    filter(!is.na(petiole_dw))

  d_cv <- full_data_cv_csv |>
    filter(!is.na(lma_disc)) |>
    filter(!is.na(lma_leaf))

  lma_mean <- sp_mean |>
    group_by(ldlalt_gr) |>
    summarise(
      lma_leaf = mean(lma_leaf),
      lma_disc = mean(lma_disc))

  fit_ols <- lm(log(lma_leaf) ~ log(lma_disc), sp_mean)
  var_fun <- function(fit) {
    sig_ols <- sqrt(deviance(fit_ols) / df.residual(fit_ols))
    exp(sig_ols^2 / 2)
  }
  sig_ols <- sqrt(deviance(fit_ols) / df.residual(fit_ols))
  var_ols <- var_fun(fit_ols)

  # exp(coef(fit_ols)[1] + log(var_ols)) |> round(3)
  # exp(coef(fit_ols)[1]) |> round(3)
#  1.62 * 1.01

  fit_ols_yaku <- lm(log(lma_leaf) ~ log(lma_disc), sp_mean|> filter(location == "Yakushima"))
  sig_ols_yaku <- sqrt(deviance(fit_ols_yaku) / df.residual(fit_ols_yaku))

  fit_ols_inv <- lm(log(lma_disc) ~ log(lma_leaf), sp_mean)
  sig_ols_inv <- sqrt(deviance(fit_ols_inv) / df.residual(fit_ols_inv))

  fit_ols_yaku_inv <- lm(log(lma_disc) ~ log(lma_leaf), sp_mean|> filter(location == "Yakushima"))
  sig_ols_yaku_inv <- sqrt(deviance(fit_ols_yaku_inv) / df.residual(fit_ols_yaku_inv))

  fit_sma <- sma(log(lma_leaf) ~ log(lma_disc), sp_mean)

  sma_lma <- sma(log10(lma_leaf) ~ log10(lma_disc),
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  sma_lma_gr <- sma(log10(lma_leaf) ~ log10(lma_disc) * ldlalt_gr,
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  lma_mean2 <- left_join(lma_mean, sma_lma_gr$groupsummary, by = c("ldlalt_gr" = "group"))

  dens_thin_large <- lma_mean2 |>
    filter(ldlalt_gr == "Dense~Thin~Large")
  nondens_thin_large <- lma_mean2 |>
    filter(ldlalt_gr == "Nondense~Thin~Large")
  nondens_thick_large <- lma_mean2 |>
    filter(ldlalt_gr == "Nondense~Thick~Large")

  lma_disc_mean <- median(sp_mean$lma_disc, na.rm = TRUE) |> round(1)
  sma_all <- cbind(lma_disc = lma_disc_mean, sma_lma$groupsummary)

  my_fun <- function(data) {
    mean_ <- ((data$lma_disc^data$Slope * 10^data$Int) / data$lma_disc - 1) * 100
    #lwr_ <- ((data$lma_disc^data$Slope * 10^data$Int_lowCI) / data$lma_disc - 1) * 100
    #upr_ <- ((data$lma_disc^data$Slope * 10^data$Int_hihgCI) / data$lma_disc - 1) * 100
    #tmp <- ((lma_disc_mean^data$Slope * 10^data$Int) / lma_disc_mean - 1) * 100
    round(mean_, 1)
  }

  #boot_fit_dat <- boot_fit(sp_mean)
  lma_mean_per <- ((boot_fit_dat$conf_mean / boot_fit_dat$xseq) - 1) * 100
  lma_lwr_per <- ((boot_fit_dat$conf_low / boot_fit_dat$xseq) - 1) * 100
  lma_upr_per <- ((boot_fit_dat$conf_high / boot_fit_dat$xseq) - 1) * 100

  # library(tidyverse)
  # library(targets)
  # tar_load(sp_mean)
  # tar_load(tree)
  n_sp <- table(sp_mean$location)
  n_ind <- table(tree$location)

  output <- path
  out <- file(paste(output), "w") # write
  writeLines(
    paste0("TRF_sp: ",
           n_sp["Mengla_Bubeng"]),
    out,
    sep = "\n")
  writeLines(
    paste0("STF_sp: ",
           n_sp["Ailao_understory"]),
    out,
    sep = "\n")
  writeLines(
    paste0("HDS_sp: ",
           n_sp["Yuanjiang_Savanna"]),
    out,
    sep = "\n")
  writeLines(
    paste0("Yaku_sp: ",
           n_sp["Yakushima"]),
    out,
    sep = "\n")
  writeLines(
    paste0("TRF_ind: ",
           n_ind["Mengla_Bubeng"]),
    out,
    sep = "\n")
  writeLines(
    paste0("STF_ind: ",
           n_ind["Ailao_understory"]),
    out,
    sep = "\n")
  writeLines(
    paste0("HDS_ind: ",
           n_ind["Yuanjiang_Savanna"]),
    out,
    sep = "\n")
  writeLines(
    paste0("Yaku_ind: ",
           n_ind["Yakushima"]),
    out,
    sep = "\n")


  writeLines(
    paste0("mean_lma_disc: ",
           mean(sp_mean$lma_disc) |> round(1) |> format(nsmall = 1)),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_slope_all_mean: ",
           sma_lma$groupsummary$Slope |> round(3)
           ),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_slope_all_lwr: ",
           sma_lma$groupsummary$Slope_lowCI |> round(3)
           ),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_slope_all_upr: ",
           sma_lma$groupsummary$Slope_highCI |> round(3)
           ),
    out,
    sep = "\n")

  writeLines(
    paste0("sma_int_all_mean: ",
           sma_lma$groupsummary$Int |> round(4)|> format(nsmall = 4)
           ),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_int_all_lwr: ",
           sma_lma$groupsummary$Int_lowCI |> round(4)|> format(nsmall = 4)
           ),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_int_all_upr: ",
           sma_lma$groupsummary$Int_highCI |> round(3) |> format(nsmall = 3)
           ),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_per_all_mean: ",
           lma_mean_per |> round(2)
           ),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_per_all_lwr: ",
           lma_lwr_per |> round(2)
           ),
    out,
    sep = "\n")
  writeLines(
    paste0("sma_per_all_upr: ",
           lma_upr_per |> round(1)
           ),
    out,
    sep = "\n")



  writeLines(
    paste0("ols_int_raw: ",
           coef(fit_ols)[1] |> exp()|> round(2)),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_int: ",
           exp(coef(fit_ols)[1] + log(var_fun(fit_ols))) |> round(2)),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_slope: ",
           coef(fit_ols)[2] |> round(3)),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_var2: ",
           var_fun(fit_ols) |> round(3)),
    #0.5 * sig_ols^2 |> round(3)),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_int_inv_raw: ",
           1),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_int_inv: ",
           exp(0 + log(var_fun(fit_ols_inv))) |> round(2)),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_slope_inv: ",
           coef(fit_ols_inv)[2] |> round(3)),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_sig_inv: ",
           sig_ols_inv |> round(3)),
    out,
    sep = "\n")
  writeLines(
    paste0("ols_var2_inv: ",
           var_fun(fit_ols_inv) |> round(3)),
    #0.5 * sig_ols^2 |> round(3)),
    out,
    sep = "\n")

  writeLines(
    paste0("sma_all: ",
           my_fun(sma_all)),
    out,
    sep = "\n")
  writeLines(
    paste0("dens_thin_large: ",
           my_fun(dens_thin_large)),
    out,
    sep = "\n")
  writeLines(
    paste0("nondens_thin_large: ",
           my_fun(nondens_thin_large)),
    out,
    sep = "\n")
  writeLines(
    paste0("nondens_thick_large: ",
           my_fun(nondens_thick_large)),
    out,
    sep = "\n")
  writeLines(
    paste0("tree_no: ",
           tree |>
             filter(location == "Yakushima") |>
             nrow()),
    out,
    sep = "\n")
  writeLines(
    paste0("r2_lma: ",
           r2_lma),
    out,
    sep = "\n")
  writeLines(
    paste0("r2_ld: ",
           r2_ld),
    out,
    sep = "\n")
  writeLines(
    paste0("all_sp: ",
           sp_mean |>
             pull(species) |>
             unique() |>
             length()),
    out,
    sep = "\n")
  writeLines(
    paste0("all_tree: ",
           tree |>
             nrow()),
    out,
    sep = "\n")
  writeLines(
    paste0("yaku_sp: ",
           sp_mean |>
             filter(location == "Yakushima") |>
             pull(species) |>
             unique() |>
             length()),
    out,
    sep = "\n")
  writeLines(
    paste0("pet_sp: ",
           pet$species |> unique() |> length()),
    out,
    sep = "\n")
  writeLines(
    paste0("la_mid: ",
           la_mid),
    out,
    sep = "\n")
  writeLines(
    paste0("lt_mid: ",
           lt_mid),
    out,
    sep = "\n")
  writeLines(
    paste0("ld_mid: ",
           ld_mid),
    out,
    sep = "\n")
  writeLines(
    paste0("la_mid2: ",
           la_mid2),
    out,
    sep = "\n")
  writeLines(
    paste0("lt_mid2: ",
           lt_mid2),
    out,
    sep = "\n")
  writeLines(
    paste0("la_lt_r: ",
           res$estimate %>% round(2)),
    out,
    sep = "\n")
  writeLines(
    paste0("la_lt_p: ",
           la_lt_p),
    out,
    sep = "\n")
  writeLines(
    paste0("la_lt_n: ",
           nrow(sp_mean)),
    out,
    sep = "\n")
  writeLines(
    paste0("cv_r2: ",
           cor(sqrt(d_cv$lma_leaf),
               sqrt(d_cv$lma_disc))^2 |> round(2)),
    out,
    sep = "\n")
  close(out)
  # The return value must be a vector of paths to the files we write:
  paste("values.yml")
}
