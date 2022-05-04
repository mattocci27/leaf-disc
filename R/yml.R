write_yml <- function(path, sp_mean, full_data_cv_csv, tree, lma_yaku_re, sma_sp_ld_tab) {
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
    tmp <- ((data$lma_disc^data$Slope * 10^data$Int) / data$lma_disc - 1) * 100
    #tmp <- ((lma_disc_mean^data$Slope * 10^data$Int) / lma_disc_mean - 1) * 100
    round(tmp, 1)
  }

  output <- path
  out <- file(paste(output), "w") # write
  writeLines(paste0("sma_all: " ,
             my_fun(sma_all)),
             out,
             sep = "\n")
  writeLines(paste0("dens_thin_large: " ,
             my_fun(dens_thin_large)),
             out,
             sep = "\n")
  writeLines(paste0("nondens_thin_large: " ,
             my_fun(nondens_thin_large)),
             out,
             sep = "\n")
  writeLines(paste0("nondens_thick_large: " ,
             my_fun(nondens_thick_large)),
             out,
             sep = "\n")
  writeLines(paste0("tree_no: " ,
             tree |>
              filter(location == "Yakushima") |>
              nrow()),
             out,
             sep = "\n")
  writeLines(paste0("r2_lma: ",
             r2_lma),
             out,
             sep = "\n")
  writeLines(paste0("r2_ld: ",
             r2_ld),
             out,
             sep = "\n")
  writeLines(paste0("all_sp: ",
             sp_mean |>
             pull(species) |>
             unique() |>
             length()),
             out,
             sep = "\n")
  writeLines(paste0("yaku_sp: ",
             sp_mean |>
              filter(location == "Yakushima") |>
             pull(species) |>
             unique() |>
             length()),
             out,
             sep = "\n")
  writeLines(paste0("pet_sp: ",
             pet$species |> unique() |> length()),
             out,
             sep = "\n")
  writeLines(paste0("la_mid: ",
             la_mid),
             out,
             sep = "\n")
  writeLines(paste0("lt_mid: ",
             lt_mid),
             out,
             sep = "\n")
  writeLines(paste0("ld_mid: ",
             ld_mid),
             out,
             sep = "\n")
  writeLines(paste0("la_mid2: ",
             la_mid2),
             out,
             sep = "\n")
  writeLines(paste0("lt_mid2: ",
             lt_mid2),
             out,
             sep = "\n")
  writeLines(paste0("la_lt_r: ",
             res$estimate %>% round(2)),
             out,
             sep = "\n")
  writeLines(paste0("la_lt_p: ",
             la_lt_p),
             out,
             sep = "\n")
  writeLines(paste0("la_lt_n: ",
             nrow(sp_mean)),
             out,
             sep = "\n")
  writeLines(paste0("cv_r2: " ,
             cor(sqrt(d_cv$lma_leaf),
             sqrt(d_cv$lma_disc))^2 |> round(2)),
             out,
             sep = "\n")
  close(out)
}