
#' @title Leaf disc vs whole-leaf LMA (pooled species-level)
lalt_pool_grid <- function(data) {
  data |>
    ggplot(aes(x = lma_disc, y = lma_leaf)) +
    geom_point(alpha = 0.8) +
    scale_x_log10() +
    scale_y_log10() +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    geom_sma(se = TRUE) +
    facet_grid(la_gr2 ~ lt_gr2, scale = "free", labeller = label_parsed) +
    xlab(expression(Leaf ~ disc ~ LMA ~ (g ~ m^{
      -2
    }))) +
    ylab(expression(Whole - leaf ~ LMA ~ (g ~ m^{
      -2
    }))) +
    stat_cor(
      aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~"), family = "Arial")
    )
}


#' @title Leaf disc vs whole-leaf LMA (separted species-level)
lalt_sep_grid <- function(data) {
  my_col <- RColorBrewer::brewer.pal(4, "RdBu")
  data |>
    mutate(punch_size = ifelse(location == "Yakushima", "1.0 cm", "0.6 cm")) |>
    ggplot(aes(x = lma_disc, y = lma_leaf, col = punch_size)) +
    geom_point(alpha = 0.6) +
    scale_x_log10() +
    scale_y_log10() +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    geom_sma(se = FALSE) +
    scale_color_manual(
      values = my_col[c(2, 4)],
      name = "Diameter of the leaf punch"
    ) +
    facet_grid(la_gr2 ~ lt_gr2, scale = "free", labeller = label_parsed) +
    xlab(expression(Leaf ~ disc ~ LMA ~ (g ~ m^{
      -2
    }))) +
    ylab(expression(Whole - leaf ~ LMA ~ (g ~ m^{
      -2
    }))) +
    stat_cor(
      aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~"), family = "Arial"),
      show.legend = FALSE
    ) +
    theme(
      legend.position = c(0.3, 0.1),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.2, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 9)
    )
}


#' @title extract sma coeffs from sma object
extract_sma <- function(fit) {
  tb1 <- fit$groupsummary |>
    mutate(slope = paste0(
      round(Slope, 2), " [", round(Slope_lowCI, 2), ", ",
      round(Slope_highCI, 2), "]"
    )) |>
    mutate(intercept = paste0(
      round(Int, 2), " [", round(Int_lowCI, 2), ", ",
      round(Int_highCI, 2), "]"
    )) |>
    mutate(sig_slope = case_when(
      Slope_highCI < 1 ~ "sig",
      Slope_lowCI > 1 ~ "sig",
      TRUE ~ "ns"
    )) |>
    mutate(sig_int = ifelse(Int_lowCI * Int_highCI > 0, "sig", "ns")) |>
    mutate(r2 = round(r2, 2)) |>
    dplyr::select(slope, intercept, r2, sig_slope, sig_int, group)

  tb2 <- tb1 |>
    mutate(slope = ifelse(sig_slope == "sig", paste0("**", slope, "**"), slope)) |>
    mutate(intercept = ifelse(sig_int == "sig", paste0("**", intercept, "**"),
      intercept
    )) |>
    dplyr::select(slope, intercept, `*R^2^*` = r2)
  list(tb1, tb2, group = tb1$group)
}

#' @title SMA table (species-level)
generate_sma_tab <- function(sp_mean) {
  sma_lma <- sma(log10(lma_leaf) ~ log10(lma_disc),
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  sma_ld <- sma(log10(ld_leaf) ~ log10(ld_disc),
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  sma_lma_lalt_gr <- sma(log10(lma_leaf) ~ log10(lma_disc) * lalt_gr,
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  sma_ld_lalt_gr <- sma(log10(ld_leaf) ~ log10(ld_disc) * lalt_gr,
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  # we don't need leaf density
  tb0 <- lapply(
    list(
      lma = sma_lma,
      ld = sma_ld,
      lma_gr = sma_lma_lalt_gr,
      ld_gr = sma_ld_lalt_gr
    ),
    extract_sma
  )

  tb <- rbind(tb0$lma[[2]], tb0$lma_gr[[2]])
  tb <- cbind(c("All", tb0$lma_gr$group), tb) |>
    as_tibble() |>
    rename(Slope = slope, Intercept = intercept)

  colnames(tb)[1] <- "Data"

  tb
}
