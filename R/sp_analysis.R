
#' @title Leaf disc vs whole-leaf LMA (pooled species-level)
lalt_pool_grid_point <- function(data) {
  data |>
    ggplot(aes(x = lma_disc, y = lma_leaf)) +
    geom_point(alpha = 0.8) +
    scale_x_log10() +
    scale_y_log10() +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    geom_sma(se = TRUE, nboot = 2000) +
    facet_grid(la_gr2 ~ lt_gr2, scale = "free", labeller = label_parsed) +
    xlab(expression(Leaf ~ disc ~ LMA ~ (g ~ m^{
      -2
    }))) +
    ylab(expression(Whole - leaf ~ LMA ~ (g ~ m^{
      -2
    }))) +
    stat_cor(
      aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~"),
          family = "Arial")
    ) +
    theme_bw() +
    theme(text = element_text(family = "Arial"))
}


#' @title Leaf disc vs whole-leaf LMA (separted species-level)
lalt_sep_grid_point <- function(data) {
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
    theme_bw() +
    theme(
      text = element_text(family = "Arial"),
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

#' @title SMA table (species-level)
generate_sma_ld_tab <- function(sp_mean) {
  sp_mean <- sp_mean |>
    mutate(lalt_gr = paste(ld_gr, "~", lalt_gr))

  sma_lma <- sma(log10(lma_leaf) ~ log10(lma_disc),
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  sma_lma_lalt_gr <- sma(log10(lma_leaf) ~ log10(lma_disc) * lalt_gr,
    data = sp_mean,
    elev.test = 0,
    slope.test = 1
  )

  # we don't need leaf density
  tb0 <- lapply(
    list(
      lma = sma_lma,
      lma_gr = sma_lma_lalt_gr
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


#' @title LMA and LD (species-level)
lma_ld_wrap_point <- function(sp_mean) {
  d_leaf <- sp_mean |>
    dplyr::select(c(species:data_contributor,
      lma = lma_leaf, ldmc = ldmc_leaf, ld = ld_leaf
    )) |>
    pivot_longer(lma:ld, names_to = "trait", values_to = "whole_leaf") |>
    filter(location != "Yakushima" | trait != "ld") |>
    filter(location != "Yakushima" | trait != "lma")
  d_disc <- sp_mean |>
    dplyr::select(species, location, lma = lma_disc, ldmc = ldmc_disc, ld = ld_disc) |>
    pivot_longer(lma:ld, names_to = "trait", values_to = "leaf_disc") |>
    filter(location != "Yakushima" | trait != "ld") |>
    filter(location != "Yakushima" | trait != "lma")
  d2 <- full_join(d_leaf, d_disc) |>
    mutate(trait = factor(trait, c("lma", "ldmc", "ld"))) |>
    mutate(trait_lab = factor(trait,
      labels = c(
        "LMA~(g~m^{-2})",
        "LDMC~(g~g^{-1})",
        "LD~(g~cm^{-3})"
      )
    ))
  p_all <- d2 |>
    filter(trait != "ldmc") |>
    ggplot(aes(x = leaf_disc, y = whole_leaf)) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    geom_point(alpha = 0.6) +
    scale_x_log10() +
    scale_y_log10() +
    geom_sma(se = TRUE, nboot = 2000) +
    facet_wrap(~trait_lab, scale = "free", labeller = label_parsed) +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~"),
        family = "Arial"
      )
    ) +
    xlab("Leaf disc") +
    ylab("Whole-leaf") +
    theme_bw() +
    theme(
      text = element_text(family = "Arial"))
  p_all
}


#' @title CV
cv_pool_point <- function(sp_cv) {
  sp_cv |>
    ggplot(aes(x = lma_disc_cv, y = lma_leaf_cv)) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    geom_point(alpha = 0.6) +
    scale_x_continuous(trans = "sqrt", breaks = c(0.001, 0.01, 0.05, 0.1)) +
    scale_y_continuous(trans = "sqrt", breaks = c(0.001, 0.01, 0.05, 0.1)) +
    geom_sma(se = TRUE, nboot = 2000) +
    ylab("CV of whole-leaf LMA") +
    xlab("CV of leaf disc LMA") +
    coord_fixed() +
    stat_cor(
      label.x.npc = 0.3,
      label.y.npc = 0,
      vjust = 1.5,
      aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~"), family = "Arial")
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Arial"))
}

cv_sep_point <- function(sp_cv) {
  p_cv1 <- sp_cv |>
    filter(location != "Yakushima") |>
    ggplot(aes(x = lma_disc_cv, y = lma_leaf_cv)) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    geom_point(alpha = 0.6) +
    scale_x_continuous(trans = "sqrt") +
    scale_y_continuous(trans = "sqrt") +
    geom_sma(se = TRUE, nboot = 2000) +
    ggtitle("Diameter: 0.6 cm") +
    ylab("CV of whole-leaf LMA") +
    xlab("CV of leaf disc LMA") +
    # coord_fixed(xlim = c(0.01, 5.4), ylim = c(0.01, 5.4)) +
    # coord_fixed() +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~"),
        family = "Arial"
      )
    )

  p_cv2 <- sp_cv |>
    filter(location == "Yakushima") |>
    ggplot(aes(x = lma_disc_cv, y = lma_leaf_cv)) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    geom_point(alpha = 0.6) +
    scale_x_continuous(trans = "sqrt") +
    scale_y_continuous(trans = "sqrt") +
    geom_sma(se = TRUE) +
    ggtitle("Diameter: 1.0 cm") +
    ylab("CV of whole-leaf LMA") +
    xlab("CV of leaf disc LMA") +
    # coord_fixed(xlim = c(0.01, 5.4), ylim = c(0.01, 5.4)) +
    # coord_fixed() +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~"),
        family = "Arial"
      )
    )

  (p_cv1 + p_cv2) +
    plot_annotation(tag_levels = "a") &
    theme_bw() &
    theme(
      text = element_text(family = "Arial"))
}


petiole_point <- function(yaku_sp) {
  pet1 <- ggplot(yaku_sp, aes(x = petiole_ratio, y = lma_leaf / lma_disc)) +
    geom_point() +
    scale_x_log10() +
    # xlab(expression(Petiole/leaf~dry~mass~ratio(g~g^-1))) +
    xlab("Petiole / leaf dry mass ratio") +
    ylab("Whole-leaf / leaf disc LMA ratio") +
    geom_smooth(method = "lm") +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~"),
        family = "Arial"
      )
    )

  pet2 <- ggplot(yaku_sp, aes(x = petiole_ratio, y = la)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Petiole / leaf dry mass ratio") +
    ylab(expression(Leaf ~ area ~ (g ~ cm^2))) +
    geom_smooth(method = "lm") +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~"),
        family = "Arial"
      )
    )

  pet2 + pet1 +
    plot_annotation(tag_levels = "a") &
    theme_bw() &
    theme(plot.tag = element_text(face = "bold"))
}
