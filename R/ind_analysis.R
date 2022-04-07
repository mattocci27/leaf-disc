lalt_tree_grid <- function(tree) {
  tree |>
    ggplot(aes(x = lma_disc, y = lma_leaf, col = dry_mass_disc)) +
    geom_point(alpha = 0.8) +
    scale_color_viridis_c(
      trans = "log10",
      name = "Total dry mass of \nleaf discs (g)"
    ) +
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
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Arial"),
      legend.position = c(0.39, 0.15),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.3, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    )
}


ratio_combine <- function(tree) {
  my_col <- RColorBrewer::brewer.pal(4, "RdBu")

  ratio_mass <- tree |>
    mutate(punch_size = ifelse(location == "Yakushima", "1.0 cm", "0.6 cm")) |>
    ggplot(aes(dry_mass_disc, lma_leaf / lma_disc,
      col = punch_size
    )) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 1) +
    scale_color_manual(
      values = my_col[c(2, 4)],
      name = "Diameter of the leaf punch"
    ) +
    xlab("Total dry mass of leaf disc (g)") +
    ylab("Whole-leaf LMA / leaf disc LMA") +
    theme_bw() +
    theme(
      legend.position = c(0.7, 0.75),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.3, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    )

  ratio_lt <- tree |>
    mutate(punch_size = ifelse(location == "Yakushima", "1.0 cm", "0.6 cm")) |>
    ggplot(aes(lt, lma_leaf / lma_disc,
      col = dry_mass_disc
    )) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 1) +
    scale_color_viridis_c(
      trans = "log10",
      name = "Total leaf disc \ndry mass (g)"
    ) +
    # scale_color_viridis_d() +
    scale_x_log10() +
    xlab("Leaf thickness (mm)") +
    ylab("Whole-leaf LMA / leaf disc LMA") +
    theme_bw() +
    theme(
      legend.position = c(0.7, 0.75),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.3, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    )

  ratio_la <- tree |>
    mutate(punch_size = ifelse(location == "Yakushima", "1.0 cm", "0.6 cm")) |>
    ggplot(aes(la, lma_leaf / lma_disc,
      col = dry_mass_disc
    )) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 1) +
    scale_color_viridis_c(
      trans = "log10",
      name = "Total leaf disc \ndry mass (g)"
    ) +
    # scale_color_viridis_d() +
    scale_x_log10() +
    xlab(expression(Leaf ~ area ~ (m^2))) +
    ylab("Whole-leaf LMA / leaf disc LMA") +
    theme_bw() +
    theme(
      legend.position = "none",
      # legend.position = c(0.2, 0.75),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.3, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    )

  p_ratio <- ratio_mass + ratio_lt + ratio_la +
    plot_annotation(tag_levels = "a") &
    #theme_bw() &
    theme(
      text = element_text(family = "Arial"))

  p_ratio
}

#' @title Multiple regression for log-transformed total dry mass of leaf discs
dm_lm <- function(tree) {
  tree <- tree |>
    mutate(pch = ifelse(location == "Yakushima", "1.0 cm", "0.6 cm"))

  fit <- lm(log(dry_mass_disc) ~ scale(log(lt)) + scale(log(la)) + pch, tree)

  s <- summary(fit)
  tb <- s$coefficients |> round(3) |> signif(3)
  colnames(tb) <- c("Estimate", "SE", "t-value", "*P* value")
  tb[, 4] <- ifelse(tb[, 4] < 0.001, "< 0.001", tb[,4])
  rownames(tb)[2] <- "log(Leaf thickness)"
  rownames(tb)[3] <- "log(Leaf area)"
  rownames(tb)[4] <- "Large leaf punch"
  tb
}