

la_lt_grid <- function(data) {
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
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Arial")
    )
}
