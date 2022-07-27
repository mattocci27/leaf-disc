#' @title Check heteroskedasticity
check_hdk <- function(tree) {
  fit <- lm(log(lma_leaf / lma_disc) ~ log(dry_mass_disc), tree)
  lmtest::bptest(fit)
}

#' @title Linear mixed model for log-transformed total dry mass of leaf discs
dm_glmm <- function(tree) {
  tree <- tree |>
    mutate(pch = ifelse(location == "Yakushima", "L1.0 cm", "S0.6 cm")) |>
    filter(!is.na(dry_mass_disc)) |>
    filter(!is.na(ld_leaf))
#  fit <- lme4::lmer(log(dry_mass_disc) ~ scale(log(lt)) + scale(log(la)) + scale(log(ld_leaf)) + pch + (1| species), tree)

  fit <- nlme::lme(log(dry_mass_disc) ~
                   scale(log(ld_leaf)) +
                   scale(log(la)) +
                   scale(log(lt)) + pch, random = ~1| species, tree)

  s <- summary(fit)
  tb <- s$tTable |> round(3) |> signif(3)
  colnames(tb) <- c("Estimate", "SE", "DF","t-value", "*P* value")
  tb[, 5] <- ifelse(tb[, 5] < 0.001, "< 0.001", tb[,4])
  rownames(tb)[2] <- "log(Leaf tissue density)"
  rownames(tb)[3] <- "log(Leaf area)"
  rownames(tb)[4] <- "log(Leaf thickness)"
  rownames(tb)[5] <- "Small leaf punch"
  tb |>
    as_tibble() |>
    write_csv("data/dm_glmm.csv")
  paste("data/dm_glmm.csv")
}


#' @title ratio vs total dry mass  plot
ratio_dm <- function(tree) {
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
      name = "Leaf punch diameter"
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
  ratio_mass
}
