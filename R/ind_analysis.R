lalt_tree_grid <- function(tree) {
  tree |>
    mutate(la_gr2 = factor(la_gr2, levels = c("Small-leaved~individuals",
                           "Large-leaved~individuals") |> rev())) |>
    ggplot(aes(x = lma_disc, y = lma_leaf, col = dry_mass_disc)) +
    geom_point(alpha = 0.8) +
    scale_color_viridis_c(
      trans = "log10",
      name = "Total dry mass of \nleaf discs (g)"
    ) +
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
      aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~"), family = "Arial")
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Arial"),
      legend.position = "none",
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.3, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    )
}

lalt_tree_grid_dense <- function(tree) {
  p1 <- tree |>
      filter(ld_gr2 == "Nondense-leaved~individuals") |>
      lalt_tree_grid() +
      ggtitle("Nondense-leaved individuals") +
      theme(legend.position = c(0.88, 0.65))
  p2 <- tree |>
      filter(ld_gr2 == "Dense-leaved~individuals") |>
      lalt_tree_grid() +
      ggtitle("Dense-leaved individuals")
  p1 + p2 +
    plot_annotation(tag_levels = "a") &
    theme(
      text = element_text(family = "Arial"))
}

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
      legend.title = element_text(size = 7),
      text = element_text(family = "Arial")
    )
  ratio_mass
}

check_hdk <- function(tree) {
  fit <- lm(log(lma_leaf / lma_disc) ~ log(dry_mass_disc), tree)
  lmtest::bptest(fit)
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

#' @title Linear mixed model for log-transformed total dry mass of leaf discs
dm_glmm <- function(tree) {
  tree <- tree |>
    mutate(pch = ifelse(location == "Yakushima", "L1.0 cm", "S0.6 cm"))
#  fit <- lme4::lmer(log(dry_mass_disc) ~ scale(log(lt)) + scale(log(la)) + scale(log(ld_leaf)) + pch + (1| species), tree)

  fit <- nlme::lme(log(dry_mass_disc) ~
                   scale(log(ld_leaf)) +
                   scale(log(la)) +
                   scale(log(lt))  + pch , random = ~1| species, tree)

  s <- summary(fit)
  tb <- s$tTable |> round(3) |> signif(3)
  colnames(tb) <- c("Estimate", "SE", "DF","t-value", "*P* value")
  tb[, 5] <- ifelse(tb[, 5] < 0.001, "< 0.001", tb[,4])
  rownames(tb)[2] <- "log(Leaf tissue density)"
  rownames(tb)[3] <- "log(Leaf area)"
  rownames(tb)[4] <- "log(Leaf thickness)"
  rownames(tb)[5] <- "Small leaf punch"
  tb
}

create_cv_fit <- function(tree, k = 10, seed = 123)  {
  set.seed(seed)
  cv <- crossv_kfold(tree, k = k)
  y_bar <- mean(log(tree$lma_leaf))

  models1 <- map(cv$train,
    ~lm(log(lma_leaf) ~ log(lma_disc), data = .))
  models2 <- map(cv$train,
    ~lm(log(lma_leaf) ~ log(la) + log(lt),
        offset = log(lma_disc), data = .))
  models3  <- map(cv$train,
    ~lm(log(lma_leaf) ~ log(ld_leaf) + log(la) + log(lt),
        offset = log(lma_disc),
        data = .))
  models4 <- map(cv$train,
    ~lm(log(lma_leaf) ~ log(lma_disc) + log(la) + log(lt),
        data = .))

  fit1 <- lm(log(lma_leaf) ~ log(lma_disc), data = tree)
  fit2 <- lm(log(lma_leaf) ~ log(la) + log(lt),
        offset = log(lma_disc), data = tree)
  fit3 <- lm(log(lma_leaf) ~ log(ld_leaf) + log(la) + log(lt),
          offset = log(lma_disc), data = tree)
  fit4 <- lm(log(lma_leaf) ~ log(lma_disc) + log(la) + log(lt),
          data = tree)

  get_pred  <- function(model, test_data){
    data  <- as.data.frame(test_data)
    pred  <- add_predictions(data, model)
    return(pred)
  }

  pred <- map(list(models1, models2, models3, models4),
    \(x) map2_df(x, cv$test, get_pred, .id = "Run"))

  tmp_fun <- function(x) {
    x |>
    group_by(Run) |>
    summarise(
      MSE = mean((log(lma_leaf) - pred)^2),
      R2 = 1 - sum((log(lma_leaf) - pred)^2) / sum((log(lma_leaf) - y_bar)^2) )
  }

  mse_dat <- map(pred, tmp_fun)
  r2 <- map_dbl(mse_dat, \(x)mean(x$R2))
  mse_ <- map_dbl(mse_dat, \(x)mean(x$MSE))
  list(
    table = tibble(model = paste("fit", 1:4), r2 = r2, mse = mse_),
    fit1 = fit1,
    fit2 = fit2,
    fit3 = fit3,
    fit4 = fit4
    )
}
