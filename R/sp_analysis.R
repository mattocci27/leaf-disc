#' @title Leaf disc vs whole-leaf LMA (simple)
sma_point <- function(data) {
  data |>
    ggplot(aes(x = lma_disc, y = lma_leaf)) +
    geom_point(alpha = 0.8) +
    scale_x_log10(breaks = c(30, 50, 100, 200, 300)) +
    scale_y_log10(breaks = c(30, 50, 100, 200, 300)) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    geom_sma(se = TRUE, nboot = 2000) +
    xlab(expression("Leaf disc LMA "(g~m^{-2}))) +
    ylab(expression("Whole-leaf LMA "(g~m^{-2}))) +
    stat_cor(
      aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~"))
    ) +
    theme_bw()
}

#' @title Leaf disc vs whole-leaf LMA (simple)
sma_point_sep <- function(data) {
  my_col <- RColorBrewer::brewer.pal(4, "RdBu")
  data <- data |>
    mutate(size_gr = ifelse(location == "Yakushima", "1.0-cm", "0.6-cm")) |>
    ggplot(aes(x = lma_disc, y = lma_leaf, col = size_gr)) +
    geom_point(alpha = 0.8) +
    scale_x_log10(breaks = c(30, 50, 100, 200)) +
    scale_y_log10(breaks = c(30, 50, 100, 200, 300)) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    geom_sma(se = TRUE, nboot = 2000) +
    scale_color_manual(
      values = my_col[c(2, 4)],
      name = "Leaf punch diameter"
    ) +
    scale_fill_manual(
      values = my_col[c(2, 4)],
      name = "Leaf punch diameter"
    ) +
    xlab(expression("Leaf disc LMA "(g~m^{-2}))) +
    ylab(expression("Whole-leaf LMA "(g~m^{-2}))) +
    stat_cor(
      aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~")),
      show.legend = FALSE
    ) +
    theme_bw() +
    theme(
      legend.position = c(0.7, 0.15),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(0.2, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 9))
}

#' @title CV
cv_pool_point <- function(sp_cv, remove_outliers = FALSE) {
  if (remove_outliers) {
  sp_cv <- sp_cv |>
    filter(lma_leaf_cv < 0.09)
  }
  p <- sp_cv |>
    ggplot(aes(x = lma_disc_cv * 100, y = lma_leaf_cv * 100)) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    geom_point(alpha = 0.8) +
    scale_x_log10(breaks = c(0.001, 0.01, 0.05, 0.1, 0.15) * 100) +
    scale_y_log10(breaks = c(0.001, 0.01, 0.05, 0.1, 0.15) * 100) +
    geom_sma(se = TRUE, nboot = 2000) +
    ylab("CV of whole-leaf LMA (%)") +
    xlab("CV of leaf disc LMA (%)") +
    coord_fixed(xlim = c(0.005, 0.15) * 100, ylim = c(0.005, 0.15) * 100) +
    stat_cor(
       label.x.npc = 0.45,
       label.y.npc = 0.1,
       vjust = 1.5,
       aes(label = paste(..rr.label.., ..n.label.., sep = "~`,`~"))
     )  +
    theme_bw()
  if (remove_outliers) {
    p <- p +
      coord_fixed(xlim = c(0.005, 0.1) * 100, ylim = c(0.005, 0.1) * 100)
  }
  p
}

#' @title LMA and LD (species-level)
lma_ld_wrap_point <- function(sp_mean) {
  d_leaf <- sp_mean |>
    dplyr::select(c(species:data_contributor,
      lma = lma_leaf,
     # ldmc = ldmc_leaf,
      ld = ld_leaf
    )) |>
    pivot_longer(lma:ld, names_to = "trait", values_to = "whole_leaf") |>
    filter(location != "Yakushima" | trait != "ld") |>
    filter(location != "Yakushima" | trait != "lma")
  d_disc <- sp_mean |>
    dplyr::select(species, location, lma = lma_disc, ld = ld_disc) |>
    pivot_longer(lma:ld, names_to = "trait", values_to = "leaf_disc") |>
    filter(location != "Yakushima" | trait != "ld") |>
    filter(location != "Yakushima" | trait != "lma")
  d2 <- full_join(d_leaf, d_disc) |>
    mutate(trait = factor(trait, c("lma", "ld"))) |>
    mutate(trait_lab = factor(trait,
      labels = c(
        "LMA~(g~m^{-2})",
     #   "LDMC~(g~g^{-1})",
        "LD~(g~cm^{-3})"
      )
    ))
  p_all <- d2 |>
    filter(trait != "ldmc") |>
    ggplot(aes(x = leaf_disc, y = whole_leaf)) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    geom_point(alpha = 0.8) +
    scale_x_log10() +
    scale_y_log10() +
    geom_sma(se = TRUE, nboot = 2000) +
    facet_wrap(~trait_lab, scale = "free", labeller = label_parsed) +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~")
      )
    ) +
    xlab("Leaf disc") +
    ylab("Whole-leaf") +
    theme_bw()
  p_all
}

#' @title Plot petiole data
petiole_point <- function(yaku_sp) {
  pet1 <- ggplot(yaku_sp, aes(x = petiole_ratio, y = lma_leaf / lma_disc)) +
    geom_point(alpha = 0.8) +
    scale_x_log10(breaks = c(0.02, 0.05, 0.1, 0.2)) +
    # xlab(expression(Petiole/leaf~dry~mass~ratio(g~g^-1))) +
    xlab("Petiole / leaf dry mass ratio") +
    ylab("Whole-leaf / leaf disc LMA ratio") +
    geom_smooth(method = "lm") +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~")
      )
    )

  pet2 <- ggplot(yaku_sp, aes(x = petiole_ratio, y = la)) +
    geom_point(alpha = 0.8) +
    scale_x_log10(breaks = c(0.02, 0.05, 0.1, 0.2)) +
    scale_y_log10() +
    xlab("Petiole / leaf dry mass ratio") +
    ylab(expression(Leaf ~ area ~ (g ~ cm^2))) +
    geom_smooth(method = "lm") +
    stat_cor(
      aes(
        label = paste(..rr.label.., ..n.label.., sep = "~`,`~")
      )
    )

  pet1 + pet2 +
    plot_annotation(tag_levels = "a") &
    theme_bw() &
    theme(
      text = element_text(size = 10),
      plot.tag = element_text(face = "bold"))
}

#' @title Cross-validation for OLS models
#' @param tree tree or spcies mean data
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
