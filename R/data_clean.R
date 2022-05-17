#' @title CV
cv <- \(x, na.rm = TRUE) sd(log(x), na.rm = TRUE) / mean(log(x), na.rm = TRUE)

#' @title Bao's CV with log-transformation
cv4 <- function(x, log = TRUE) {
  x <- x[!is.na(x)]
  if (log) x <- log(x)
  n <- length(x)
  mu <- mean(x)
  sig <- sd(x)
  cv1 <- sig / mu
  gamma1 <- sum(((x - mu) / sig)^3) / n
  gamma2 <- sum(((x - mu) / sig)^4) / n
  cv4 <- cv1 - cv1^3 / n + cv1 / (4 * n)
        + cv1^2 * gamma1 / (2 * n) + cv1 * gamma2 / (8 * n)
  cv4
}

#' @title Clean names in Yakushima data
clean_lma_yaku <- function(lma_yaku) {
  d3 <- read_csv(lma_yaku) |>
    clean_names(abbreviations = c("LMA", "LD", "LDMC", "LT"))
  yaku <- d3 |>
    filter(growthform != "H") |>
    mutate(location = "Yakushima") |>
    mutate(biomes = "ST2") |>
    mutate(garden = "NO") |>
    mutate(mat = NA) |>
    mutate(map = NA) |>
    mutate(contributor = "Onoda Yusuke") |>
    mutate(growthform = ifelse(growthform == "W", "Trees", growthform)) |>
    rename(
      lt = lamina_thickness,
      la = la_mean_leaf_let,
      data_contributor = contributor,
      growth_form = growthform,
      lma_leaf = whole_leaf_lma,
      lma_disc = ld_lma,
      ldmc_leaf = ldmc,
      ld_disc = td) |>
    mutate(ld_leaf = lma_leaf / lt * 10^-3) %>%
    mutate(ld_disc = NA) |>
    mutate(ldmc_disc = NA) |>
    mutate(lt_disc = NA) |>
    mutate(petiole_ratio = petiole_dw / scanned_leaf_dw)
  yaku
}

#' @title Clean names in Yaunnan data
clean_lma_raw <- function(lma_raw) {
  d <- read_csv(lma_raw) |>
    clean_names(abbreviations = c("LMA", "LD", "LDMC", "LT"))
  d |>
    mutate(location = ifelse(location == "Ailao_underground", "Ailao_understory", location)) |>
    filter(!is.na(species)) |>
    filter(!is.na(lma_disc)) |>
    # 0.6 cm: 0.3^2 * pi * 3 disc = 0.848
    mutate(lma_disc = lma_disc * 0.899 / 0.848) |>
    mutate(species = str_replace_all(species, "\\?", " ")) |>
    mutate(id2 = paste(id, rep, sep = "-"))
}

#' @title Create tree-level dataset
data_clean_tree <- function(lma_raw_re, lma_yaku_re) {
  d_tree <- lma_raw_re |>
    dplyr::select(
      "id" = id2,
      "species",
      "family",
      "growth_form",
      "leaf_habit",
      "leaf_type",
      "location",
      "biomes",
      "data_contributor",
      "lma_disc",
      "lma_leaf",
      # "ldmc_disc",
      # "ldmc_leaf",
      "ld_disc",
      "ld_leaf",
      "lt_leaf",
      "lt_disc",
      "la_leaf" = leaf_area_leaf
    )

  yaku_tree <- lma_yaku_re |>
    dplyr::select(
      "id" = no,
      "species",
      "family" = apg_family,
      "growth_form",
      "leaf_habit" = "eve_dec",
      "location",
      "biomes",
      "data_contributor",
      "lma_disc",
      "lma_leaf",
      "ld_disc",
      "ld_leaf",
      "lt_leaf" = lt,
      "la_leaf" = la
    )

  tree_data <- full_join(d_tree, yaku_tree,
    by = c(
      "id", "species", "family", "growth_form", "leaf_habit", "location",
      "data_contributor",
      "biomes", "lma_disc", "lma_leaf", "ld_disc", "ld_leaf", "lt_leaf", "la_leaf"
    )) |>
    rename(la = la_leaf) |>
    rename(lt = lt_leaf) |>
    # we only use samples that have both LMAw and LMAd
    # filter(!is.na(lma_disc)) |>
    # filter(!is.na(lma_leaf)) |>
    # filter(!is.na(lt)) |>
    mutate(dry_mass_disc2 = case_when(
     location == "Yakushima" ~ lma_disc * 1.57 * 10^-4 / 2,
    # disc size for lma_disc in lma_raw_re is 0.899
     TRUE ~ lma_disc * 0.889 * 10^-4 / 3
                  )) |>
    # total dry mass
    mutate(dry_mass_disc = case_when(
     location == "Yakushima" ~ lma_disc * 1.57 * 10^-4,
     # disc size for lma_disc in lma_raw_re is 0.899
     TRUE ~ lma_disc * 0.889 * 10^-4
                  ))

  tree_data |> write_csv("data/tree_data.csv")
  paste("data/tree_data.csv")
}

#' @title Create species means from the tree data
data_clean_mean <- function(tree) {
  sp_dat <- tree |>
    # to calculate means, we use samples that have both LMAw and LMAd
    filter(!is.na(lma_disc)) |>
    filter(!is.na(lma_leaf)) |>
    filter(!is.na(lt)) |>
    group_by(species, family, growth_form, leaf_habit, leaf_type,
    location, biomes, data_contributor) |>
    summarise_at(
      .vars = vars(
        lma_disc,
        lma_leaf,
        ld_disc,
        ld_leaf,
        lt,
        lt_disc,
        la
      ),
      .funs = \(x)mean(x, na.rm = TRUE)
    ) |>
    arrange(location) |>
    mutate(lma_ratio = lma_leaf / lma_disc) |>
    mutate(ld_ratio = ld_leaf / ld_disc)

   sp_dat |>
      write_csv("data/sp_mean.csv")
   paste("data/sp_mean.csv")
}

#' @title Create cv from the tree data
data_clean_cv <- function(tree) {
  # sample with n > 5
  tree_n <- tree |>
    group_by(species, location) |>
    summarize(n = n()) |>
    filter(n >= 5) |>
    ungroup()
  tree2 <- left_join(tree_n, tree, by = c("species", "location"))

  sp_dat <- tree2 |>
    # to calculate cv, we use samples that have both LMAw and LMAd
    filter(!is.na(lma_disc)) |>
    filter(!is.na(lma_leaf)) |>
    # filter(!is.na(lt)) |>
    group_by(species, family, growth_form, leaf_habit, leaf_type,
    location, biomes, data_contributor, n) |>
    rename(lma_leaf_cv = lma_leaf) |>
    rename(lma_disc_cv = lma_disc) |>
    summarise_at(
      .vars = vars(
        lma_disc_cv,
        lma_leaf_cv
        # ld_disc,
        # ld_leaf,
        # lt,
        # lt_disc,
        # la
      ),
      .funs = cv4
    ) |>
    arrange(location)

   sp_dat |>
      write_csv("data/sp_cv.csv")
   paste("data/sp_cv.csv")
}

#' @title clean petiole data
create_yaku_sp <- function(lma_yaku_re) {
  lma_yaku_re |>
    group_by(species) |>
    summarize_at(
                 .vars = vars(
               lma_disc,
               lma_leaf,
               ld_disc,
               ld_leaf,
               lt,
               lt_disc,
               la,
               petiole_ratio
                 ),
      .funs =  \(x)mean(x, na.rm = TRUE)) |>
    filter(petiole_ratio > 0) |>
    filter(!is.na(lma_disc))
}
