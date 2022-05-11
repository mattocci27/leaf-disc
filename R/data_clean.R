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
    mutate(ldmc_disc = NA) |>
    mutate(lt_disc = NA) |>
    mutate(petiole_ratio = petiole_dw / scanned_leaf_dw)
  yaku
}

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

clean_lma_la <- function(lma_la) {
  d2 <- read_csv(lma_la) |>
    clean_names(abbreviations = c("LMA", "LD", "LDMC", "LT"))

  d2 <- d2 |>
    filter(location != "Garden_palm") |>
    filter(location != "Ailao_understory") |>
    filter(location != "Mengla_bubeng") |>
    filter(location != "Yuanjiang_savan") |>
    filter(species != "Dendrobium primulinum Lindl.") |>
    filter(species != "Dendrobium nobile Lindl.")
  d2
}

data_clean <- function(d, d2, d3) {
  sp_dat <- d |>
    group_by(species, location) |>
    summarise_at(
      .vars = vars(
        lma_disc,
        lma_leaf,
        ld_disc,
        ld_leaf,
        ldmc_disc,
        ldmc_leaf,
        lt_leaf,
        lt_disc,
        leaf_area_leaf
      ),
      .funs = \(x)mean(x, na.rm = TRUE)
    )

  sp_list <- d |>
    dplyr::select(species:data_contributor) |>
    unique()

  new_dat <- full_join(sp_list, sp_dat, by = c("species", "location")) |>
    rename(la_leaf = leaf_area_leaf) |>
    mutate(garden = "NO")

  d2 <- d2 |>
    filter(location != "Garden_palm") |>
    filter(location != "Ailao_understory") |>
    filter(location != "Mengla_bubeng") |>
    filter(location != "Yuanjiang_savan") |>
    filter(species != "Dendrobium primulinum Lindl.") |>
    filter(species != "Dendrobium nobile Lindl.")


  new_dat2 <- full_join(new_dat, d2,
    by = c("species", "family", "location", "biomes", "lma_disc",
      "garden",
      "lma_leaf",
      "la_leaf" = "la",
      "lt_leaf" = "lt",
      "lt_disc",
      "ld_disc",
      "ld_leaf", "ldmc_disc", "ldmc_leaf",
      "leaf_habit",
      "growth_form",
      "data_contributor"
    )
  )

  yaku_sp <- d3 |>
    group_by(species) |>
    summarize(
      family = apg_family,
      growth_form,
      eve_dec,
      location,
      biomes,
      garden,
      mat,
      map,
      data_contributor,
      lma_disc = mean(lma_disc, na.rm = TRUE),
      lma_leaf = mean(lma_leaf, na.rm = TRUE),
      ldmc_disc = ldmc_disc,
      ldmc_leaf = mean(ldmc_leaf, na.rm = TRUE),
      ld_disc = mean(ld_disc, na.rm = TRUE),
      ld_leaf = mean(ld_leaf, na.rm = TRUE),
      lt = mean(lt, na.rm = TRUE),
      lt_disc = mean(lt_disc, na.rm = TRUE),
      la = mean(la, na.rm = TRUE)
    ) |>
    unique()

  new_dat3 <- full_join(new_dat2, yaku_sp,
    by = c("species", "family", "location", "biomes",
      "lma_disc", "lma_leaf", "ld_leaf", "ldmc_disc",
      "growth_form",
      "leaf_habit" = "eve_dec",
      "data_contributor",
      "ld_disc",
      "lt_leaf" = "lt",
      "la_leaf" = "la",
      "ldmc_leaf", "garden", "mat", "map",
      "lt_disc")) |>
    mutate(leaf_type = case_when(
      leaf_type == "single" ~ "S",
      leaf_type == "compound" ~ "C"
    ))

   new_dat3 |>
      write_csv("data/full_data.csv")
   paste("data/full_data.csv")
}

data_clean_cv <- function(d, d3) {
  sp_dat <- d |>
    group_by(species, location) |>
    summarise_at(
      .vars = vars(
        lma_disc,
        lma_leaf,
        ld_disc,
        ld_leaf,
        lt_leaf,
        lt_disc,
        leaf_area_leaf
      ),
      .funs = cv4
    )

  sp_list <- d |>
    dplyr::select(species:data_contributor) |>
    unique()

  new_dat <- full_join(sp_list, sp_dat, by = c("species", "location")) |>
    rename(la_leaf = leaf_area_leaf) |>
    mutate(garden = "NO")

  yaku_sp_n <- d3 |>
    group_by(species, location) |>
    summarize(n = n()) |>
    filter(n >= 5)

  yaku_sp <- d3 |>
    group_by(species) |>
    summarize(
      family = apg_family,
      growth_form,
      eve_dec,
      location,
      biomes,
      garden,
      # mat,
      # map,
      data_contributor,
      lma_disc = cv4(lma_disc),
      lma_leaf = cv4(lma_leaf),
      # ldmc_disc = ldmc_disc,
      # ldmc_leaf = cv4(ldmc_leaf, na.rm = TRUE),
      ld_disc = cv4(ld_disc),
      ld_leaf = cv4(ld_leaf),
      lt = cv4(lt),
      lt_disc = cv4(lt_disc),
      la = cv4(la)
    ) |>
    unique()

  yaku_sp <- right_join(yaku_sp, yaku_sp_n, by = c("species", "location"))

  new_dat3 <- full_join(new_dat, yaku_sp,
    by = c("species", "family", "location", "biomes",
      "lma_disc", "lma_leaf", "ld_leaf",
      "growth_form",
      "leaf_habit" = "eve_dec",
      "data_contributor",
      "ld_disc",
      "lt_leaf" = "lt",
      "la_leaf" = "la",
      "garden",
      "lt_disc")) |>
    mutate(leaf_type = case_when(
      leaf_type == "single" ~ "S",
      leaf_type == "compound" ~ "C"
    ))

  new_dat3 |>
    write_csv("data/full_data_cv.csv")
  paste("data/full_data_cv.csv")
}

data_clean_tree <- function(d, d3) {
  d_tree <- d |>
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
      "ld_disc",
      "ld_leaf",
      "lt_leaf",
      "lt_disc",
      "la_leaf" = leaf_area_leaf
    )

  yaku_tree <- d3 |>
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
    )
  )

  tree_data |> write_csv("data/tree_data.csv")
  paste("data/tree_data.csv")
}


sp_dat_mean <- function(full_data_csv){
  d <- full_data_csv |>
    filter(!is.na(lma_disc)) |>
    filter(!is.na(lma_leaf)) |>
    filter(!is.na(lt_leaf)) |>
    rename(lt = lt_leaf) |>
    rename(la = la_leaf) |>
    filter(location %in% c("Ailao_understory", "Mengla_Bubeng", "Yuanjiang_Savanna", "Yakushima")) |>
    #filter(Location != "Yakushima") |>
    mutate(lma_ratio = lma_leaf / lma_disc) |>
    mutate(ld_ratio = ld_leaf / ld_disc)

  la1 <- quantile(d$la, 0.25, na.rm = TRUE)
  la2 <- quantile(d$la, 0.5, na.rm = TRUE)
  la3 <- quantile(d$la, 0.75, na.rm = TRUE)
  lt1 <- quantile(d$lt, 0.25, na.rm = TRUE)
  lt2 <- quantile(d$lt, 0.5, na.rm = TRUE)
  lt3 <- quantile(d$lt, 0.75, na.rm = TRUE)

  la_mid <- median(d$la)
  lt_mid <- median(d$lt, na.rm = TRUE)
  ld_mid <- median(d$ld_leaf, na.rm = TRUE)

  d <- d |>
    mutate(lt_gr = case_when(
      lt < lt1 ~ "Very~thin",
      lt < lt2 ~ "Thin",
      lt < lt3 ~ "Thick",
      TRUE ~ "Very~thick")) |>
    mutate(lt_gr = factor(lt_gr, levels = c("Very~thin", "Thin", "Thick",
                                            "Very~thick"))) |>
    mutate(la_gr = case_when(
      la < la1 ~ "Very~small",
      la < la2 ~ "Small",
      la < la3 ~ "Large",
      TRUE ~ "Very~large")) |>
    mutate(la_gr = factor(la_gr, levels = c("Very~small", "Small", "Large",
                                            "Very~large"))) |>
    mutate(lalt_gr = case_when(
     la < la_mid & lt < lt_mid ~ "Thin~Small",
     la < la_mid & lt >= lt_mid ~ "Thick~Small",
     la >= la_mid & lt < lt_mid ~ "Thin~Large",
     la >= la_mid & lt >= lt_mid ~ "Thick~Large",
     TRUE ~ "aa"
                            )) |>
    mutate(la_gr2 = ifelse(la < la_mid, "Small-leaved~species",
                           "Large-leaved~species")) |>
    mutate(lt_gr2 = ifelse(lt < lt_mid, "Thin-leaved~species",
                           "Thick-leaved~species")) |>
    mutate(ld_gr = ifelse(ld_leaf < ld_mid, "Dense",
                           "Nondense")) |>
    mutate(ld_gr2 = ifelse(ld_leaf < ld_mid, "Dense-leaved~species",
                           "Nondense-leaved~species")) |>
    mutate(la_gr2 = factor(la_gr2, levels = c("Small-leaved~species",
                           "Large-leaved~species"))) |>
    mutate(lt_gr2 = factor(lt_gr2, levels = c("Thin-leaved~species",
                           "Thick-leaved~species"))) |>
    mutate(ld_gr2 = factor(ld_gr2, levels = c("Nondense-leaved~species",
                           "Dense-leaved~species"))) |>
    mutate(ldlalt_gr = paste0(ld_gr, "~", lalt_gr))

  d
}

sp_dat_cv <- function(full_data_cv_csv) {
  full_data_cv_csv |>
  dplyr::select(species, location,
                lma_leaf_cv = lma_leaf, lma_disc_cv = lma_disc,
                ld_leaf_cv = ld_leaf, ld_disc_cv = ld_disc) |>
  filter(!is.na(lma_leaf_cv)) |>
  filter(!is.na(lma_disc_cv))
}

tree_dat_clean <- function(tree_data_csv) {
  tree <- tree_data_csv |>
    filter(!is.na(lma_disc)) |>
    filter(!is.na(lma_leaf)) |>
    filter(!is.na(lt_leaf)) |>
    rename(lt = lt_leaf) |>
    rename(la = la_leaf) |>
    mutate(dry_mass_disc2 = case_when(
     location == "Yakushima" ~ lma_disc * 1.57 * 10^-4 / 2,
     TRUE ~ lma_disc * 0.889 * 10^-4 / 3
                  )) |>
    mutate(dry_mass_disc = case_when(
     location == "Yakushima" ~ lma_disc * 1.57 * 10^-4,
     TRUE ~ lma_disc * 0.889 * 10^-4
                  ))

  la1_tree <- quantile(tree$la, 0.25, na.rm = TRUE)
  la2_tree <- quantile(tree$la, 0.5, na.rm = TRUE)
  la3_tree <- quantile(tree$la, 0.75, na.rm = TRUE)
  lt1_tree <- quantile(tree$lt, 0.25, na.rm = TRUE)
  lt2_tree <- quantile(tree$lt, 0.5, na.rm = TRUE)
  lt3_tree <- quantile(tree$lt, 0.75, na.rm = TRUE)

  la_mid_tree <- median(tree$la, na.rm = TRUE)
  lt_mid_tree <- median(tree$lt, na.rm = TRUE)
  ld_mid_tree <- median(tree$ld_leaf, na.rm = TRUE)

  tree <- tree |>
    mutate(lt_gr = case_when(
      lt < lt1_tree ~ "Very~thin",
      lt < lt2_tree ~ "Thin",
      lt < lt3_tree ~ "Thick",
      TRUE ~ "Very~thick")) |>
    mutate(lt_gr = factor(lt_gr, levels = c("Very~thin", "Thin", "Thick",
                                            "Very~thick"))) |>
    mutate(la_gr = case_when(
      la < la1_tree ~ "Very~small",
      la < la2_tree ~ "Small",
      la < la3_tree ~ "Large",
      TRUE ~ "Very~large")) |>
    mutate(la_gr = factor(la_gr, levels = c("Very~small", "Small", "Large",
                                            "Very~large"))) |>
    mutate(lalt_gr = case_when(
     la < la_mid_tree & lt < lt_mid_tree ~ "Thin~Small",
     la < la_mid_tree & lt >= lt_mid_tree ~ "Thick~Small",
     la >= la_mid_tree & lt < lt_mid_tree ~ "Thin~Large",
     la >= la_mid_tree & lt >= lt_mid_tree ~ "Thick~Large",
     TRUE ~ "aa"
                            )) |>
    mutate(la_gr2 = ifelse(la < la_mid_tree, "Small-leaved~individuals",
                           "Large-leaved~individuals")) |>
    mutate(lt_gr2 = ifelse(lt < lt_mid_tree, "Thin-leaved~individuals",
                           "Thick-leaved~individuals")) |>
    mutate(ld_gr = ifelse(ld_leaf < ld_mid_tree, "Dense",
                           "Nondense")) |>
    mutate(ld_gr2 = ifelse(ld_leaf < ld_mid_tree, "Dense-leaved~individuals",
                           "Nondense-leaved~individuals")) |>
    mutate(la_gr2 = factor(la_gr2, levels = c("Small-leaved~individuals",
                           "Large-leaved~individuals"))) |>
    mutate(lt_gr2 = factor(lt_gr2, levels = c("Thin-leaved~individuals",
                           "Thick-leaved~individuals"))) |>
    mutate(ld_gr2 = factor(ld_gr2, levels = c("Nondense-leaved~individuals",
                           "Dense-leaved~individuals"))) |>
    mutate(ldlalt_gr = paste0(ld_gr, "~", lalt_gr))

  tree
}

create_yaku_sp <- function(lma_yaku_re) {
  lma_yaku_re |>
    group_by(species) |>
    summarize_at(
                 .vars = vars(
               lma_disc,
               lma_leaf,
               ldmc_disc,
               ldmc_leaf,
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
