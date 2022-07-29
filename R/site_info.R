update_site_info <- function(site_info_raw, yml) {
  site <- fromJSON(site_info_raw)
  val <- yaml::yaml.load_file(yml)
  site2 <- site |>
    mutate(`Number of species for species means` = c(
      val$TRF_sp,
      val$STF_sp,
      val$HDS_sp,
      val$Yaku_sp
    )) |>
    mutate(`Number of individuals for individuals means` = c(
      val$TRF_ind,
      val$STF_ind,
      val$HDS_ind,
      val$Yaku_ind
    )) |>
    mutate(`Number of species for CV` = c(
      val$TRF_sp_cv,
      val$STF_sp_cv,
      val$HDS_sp_cv,
      val$Yaku_sp_cv
    )) |>
    mutate(`Number of individuals for CV` = c(
      val$TRF_ind,
      val$STF_ind,
      # manually removed one sp
      val$HDS_ind - 1,
      val$Yaku_ind_cv
    ))

  write_json(site2, "data/site_info.json", pretty = TRUE)
  # The return value must be a vector of paths to the files we write:
  paste("data/site_info.json")
}

#' @title Produce site csv for docx
write_site_csv <- function(site_info) {
  site <- fromJSON(site_info)
  site2 <- t(site)
  site2[1,] <- t(site)[2,]
  site2[2,] <- t(site)[1,]
  colnames(site2) <- site2[1,]
  site2 <- site2[-1,]
  write.csv(site2, here("data", "site_info.csv"))
  paste(here("data", "site_info.csv"))
}
