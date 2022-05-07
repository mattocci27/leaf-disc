update_site_info <- function(site_info_raw, yml) {
  site <- fromJSON(site_info_raw)
  val <- yaml::yaml.load_file(yml)
  site2 <- site |>
    mutate(`Number of species` = c(
      val$TRF_sp,
      val$STF_sp,
      val$HDS_sp,
      val$Yaku_sp
    )) |>
    mutate(`Number of individuals` = c(
      val$TRF_ind,
      val$STF_ind,
      val$HDS_ind,
      val$Yaku_ind
    ))

  write_json(site2, "data/site_info.json", pretty = TRUE)
  # The return value must be a vector of paths to the files we write:
  paste("data/site_info.json")
}
