

get_name <- function(x) deparse(substitute(x))

create_plot_dat <- function(plot_list) {
  plots <- sapply(plot_list, get_name) |> names()

  file_png <- str_replace_all(plots, "_plot", ".png")
  file_tiff <- str_replace_all(plots, "_plot", ".tiff")
  file_pdf <- str_replace_all(plots, "_plot", ".pdf")

  file_png <- paste0("figs/", file_png)
  file_tiff <- paste0("figs/", file_tiff)
  file_pdf <- paste0("figs/", file_pdf)

  plot_dat <- tibble(plots, file_png, file_tiff, file_pdf)
  plot_dat
}


save_tiff <- function(file_tiff, plots, width, height) {
  message("plot")
  get(plots) |> print()

  message("file")
  file_tiff |> print()

  # ggsave(file_tiff,
  #   plots,
  #   dpi = 300,
  #   width = width,
  #   height = height
  # )
}


#
#
# mcmapply(save_tiff, hoge, hoge2)
