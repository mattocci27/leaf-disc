# To save ggplot figures
my_ggsave <- function(plot, filename, height = 11.4, width = 11.4, dpi = 200, units = "cm", ...) {
  ggsave(
    filename = filename,
    plot = plot,
    height = height,
    width = width,
    units = units,
    dpi = dpi,
    ...
  )
}

# for renv
library(languageserver)
# theme
# library(tidyverse)

# theme_set(theme_bw())
# theme_update(text = element_text(family = "Arial"))
