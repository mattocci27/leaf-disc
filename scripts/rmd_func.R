page_break <- function(section = NULL){
  if(knitr:::pandoc_to() == "latex") {
    paste("\\newpage \n #", section)
  } else {paste("#####", section)}
}

# number tables
# from http://stackoverflow.com/a/18672268/1036500
# library(knitr)

#to = local({
#  i <- 0
#  function(x) {
#    i <<- i + 1
#    paste("<table><caption>",
#    '**Table ****', i, '**: ', x,
#    "</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>", sep = '')
#  }
#})
#knit_hooks$set(tab.cap = function(before, options, envir) {
#  if(before) # caption above tables
#    to(options$tab.cap)
#})
#default_output_hook = knit_hooks$get("output")
#knit_hooks$set(output = function(x, options) {
#  if (is.null(options$tab.cap) == F)
#    x
#  else
#    default_output_hook(x, options)
#})
#
# This is a knitr hook to enable figure and table captions
# number figures
# from http://stackoverflow.com/a/18672268/1036500
#tn <- local({
#  i <- 0
#  function(x) {
#    i <<- i + 1
#    paste("<figure><figcaption>", '**Figure ****', i, '**: ', x, "</figcaption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></figure><p>", sep = '')
#  }
#})
#knit_hooks$set(fig.cap = function(before, options, envir) {
#  if(!before)
#    tn(options$fig.cap)
#})
#default_output_hook = knit_hooks$get("output")
#knit_hooks$set(output = function(x, options) {
#  if (is.null(options$fig.cap) == F)
#    x
#  else
#    default_output_hook(x, options)
#})