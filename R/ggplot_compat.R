
# This line helper keeps ggplot2 line width arguments compatible across versions:
# - ggplot2 < 3.4.0 expects `size`
# - ggplot2 >= 3.4.0 expects `linewidth`
.gg_linewidth_arg <- function(width) {
  if (utils::packageVersion("ggplot2") >= "3.4.0")
    return(list(linewidth = width))
  list(size = width)
}

