
# Compatibility helper for ggplot2 line width across versions:
# - ggplot2 < 3.4.0 expects `size` for line geoms
# - ggplot2 >= 3.4.0 expects `linewidth`
.gg_linewidth_arg <- function(width) {
  if (utils::packageVersion("ggplot2") >= "3.4.0")
    return(list(linewidth = width))
  list(size = width)
}

