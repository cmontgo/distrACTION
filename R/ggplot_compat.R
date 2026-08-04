
# ggplot2 4.0.0 only supports `linewidth` for line-based geoms.
# The `size` aesthetic for lines was fully removed.
.gg_linewidth_arg <- function(width) {
  list(linewidth = width)
}

