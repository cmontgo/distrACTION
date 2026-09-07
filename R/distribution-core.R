# ---------------------------------------------------------------------------
# The shared engine behind all eight distribution analyses.
#
# Each R/<dist>.b.R is now a shim that names its spec (see distribution-specs.R)
# and delegates here. The arithmetic is deliberately separable from the jamovi
# plumbing: distributionResults() is a plain function of a spec name and an
# options list, so it can be tested without jmvcore. That is what
# tests/testthat/test-characterization.R exercises against the v1.2.2 fixture.
# ---------------------------------------------------------------------------

# Plot constants, previously repeated verbatim in all eight .plot() methods.
.dcolours    <- c(area = "#e0bc6b", quantile = "#7b9ee6", grey = "#9f9f9f")
.dpointSize  <- 0.000001
.dlineType   <- "dashed"
.dlineWidth  <- 1
.dtextSize   <- 16
.dtextSmall  <- 10
# A quantile marker shorter than 1/18th of the tallest bar is hard to see, so it
# gets stretched to that. Transcribed from v1.2.2.
.dminMarker  <- 18


# --- options ---------------------------------------------------------------

#' Collect an analysis's options into a plain list.
#'
#' The jamovi option names are shared across all eight analyses, so one reader
#' serves them all. dp3 is absent for most and comes back NULL.
distributionOptions <- function(self) {
    o <- list(
        dp1 = self$options$dp1,
        dp2 = if ("dp2" %in% names(self$options)) self$options$dp2 else NULL,
        dp3 = if ("dp3" %in% names(self$options)) self$options$dp3 else NULL,
        x1  = self$options$x1,
        x2  = self$options$x2,
        p   = self$options$p)
    o$showDist  <- isTRUE(self$options$DistributionFunction)
    o$showQuant <- isTRUE(self$options$QuantileFunction)
    o$distMode  <- self$options$DistributionFunctionType
    o$quantMode <- if ("QuantileFunctionType" %in% names(self$options))
                       self$options$QuantileFunctionType else "cumulative"
    o
}

#' The two ends of a central interval: (1-p)/2 either side.
centralBounds <- function(p) {
    lower <- (1 - p) / 2
    c(lower = lower, upper = lower + p)
}


# --- the numbers -----------------------------------------------------------

#' Compute one Results-table row.
#'
#' Pure: no jamovi objects, no side effects. This is the function the
#' characterization suite pins against the v1.2.2 fixture.
#'
#' @param dist      spec name, e.g. "normal"
#' @param o         options list with dp1..dp3, x1, x2, p
#' @param distMode  "is" | "lower" | "higher" | "interval", or NA to skip
#' @param quantMode "cumulative" | "central", or NA to skip
#' @return list(probability, quantile, quantileLower, quantileUpper, mean, sd)
distributionResults <- function(dist, o, distMode = NA, quantMode = NA) {
    spec <- if (is.character(dist)) distSpec(dist) else dist
    if (!is.null(spec$clamp)) o <- spec$clamp(o)

    probability <- NA_real_
    if (!is.na(distMode)) {
        probability <- switch(distMode,
            is       = spec$d(o$x1, o),
            lower    = spec$p(o$x1, o),
            # A discrete tail includes its endpoint, so P(X >= x1) carries the
            # + d(x1) term back; a continuous one does not.
            higher   = if (spec$discrete) 1 - spec$p(o$x1, o) + spec$d(o$x1, o)
                       else               1 - spec$p(o$x1, o),
            interval = if (spec$discrete) spec$p(o$x2, o) - spec$p(o$x1, o) + spec$d(o$x1, o)
                       else               spec$p(o$x2, o) - spec$p(o$x1, o),
            stop("unknown distribution mode: ", distMode))
    }

    quantile <- quantileLower <- quantileUpper <- NA_real_
    if (!is.na(quantMode)) {
        if (identical(quantMode, "cumulative")) {
            quantile <- spec$q(o$p, o)
        } else {
            b <- centralBounds(o$p)
            quantileLower <- spec$q(b[["lower"]], o)
            quantileUpper <- spec$q(b[["upper"]], o)
        }
    }

    m <- spec$moments(o)
    list(probability = probability, quantile = quantile,
         quantileLower = quantileLower, quantileUpper = quantileUpper,
         mean = m$mean, sd = m$sd)
}


# --- the curve -------------------------------------------------------------

#' The x grid and density used for the plot.
#'
#' Continuous specs give a point count; discrete specs return NA from
#' gridSize() and get their integer support instead.
distributionCurve <- function(spec, o) {
    w <- spec$window(o)
    n <- spec$gridSize(o)
    x <- if (is.na(n)) seq(w[1], w[2], by = 1) else seq(w[1], w[2], length = n)
    data.frame(X = x, Prob = spec$d(x, o))
}

#' Blank out the part of the curve that falls outside the requested region, so
#' that geom_area / geom_col shades only what was asked for.
distributionShade <- function(curve, o, distMode) {
    keep <- switch(distMode,
        is       = curve$X == o$x1,
        lower    = curve$X <= o$x1,
        higher   = curve$X >= o$x1,
        interval = curve$X >= o$x1 & curve$X <= o$x2,
        rep(TRUE, nrow(curve)))
    curve$Prob[!keep] <- NA
    curve$X[!keep]    <- NA
    curve
}

#' Where the dashed quantile markers go, how tall they are, and whether they
#' fall outside the plotted window.
distributionMarkers <- function(spec, o, curve, window) {
    peak <- suppressWarnings(max(curve$Prob, na.rm = TRUE))
    if (!is.finite(peak)) peak <- 1

    stretch <- function(h) if (is.finite(h) && h * .dminMarker < peak) peak / .dminMarker else h

    if (identical(o$quantMode, "cumulative")) {
        at  <- spec$q(o$p, o)
        pos <- c(lower = at, upper = at)
    } else {
        b   <- centralBounds(o$p)
        pos <- c(lower = spec$q(b[["lower"]], o), upper = spec$q(b[["upper"]], o))
    }
    len <- c(lower = stretch(spec$d(pos[["lower"]], o)),
             upper = stretch(spec$d(pos[["upper"]], o)))
    if (identical(o$quantMode, "cumulative")) len[["lower"]] <- len[["upper"]]

    alpha <- c(lower = 1, upper = 1)
    label <- "Quantile"
    size  <- .dtextSize

    below <- pos < window[1]
    above <- pos > window[2]
    if (identical(o$quantMode, "cumulative")) {
        if (above[["upper"]] || below[["upper"]]) {
            label <- "Quantile out of range"; alpha[] <- 0; size <- .dtextSmall
            pos[] <- if (above[["upper"]]) window[2] else window[1]
        }
    } else {
        if (above[["upper"]] && below[["lower"]]) {
            label <- "Quantile out of range"; alpha[] <- 0; size <- .dtextSmall
            pos[["upper"]] <- window[2]; pos[["lower"]] <- window[1]
        } else if (above[["upper"]]) {
            label <- "(Upper) Quantile out of range"
            alpha[["upper"]] <- 0; size <- .dtextSmall; pos[["upper"]] <- window[2]
        } else if (below[["lower"]]) {
            label <- "(Lower) Quantile out of range"
            alpha[["lower"]] <- 0; size <- .dtextSmall; pos[["lower"]] <- window[1]
        }
    }
    list(pos = pos, len = len, alpha = alpha, label = label, textSize = size)
}


# --- the jamovi generics ---------------------------------------------------

#' Generic .run() for every distribution analysis.
runDistribution <- function(self, spec) {
    o <- distributionOptions(self)
    if (!is.null(spec$clamp)) o <- spec$clamp(o)

    # Input table: parameters down the left, the two function summaries beside.
    lines <- spec$paramLines(o)
    distLabel <- if (!o$showDist) "" else switch(o$distMode,
        is       = "Mode: P(X = x1)",
        lower    = "Mode: P(X ≤ x1)",
        higher   = "Mode: P(X ≥ x1)",
        interval = paste0("Mode: x2 = ", o$x2), "")
    quantLabel <- if (!o$showQuant || length(spec$quantModes) < 2) "" else
        switch(o$quantMode, cumulative = "cumulative mode", central = "central mode", "")

    inputs <- self$results$Inputs
    for (i in seq_along(lines))
        inputs$setRow(rowNo = i, values = list(
            ParametersColumn         = lines[i],
            DistributionFunctionColumn = if (i == 1) paste0("x1 = ", o$x1)
                                         else if (i == 2) distLabel else "",
            QuantileFunctionColumn   = if (i == 1) paste0("p = ", o$p)
                                       else if (i == 2) quantLabel else ""))

    # Results table.
    res <- distributionResults(spec, o,
                               distMode  = if (o$showDist)  o$distMode  else NA,
                               quantMode = if (o$showQuant) o$quantMode else NA)

    blank <- function(v) if (is.na(v)) "" else v
    values <- list(
        DistributionResultColumn = blank(res$probability),
        MeanColumn = res$mean,
        SDColumn   = res$sd)
    if ("central" %in% spec$quantModes) {
        # cumulative writes x1; central writes the pair
        values$QuantileResultColumn      <- blank(res$quantile)
        values$QuantileLowerResultColumn <- blank(res$quantileLower)
        values$QuantileUpperResultColumn <- blank(res$quantileUpper)
    } else {
        values$QuantileResultColumn <- blank(res$quantile)
    }
    self$results$Outputs$setRow(rowNo = 1, values = values)

    # Plot state: a named list, not values smuggled into data-frame cells.
    window <- spec$window(o)
    curve  <- distributionCurve(spec, o)
    shaded <- if (o$showDist) distributionShade(curve, o, o$distMode) else curve
    self$results$plot$setState(list(
        x = curve$X, density = curve$Prob, shaded = shaded$Prob,
        breaks = spec$breaks(o, window),
        discrete = spec$discrete,
        showDist = o$showDist, showQuant = o$showQuant,
        markers = if (o$showQuant) distributionMarkers(spec, o, curve, window) else NULL))

    if (o$showDist && identical(o$distMode, "interval") && o$x1 >= o$x2)
        jmvcore::reject("x2 must be greater than x1")

    invisible(NULL)
}

#' Generic .plot() for every distribution analysis.
plotDistribution <- function(self, image, spec) {
    st <- image$state
    if (is.null(st)) return(FALSE)

    d <- data.frame(X = st$x, Prob = st$density, CurveProb = st$shaded)

    plot <- ggplot2::ggplot(d, ggplot2::aes(x = X, y = Prob)) +
        ggplot2::xlab("") + ggplot2::ylab("") +
        ggplot2::scale_x_continuous(breaks = st$breaks)

    # Discrete analyses draw the whole distribution in grey behind the shading.
    if (st$discrete)
        plot <- plot + ggplot2::geom_col(ggplot2::aes(x = X, y = Prob), fill = "grey")

    if (st$showDist) {
        shade <- if (st$discrete) ggplot2::geom_col else ggplot2::geom_area
        plot <- plot +
            shade(mapping = ggplot2::aes(x = X, y = CurveProb, fill = " P (Area)")) +
            ggplot2::scale_fill_manual(values = unname(.dcolours))
    }

    if (!is.null(st$markers)) {
        m <- st$markers
        seg <- function(which) do.call(ggplot2::geom_segment, c(list(
            ggplot2::aes(linetype = m$label),
            x = m$pos[[which]], y = 0,
            xend = m$pos[[which]], yend = m$len[[which]],
            colour = .dcolours[["quantile"]], alpha = m$alpha[[which]]),
            .gg_linewidth_arg(.dlineWidth)))
        plot <- plot + seg("lower") + seg("upper") +
            ggplot2::scale_linetype_manual(values = .dlineType)
    }

    # Continuous analyses draw the curve itself on top.
    if (!st$discrete)
        plot <- plot +
            ggplot2::geom_point(size = .dpointSize, color = .dcolours[["area"]]) +
            ggplot2::geom_line()

    textSize <- if (is.null(st$markers)) .dtextSize else st$markers$textSize
    plot <- plot + ggplot2::theme_classic() +
        ggplot2::theme(legend.text = ggplot2::element_text(size = textSize),
                       legend.title = ggplot2::element_blank())

    print(plot)
    TRUE
}
