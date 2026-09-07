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

# Plot window: how much tail to include, and how far from the median the window
# may reach in interquartile ranges. The IQR cap is what keeps heavy tails
# (Cauchy, chi-square with 1 df) from producing a window so wide the
# distribution is a flat line.
.dwindowAlpha <- 1e-4
.dwindowIQR   <- 6
# Never draw more bars, or more axis labels, than a 500px image can carry.
.dmaxBars     <- 500
.dmaxTicks    <- 25


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

    # P(X = x1) for a fractional x1 is genuinely 0 - X only takes whole values -
    # so R's "non-integer x" warning is noise in the jamovi log here. Only that
    # one is muffled; anything else still surfaces.
    density <- function(at) withCallingHandlers(spec$d(at, o), warning = function(w) {
        if (grepl("non-integer", conditionMessage(w))) invokeRestart("muffleWarning")
    })

    probability <- NA_real_
    if (!is.na(distMode)) {
        probability <- switch(distMode,
            is       = density(o$x1),
            lower    = spec$p(o$x1, o),
            # A discrete tail includes its endpoint, so P(X >= x1) carries the
            # + d(x1) term back; a continuous one does not.
            higher   = if (spec$discrete) 1 - spec$p(o$x1, o) + density(o$x1)
                       else               1 - spec$p(o$x1, o),
            interval = if (spec$discrete) spec$p(o$x2, o) - spec$p(o$x1, o) + density(o$x1)
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


# --- validation ------------------------------------------------------------

#' Check an option set before anything is computed.
#'
#' @return NULL if the options are usable, otherwise a message for the user.
#'
#' In v1.2.2 the only check ran at the very END of .run(), after a negative
#' probability had already been written into the Results table, and the
#' hypergeometric "corrected" out-of-range parameters instead of reporting them.
distributionValidate <- function(spec, o) {
    if (isTRUE(o$showDist) && identical(o$distMode, "interval") && o$x1 >= o$x2)
        return("x2 must be greater than x1")

    # Parameters that index a count have to be whole numbers: dbinom() and
    # dhyper() return NaN for fractional size/N/K/n rather than complaining.
    for (chk in spec$integerParams)
        if (!isTRUE(all.equal(o[[chk$opt]], round(o[[chk$opt]]))))
            return(paste0(chk$label, " must be a whole number"))

    if (!is.null(spec$validate)) {
        msg <- spec$validate(o)
        if (!is.null(msg)) return(msg)
    }
    NULL
}


# --- the curve -------------------------------------------------------------

#' The x range the plot covers.
#'
#' Discrete analyses get their integer support (capped, so a geometric with a
#' tiny success probability cannot ask for a hundred million bars). Continuous
#' analyses get a quantile window whose width is capped in interquartile ranges,
#' then widened to include whatever the user actually asked about.
#'
#' A quantile window cannot invert, which is what the old per-distribution
#' arithmetic did for a t with noncentrality below -1.
distributionWindow <- function(spec, o) {
    base <- if (!is.null(spec$window)) {
        spec$window(o)                              # documented overrides only
    } else if (spec$discrete) {
        s  <- spec$support(o)
        lo <- ceiling(s[1]); hi <- floor(s[2])
        if (!is.finite(hi) || hi - lo > .dmaxBars) {
            # Too many outcomes to draw. Fall back to the part of the support
            # that carries the mass - which for a Poisson with a large lambda is
            # nowhere near zero, so the window cannot simply start at the
            # support's lower end and count upwards.
            lo <- max(lo, floor(spec$q(.dwindowAlpha, o)))
            hi <- min(hi, ceiling(spec$q(1 - .dwindowAlpha, o)))
            if (!is.finite(hi) || hi - lo > .dmaxBars) hi <- lo + .dmaxBars
        }
        c(lo, hi)
    } else {
        med <- spec$q(0.5, o)
        iqr <- spec$q(0.75, o) - spec$q(0.25, o)
        c(max(spec$q(.dwindowAlpha, o),     med - .dwindowIQR * iqr),
          min(spec$q(1 - .dwindowAlpha, o), med + .dwindowIQR * iqr))
    }

    # Whatever the user asked about must be on the picture, or the shaded area
    # they are looking for is silently off-screen. This applies to the overrides
    # too - a normal's +/- 4 SD window is no use when x1 sits at 6 SD.
    marks <- c(o$x1, if (identical(o$distMode, "interval")) o$x2)
    if (isTRUE(o$showQuant) && !is.null(o$p)) {
        marks <- c(marks, if (identical(o$quantMode, "central")) {
            b <- centralBounds(o$p); c(spec$q(b[["lower"]], o), spec$q(b[["upper"]], o))
        } else spec$q(o$p, o))
    }
    marks <- marks[is.finite(marks)]
    w <- range(c(base, marks))

    # Widening for a mark must not push a discrete plot past the bar cap - and
    # when it would, the body of the distribution wins. Truncating the union
    # from its lower end instead would hand back a window of empty bars for,
    # say, a Poisson with lambda 1000 and x1 = 1.
    if (spec$discrete && w[2] - w[1] > .dmaxBars) {
        spare <- max(0, .dmaxBars - (base[2] - base[1]))
        w <- c(max(w[1], base[1] - spare), min(w[2], base[2] + spare))
        if (w[2] - w[1] > .dmaxBars) w <- c(base[1], base[1] + .dmaxBars)
    }
    w
}

#' The x-axis break positions.
#'
#' One rule instead of eight. Only the Poisson used to thin its labels, which is
#' why a binomial with size 100 drew 101 overlapping numbers.
distributionBreaks <- function(spec, o, window) {
    if (!is.null(spec$breaks)) return(spec$breaks(o, window))   # overrides only

    if (spec$discrete) {
        lo <- ceiling(window[1]); hi <- floor(window[2])
        if (hi - lo + 1 <= .dmaxTicks) return(lo:hi)
        return(unique(round(pretty(c(lo, hi), n = 12))))
    }
    pretty(window, n = 9)
}

#' The x grid and density used for the plot.
distributionCurve <- function(spec, o, window = NULL) {
    w <- if (is.null(window)) distributionWindow(spec, o) else window
    x <- if (spec$discrete) seq(w[1], w[2], by = 1)
         else               seq(w[1], w[2], length = 1000)
    data.frame(X = x, Prob = spec$d(x, o))
}

#' An upper limit for the y axis, or NA to leave it alone.
#'
#' The chi-square density with df < 2 and the F density with df1 < 2 both
#' diverge at zero, so the tallest point on the grid can be thousands of times
#' the height of the distribution's actual body - which flattens the curve, and
#' the shaded probability with it, onto the axis. Capping at the tallest point
#' from the lower quartile onwards restores the usual textbook view. The cap only
#' engages when the raw maximum really is out of scale, so every well-behaved
#' distribution is untouched.
distributionYCap <- function(spec, o, curve) {
    dens <- curve$Prob[is.finite(curve$Prob)]
    if (!length(dens)) return(NA_real_)
    raw <- max(dens)
    body <- curve$Prob[curve$X >= spec$q(0.25, o) & is.finite(curve$Prob)]
    if (!length(body)) return(NA_real_)
    cap <- max(body)
    if (is.finite(cap) && cap > 0 && raw > 2 * cap) cap * 1.05 else NA_real_
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

    # Validate first. v1.2.2 checked at the end of .run(), so an invalid
    # interval wrote a negative probability into the Results table and set the
    # plot state before rejecting.
    invalid <- distributionValidate(spec, o)
    if (!is.null(invalid)) jmvcore::reject(invalid)

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
    outputs <- self$results$Outputs
    outputs$setRow(rowNo = 1, values = values)

    # A number column cannot carry "undefined (df <= 1)", so say why in a
    # footnote rather than leaving the student with a bare NaN.
    if (!is.null(spec$momentNote)) {
        note <- spec$momentNote(o)
        for (col in names(note))
            if (!is.null(note[[col]]) && is.function(outputs$addFootnote))
                outputs$addFootnote(rowNo = 1, col, note[[col]])
    }

    # Plot state: a named list, not values smuggled into data-frame cells.
    window <- distributionWindow(spec, o)
    curve  <- distributionCurve(spec, o, window)
    shaded <- if (o$showDist) distributionShade(curve, o, o$distMode) else curve
    self$results$plot$setState(list(
        x = curve$X, density = curve$Prob, shaded = shaded$Prob,
        breaks = distributionBreaks(spec, o, window),
        yCap = if (spec$discrete) NA_real_ else distributionYCap(spec, o, curve),
        discrete = spec$discrete,
        showDist = o$showDist, showQuant = o$showQuant,
        markers = if (o$showQuant) distributionMarkers(spec, o, curve, window) else NULL))

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

    # coord_cartesian clips the view; scale_y_continuous(limits=) would drop the
    # rows instead and take the shaded area with them.
    if (!is.null(st$yCap) && is.finite(st$yCap))
        plot <- plot + ggplot2::coord_cartesian(ylim = c(0, st$yCap))

    # The unshaded part of the curve is blanked with NA on purpose, so ggplot2's
    # "Removed N rows containing non-finite values" is expected here and only
    # amounts to noise in the jamovi log. Anything else still surfaces.
    withCallingHandlers(print(plot), warning = function(w) {
        if (grepl("Removed .* (rows|row) containing", conditionMessage(w)))
            invokeRestart("muffleWarning")
    })
    TRUE
}
