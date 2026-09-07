# ---------------------------------------------------------------------------
# One spec per distribution.
#
# Everything the eight analyses do identically lives in distribution-core.R.
# This file holds only what actually differs: the density/cdf/quantile calls,
# whether the endpoint belongs to the tail, the closed-form moments, the labels
# for the Input table, and the modes the analysis offers.
#
# The plot window, grid and axis-break entries are transcribed as-is from
# v1.2.2 so the extraction is behaviour-preserving. They are replaced by shared
# rules in a later commit; see tests/testthat/test-known-issues.R for what is
# wrong with them.
# ---------------------------------------------------------------------------

#' Build one distribution spec.
#'
#' @param discrete   TRUE if the endpoint x1 belongs to the higher/interval tail
#' @param d,p,q      density, cdf and quantile closures, each taking (value, options)
#' @param moments    function(options) -> list(mean, sd); NaN where undefined
#' @param paramLines function(options) -> character vector, one Input-table row each
#' @param distModes  subset of c("is", "lower", "higher", "interval")
#' @param quantModes subset of c("cumulative", "central")
#' @param window     function(options) -> c(lower, upper), the plotted x range
#' @param gridSize   function(options) -> number of points, or NA for the integer support
#' @param breaks     function(options, window) -> x-axis break positions
#' @param clamp      optional function(options) -> options, applied before anything else
distSpecNew <- function(discrete, d, p, q, moments, paramLines,
                        distModes, quantModes, window, gridSize, breaks,
                        clamp = NULL) {
    list(discrete = discrete, d = d, p = p, q = q, moments = moments,
         paramLines = paramLines, distModes = distModes, quantModes = quantModes,
         window = window, gridSize = gridSize, breaks = breaks, clamp = clamp)
}

.distSpecs <- list(

    # ---- Normal ----------------------------------------------------------
    normal = distSpecNew(
        discrete = FALSE,
        d = function(x, o)  dnorm(x, o$dp1, o$dp2),
        p = function(q, o)  pnorm(q, o$dp1, o$dp2),
        q = function(pr, o) qnorm(pr, o$dp1, o$dp2),
        moments = function(o) list(mean = o$dp1, sd = o$dp2),
        paramLines = function(o) c(paste0("Mean = ", o$dp1), paste0("SD = ", o$dp2)),
        distModes  = c("lower", "higher", "interval"),
        quantModes = c("cumulative", "central"),
        window   = function(o) c(o$dp1 - 4 * o$dp2, o$dp1 + 4 * o$dp2),
        gridSize = function(o) 1000,
        breaks   = function(o, w) seq(w[1], w[2], by = o$dp2)),

    # ---- t ---------------------------------------------------------------
    t = distSpecNew(
        discrete = FALSE,
        d = function(x, o)  dt(x, o$dp1, o$dp2),
        p = function(q, o)  pt(q, o$dp1, o$dp2),
        q = function(pr, o) qt(pr, o$dp1, o$dp2),
        moments = function(o) {
            mu <- if (o$dp1 > 1) o$dp2 * sqrt(o$dp1 / 2) *
                                 exp(lgamma((o$dp1 - 1) / 2) - lgamma(o$dp1 / 2)) else NaN
            sd <- if (o$dp1 > 2)
                      sqrt(o$dp1 * (1 + o$dp2^2) / (o$dp1 - 2) -
                           ifelse(is.na(mu), 0, mu^2)) else NaN
            list(mean = mu, sd = sd)
        },
        paramLines = function(o) c(paste0("df = ", o$dp1), paste0("δ = ", o$dp2)),
        distModes  = c("lower", "higher", "interval"),
        quantModes = c("cumulative", "central"),
        # transcribed from tdistribution.b.R:36-61 - widens as df shrinks
        window = function(o) {
            m <- qt(0.5, df = o$dp1, ncp = o$dp2)
            k <- if (o$dp1 < 2) 10 else if (o$dp1 < 3) 8 else if (o$dp1 < 5) 7 else
                 if (o$dp1 < 6) 6 else if (o$dp1 < 11) 5 else 4
            c(m - k, m + k * (o$dp2 + 1))
        },
        gridSize = function(o) 1000,
        breaks = function(o, w) {
            n <- if (o$dp1 < 2) 11 else if (o$dp1 < 3) 17 else if (o$dp1 < 5) 15 else
                 if (o$dp1 < 6) 13 else if (o$dp1 < 11) 11 else 9
            seq(ceiling(w[1]), floor(w[2]), length.out = n)
        }),

    # ---- Chi-square ------------------------------------------------------
    chi2 = distSpecNew(
        discrete = FALSE,
        d = function(x, o)  dchisq(x, o$dp1, o$dp2),
        p = function(q, o)  pchisq(q, o$dp1, o$dp2),
        q = function(pr, o) qchisq(pr, o$dp1, o$dp2),
        moments = function(o) list(mean = o$dp1 + o$dp2,
                                   sd = sqrt(2 * (o$dp1 + 2 * o$dp2))),
        paramLines = function(o) c(paste0("df = ", o$dp1), paste0("λ = ", o$dp2)),
        distModes  = c("lower", "higher", "interval"),
        quantModes = "cumulative",
        # transcribed from chi2distribution.b.R:34-41
        window = function(o) {
            lower <- 0; upper <- o$dp1 * 6 + o$dp2
            if (o$dp1 > 4)  upper <- ceiling(qchisq(0.9999, o$dp1, o$dp2))
            if (o$dp1 > 20) lower <- ceiling(qchisq(0.0001, o$dp1, o$dp2))
            c(lower, upper)
        },
        gridSize = function(o) 1000,
        breaks   = function(o, w) seq(w[1], w[2], by = 1)),

    # ---- F ---------------------------------------------------------------
    f = distSpecNew(
        discrete = FALSE,
        d = function(x, o)  df(x, o$dp1, o$dp2, o$dp3),
        p = function(q, o)  pf(q, o$dp1, o$dp2, o$dp3),
        q = function(pr, o) qf(pr, o$dp1, o$dp2, o$dp3),
        moments = function(o) list(
            mean = if (o$dp2 > 2) o$dp2 * (o$dp1 + o$dp3) / (o$dp1 * (o$dp2 - 2)) else NaN,
            sd   = if (o$dp2 > 4)
                       sqrt(2 * o$dp2^2 * ((o$dp1 + o$dp3)^2 +
                            (o$dp1 + 2 * o$dp3) * (o$dp2 - 2)) /
                            (o$dp1^2 * (o$dp2 - 2)^2 * (o$dp2 - 4))) else NaN),
        paramLines = function(o) c(paste0("df1 = ", o$dp1), paste0("df2 = ", o$dp2),
                                   paste0("λ = ", o$dp3)),
        distModes  = c("lower", "higher", "interval"),
        quantModes = "cumulative",
        # transcribed from fdistribution.b.R:36-44
        window = function(o) {
            lower <- 0
            upper <- if (o$dp1 < 5) 15 else 10
            if (((upper - lower) / 2) > (qf(0.9999, o$dp1, o$dp2, o$dp3) - lower))
                upper <- ceiling(qf(0.9995, o$dp1, o$dp2, o$dp3) * 2) / 2
            c(lower, upper)
        },
        gridSize = function(o) 1000,
        breaks = function(o, w) {
            special <- !identical(w[2], if (o$dp1 < 5) 15 else 10)
            b <- seq(w[1], ceiling(w[2]), by = 1)
            if (special && length(b) < 5)
                b <- seq(w[1], ceiling(w[2]), by = 0.25)
            b
        }),

    # ---- Binomial --------------------------------------------------------
    binomial = distSpecNew(
        discrete = TRUE,
        d = function(x, o)  dbinom(x, o$dp1, o$dp2),
        p = function(q, o)  pbinom(q, o$dp1, o$dp2),
        q = function(pr, o) qbinom(pr, o$dp1, o$dp2),
        moments = function(o) list(mean = o$dp1 * o$dp2,
                                   sd = sqrt(o$dp1 * o$dp2 * (1 - o$dp2))),
        paramLines = function(o) c(paste0("Size = ", o$dp1), paste0("Prob. = ", o$dp2)),
        distModes  = c("is", "lower", "higher", "interval"),
        quantModes = c("cumulative", "central"),
        window   = function(o) c(0, o$dp1),
        gridSize = function(o) NA,
        breaks   = function(o, w) seq(0, o$dp1, by = 1)),

    # ---- Poisson ---------------------------------------------------------
    poisson = distSpecNew(
        discrete = TRUE,
        d = function(x, o)  dpois(x, o$dp1),
        p = function(q, o)  ppois(q, o$dp1),
        q = function(pr, o) qpois(pr, o$dp1),
        moments = function(o) list(mean = o$dp1, sd = sqrt(o$dp1)),
        paramLines = function(o) c(paste0("λ = ", o$dp1), ""),
        distModes  = c("is", "lower", "higher", "interval"),
        quantModes = "cumulative",
        window   = function(o) c(0, ceiling(qpois(0.99999, o$dp1))),
        gridSize = function(o) NA,
        # the only analysis that thins its breaks; poissondistribution.b.R:165-171
        breaks = function(o, w) {
            N  <- w[2]
            bw <- if (N > 100) floor(N / 100) * 10 else if (N > 50) floor(N / 50) * 5 else 1
            seq(w[1], N, by = bw)
        }),

    # ---- Geometric -------------------------------------------------------
    geom = distSpecNew(
        discrete = TRUE,
        d = function(x, o)  dgeom(x, o$dp1),
        p = function(q, o)  pgeom(q, o$dp1),
        q = function(pr, o) qgeom(pr, o$dp1),
        moments = function(o) if (o$dp1 > 0 && o$dp1 <= 1)
                                  list(mean = (1 - o$dp1) / o$dp1,
                                       sd = sqrt(1 - o$dp1) / o$dp1)
                              else list(mean = NaN, sd = NaN),
        paramLines = function(o) c(paste0("Prob. = ", o$dp1), ""),
        distModes  = c("is", "lower", "higher", "interval"),
        quantModes = c("cumulative", "central"),
        window   = function(o) c(0, ceiling(qgeom(0.99999, o$dp1))),
        gridSize = function(o) NA,
        breaks   = function(o, w) seq(w[1], w[2], by = 1)),

    # ---- Hypergeometric --------------------------------------------------
    # Parameter mapping: dp1 = N (population), dp2 = K (successes), dp3 = n
    # (sample), onto dhyper(x, m = K, n = N - K, k = n).
    hyper = distSpecNew(
        discrete = TRUE,
        d = function(x, o)  dhyper(x, o$dp2, o$dp1 - o$dp2, o$dp3),
        p = function(q, o)  phyper(q, o$dp2, o$dp1 - o$dp2, o$dp3),
        q = function(pr, o) qhyper(pr, o$dp2, o$dp1 - o$dp2, o$dp3),
        moments = function(o)
            # This guard is unreachable while clamp() below is in place - the
            # clamp forces every conjunct true. Kept faithful to v1.2.2 for now;
            # both are replaced by real validation in a later commit.
            if (o$dp1 > 0 && o$dp2 >= 0 && o$dp3 >= 0 && o$dp2 <= o$dp1 && o$dp3 <= o$dp1)
                list(mean = o$dp3 * o$dp2 / o$dp1,
                     sd = if (o$dp1 > 1)
                              sqrt(o$dp3 * o$dp2 * (o$dp1 - o$dp2) * (o$dp1 - o$dp3) /
                                   (o$dp1^2 * (o$dp1 - 1))) else NaN)
            else list(mean = NaN, sd = NaN),
        paramLines = function(o) c(paste0("N = ", o$dp1),
                                   paste0("K = ", o$dp2, ", n = ", o$dp3)),
        distModes  = c("is", "lower", "higher", "interval"),
        quantModes = c("cumulative", "central"),
        window   = function(o) c(max(0, o$dp3 - (o$dp1 - o$dp2)), min(o$dp3, o$dp2)),
        gridSize = function(o) NA,
        breaks   = function(o, w) seq(w[1], w[2], by = 1),
        clamp = function(o) {
            o$dp1 <- max(1, o$dp1)
            o$dp2 <- min(max(0, o$dp2), o$dp1)
            o$dp3 <- min(max(0, o$dp3), o$dp1)
            o
        })
)

#' Look up a distribution spec by name.
#' @param name one of names(.distSpecs)
distSpec <- function(name) {
    spec <- .distSpecs[[name]]
    if (is.null(spec)) stop("unknown distribution spec: ", name)
    spec$name <- name
    spec
}
