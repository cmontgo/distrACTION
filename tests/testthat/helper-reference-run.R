# ---------------------------------------------------------------------------
# Reference transcription of the Results-table arithmetic as it stood at
# v1.2.2, before the shared core existed.
#
# Every branch below is a line-for-line transcription of the corresponding
# <dist>.b.R $.run() sections 1.2.1 and 1.2.2, with the source noted. It is the
# oracle for the characterization fixture: the refactored core has to reproduce
# these numbers exactly.
#
# Transcription is only trustworthy if it is checked, so tools/make-reference-fixture.R
# also recomputes every value from first principles and refuses to write the
# fixture unless the two agree. A typo here cannot silently become the spec.
# ---------------------------------------------------------------------------

# Which modes each analysis actually offers, from jamovi/<dist>.a.yaml.
reference_modes <- list(
    normal   = list(dist = c("lower", "higher", "interval"),        quantile = c("cumulative", "central")),
    t        = list(dist = c("lower", "higher", "interval"),        quantile = c("cumulative", "central")),
    chi2     = list(dist = c("lower", "higher", "interval"),        quantile = "cumulative"),
    f        = list(dist = c("lower", "higher", "interval"),        quantile = "cumulative"),
    binomial = list(dist = c("is", "lower", "higher", "interval"),  quantile = c("cumulative", "central")),
    poisson  = list(dist = c("is", "lower", "higher", "interval"),  quantile = "cumulative"),
    geom     = list(dist = c("is", "lower", "higher", "interval"),  quantile = c("cumulative", "central")),
    hyper    = list(dist = c("is", "lower", "higher", "interval"),  quantile = c("cumulative", "central"))
)

# d / p / q closures per distribution, in the same argument order the .b.R files use.
reference_fns <- list(
    normal   = list(d = function(x, o) dnorm(x, o$dp1, o$dp2),
                    p = function(q, o) pnorm(q, o$dp1, o$dp2),
                    q = function(pr, o) qnorm(pr, o$dp1, o$dp2)),
    t        = list(d = function(x, o) dt(x, o$dp1, o$dp2),
                    p = function(q, o) pt(q, o$dp1, o$dp2),
                    q = function(pr, o) qt(pr, o$dp1, o$dp2)),
    chi2     = list(d = function(x, o) dchisq(x, o$dp1, o$dp2),
                    p = function(q, o) pchisq(q, o$dp1, o$dp2),
                    q = function(pr, o) qchisq(pr, o$dp1, o$dp2)),
    f        = list(d = function(x, o) df(x, o$dp1, o$dp2, o$dp3),
                    p = function(q, o) pf(q, o$dp1, o$dp2, o$dp3),
                    q = function(pr, o) qf(pr, o$dp1, o$dp2, o$dp3)),
    binomial = list(d = function(x, o) dbinom(x, o$dp1, o$dp2),
                    p = function(q, o) pbinom(q, o$dp1, o$dp2),
                    q = function(pr, o) qbinom(pr, o$dp1, o$dp2)),
    poisson  = list(d = function(x, o) dpois(x, o$dp1),
                    p = function(q, o) ppois(q, o$dp1),
                    q = function(pr, o) qpois(pr, o$dp1)),
    geom     = list(d = function(x, o) dgeom(x, o$dp1),
                    p = function(q, o) pgeom(q, o$dp1),
                    q = function(pr, o) qgeom(pr, o$dp1)),
    # hypergeometric: the .b.R clamps before calling, and maps (N, K, n) onto
    # dhyper(x, m = K, n = N - K, k = n).
    hyper    = list(d = function(x, o) dhyper(x, o$dp2, o$dp1 - o$dp2, o$dp3),
                    p = function(q, o) phyper(q, o$dp2, o$dp1 - o$dp2, o$dp3),
                    q = function(pr, o) qhyper(pr, o$dp2, o$dp1 - o$dp2, o$dp3))
)

# TRUE where the .b.R adds the + d(x1) endpoint term to the higher/interval tails.
reference_discrete <- c(normal = FALSE, t = FALSE, chi2 = FALSE, f = FALSE,
                        binomial = TRUE, poisson = TRUE, geom = TRUE, hyper = TRUE)

# The hypergeometric clamp, hypergeometricdistribution.b.R:28-32. Applied before
# anything else touches the parameters. No other analysis clamps.
reference_clamp <- function(dist, o) {
    if (identical(dist, "hyper")) {
        o$dp1 <- max(1, o$dp1)
        o$dp2 <- min(max(0, o$dp2), o$dp1)
        o$dp3 <- min(max(0, o$dp3), o$dp1)
    }
    o
}

# Closed-form moments, transcribed from section 1.2.2 of each file.
reference_moments <- function(dist, o) {
    switch(dist,
        # normaldistribution.b.R:136-137
        normal = list(mean = o$dp1, sd = o$dp2),
        # tdistribution.b.R:158-172 - noncentral t, undefined below df 1 / df 2
        t = {
            mu <- if (o$dp1 > 1) o$dp2 * sqrt(o$dp1 / 2) *
                                 exp(lgamma((o$dp1 - 1) / 2) - lgamma(o$dp1 / 2)) else NaN
            sd <- if (o$dp1 > 2)
                      sqrt(o$dp1 * (1 + o$dp2^2) / (o$dp1 - 2) -
                           ifelse(is.na(mu), 0, mu^2)) else NaN
            list(mean = mu, sd = sd)
        },
        # chi2distribution.b.R:120-121 - noncentral chi-square
        chi2 = list(mean = o$dp1 + o$dp2, sd = sqrt(2 * (o$dp1 + 2 * o$dp2))),
        # fdistribution.b.R - noncentral F, undefined at df2 <= 2 / df2 <= 4
        f = list(
            mean = if (o$dp2 > 2) o$dp2 * (o$dp1 + o$dp3) / (o$dp1 * (o$dp2 - 2)) else NaN,
            sd   = if (o$dp2 > 4)
                       sqrt(2 * o$dp2^2 * ((o$dp1 + o$dp3)^2 + (o$dp1 + 2 * o$dp3) * (o$dp2 - 2)) /
                            (o$dp1^2 * (o$dp2 - 2)^2 * (o$dp2 - 4))) else NaN),
        # binomialdistribution.b.R:137-138
        binomial = list(mean = o$dp1 * o$dp2, sd = sqrt(o$dp1 * o$dp2 * (1 - o$dp2))),
        # poissondistribution.b.R:111-112
        poisson = list(mean = o$dp1, sd = sqrt(o$dp1)),
        # geometricdistribution.b.R:142-148 - R's failures-before-first-success form
        geom = if (o$dp1 > 0 && o$dp1 <= 1)
                   list(mean = (1 - o$dp1) / o$dp1, sd = sqrt(1 - o$dp1) / o$dp1)
               else list(mean = NaN, sd = NaN),
        # hypergeometricdistribution.b.R:143-156. The guard is unreachable after
        # the clamp above - kept here so the fixture records that fact.
        hyper = if (o$dp1 > 0 && o$dp2 >= 0 && o$dp3 >= 0 && o$dp2 <= o$dp1 && o$dp3 <= o$dp1)
                    list(mean = o$dp3 * o$dp2 / o$dp1,
                         sd = if (o$dp1 > 1)
                                  sqrt(o$dp3 * o$dp2 * (o$dp1 - o$dp2) * (o$dp1 - o$dp3) /
                                       (o$dp1^2 * (o$dp1 - 1))) else NaN)
                else list(mean = NaN, sd = NaN),
        stop("unknown distribution: ", dist))
}

#' Reproduce one Results-table row exactly as v1.2.2 produced it.
#'
#' @param dist one of names(reference_fns)
#' @param o    named list of options: dp1..dp3, x1, x2, p
#' @param distMode  "is" | "lower" | "higher" | "interval", or NA to skip
#' @param quantMode "cumulative" | "central", or NA to skip
#' @return list(probability, quantile, quantileLower, quantileUpper, mean, sd)
reference_run <- function(dist, o, distMode = NA, quantMode = NA) {
    o  <- reference_clamp(dist, o)
    fn <- reference_fns[[dist]]
    discrete <- reference_discrete[[dist]]

    probability <- NA_real_
    if (!is.na(distMode)) {
        probability <- switch(distMode,
            # P(X = x1): discrete analyses only
            is       = fn$d(o$x1, o),
            lower    = fn$p(o$x1, o),
            # discrete files add + d(x1); continuous files use plain 1 - p(x1)
            higher   = if (discrete) 1 - fn$p(o$x1, o) + fn$d(o$x1, o)
                       else          1 - fn$p(o$x1, o),
            interval = if (discrete) fn$p(o$x2, o) - fn$p(o$x1, o) + fn$d(o$x1, o)
                       else          fn$p(o$x2, o) - fn$p(o$x1, o))
    }

    quantile <- quantileLower <- quantileUpper <- NA_real_
    if (!is.na(quantMode)) {
        if (identical(quantMode, "cumulative")) {
            quantile <- fn$q(o$p, o)
        } else {
            # central: LowerQuantile <- (1-p)/2; HigherQuantile <- LowerQuantile + p
            lo <- (1 - o$p) / 2
            quantileLower <- fn$q(lo, o)
            quantileUpper <- fn$q(lo + o$p, o)
        }
    }

    m <- reference_moments(dist, o)
    list(probability = probability, quantile = quantile,
         quantileLower = quantileLower, quantileUpper = quantileUpper,
         mean = m$mean, sd = m$sd)
}
