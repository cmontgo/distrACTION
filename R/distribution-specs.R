# ---------------------------------------------------------------------------
# One spec per distribution.
#
# Everything the eight analyses do identically lives in distribution-core.R.
# This file holds only what actually differs: the density/cdf/quantile calls,
# whether the endpoint belongs to the tail, the closed-form moments, the labels
# for the Input table, and the modes the analysis offers.
#
# Plot windows and axis breaks now come from the shared rules in
# distribution-core.R. A spec may override them, but only with a reason worth
# writing down - the normal is the sole case, because sigma-spaced ticks carry
# the empirical rule that the rest of a stats course is built on.
# ---------------------------------------------------------------------------

#' Build one distribution spec.
#'
#' @param discrete   TRUE if the endpoint x1 belongs to the higher/interval tail
#' @param d,p,q      density, cdf and quantile closures, each taking (value, options)
#' @param moments    function(options) -> list(mean, sd); NaN where undefined
#' @param paramLines function(options) -> character vector, one Input-table row each
#' @param distModes  subset of c("is", "lower", "higher", "interval")
#' @param quantModes subset of c("cumulative", "central")
#' @param momentNote optional function(options) -> list(MeanColumn=, SDColumn=) footnotes
#' @param support    discrete only: function(options) -> c(lowest, highest) outcome
#' @param integerParams list of list(opt=, label=) for parameters that must be whole
#' @param validate   optional function(options) -> message, or NULL when valid
#' @param window     optional override, function(options) -> c(lower, upper)
#' @param breaks     optional override, function(options, window) -> break positions
distSpecNew <- function(discrete, d, p, q, moments, paramLines,
                        distModes, quantModes, support = NULL,
                        integerParams = list(), validate = NULL,
                        window = NULL, breaks = NULL, momentNote = NULL) {
    list(discrete = discrete, d = d, p = p, q = q, moments = moments,
         paramLines = paramLines, distModes = distModes, quantModes = quantModes,
         support = support, integerParams = integerParams, validate = validate,
         window = window, breaks = breaks, momentNote = momentNote)
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
        # Deliberate override: +/- 4 SD with ticks every SD, so the axis reads
        # off the empirical rule (68/95/99.7) rather than round numbers.
        window = function(o) c(o$dp1 - 4 * o$dp2, o$dp1 + 4 * o$dp2),
        breaks = function(o, w) seq(w[1], w[2], by = o$dp2)),

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
        momentNote = function(o) list(
            MeanColumn = if (o$dp1 <= 1) "The mean of a t distribution is undefined for df ≤ 1",
            SDColumn   = if (o$dp1 <= 2) "The SD of a t distribution is undefined for df ≤ 2"),
        distModes  = c("lower", "higher", "interval"),
        quantModes = c("cumulative", "central"),
        # The old window was median - k, median + k*(ncp+1), which collapsed to
        # zero width at ncp = -1 and inverted below it. Shared rule now.
        #
        # df is deliberately NOT constrained to whole numbers: dt() handles real
        # df, and Welch-Satterthwaite produces fractional df routinely - which is
        # exactly what someone checking a Welch t-test would type in here.
        ),

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
        # The old window started at exactly 0, where the density diverges for
        # df < 2; breaks stepped by 1 regardless of df. Shared rules now.
        # df is real-valued, as for the t.
        ),

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
        momentNote = function(o) list(
            MeanColumn = if (o$dp2 <= 2) "The mean of an F distribution is undefined for df2 ≤ 2",
            SDColumn   = if (o$dp2 <= 4) "The SD of an F distribution is undefined for df2 ≤ 4"),
        distModes  = c("lower", "higher", "interval"),
        quantModes = "cumulative",
        # df1 and df2 are real-valued, as for the t.
        ),

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
        support = function(o) c(0, o$dp1),
        integerParams = list(list(opt = "dp1", label = "Size"))),

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
        # This was the only analysis that thinned its axis labels. That rule is
        # now shared, so the other seven get it too.
        support = function(o) c(0, ceiling(qpois(0.99999, o$dp1)))),

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
        # qgeom(0.99999, p) reaches 1.15e11 at the option minimum the YAML used
        # to allow, and the old code turned that straight into data-frame rows.
        # The shared window caps the bar count; the YAML minimum was raised too.
        support = function(o) c(0, ceiling(qgeom(0.99999, o$dp1)))),

    # ---- Hypergeometric --------------------------------------------------
    # Parameter mapping: dp1 = N (population), dp2 = K (successes), dp3 = n
    # (sample), onto dhyper(x, m = K, n = N - K, k = n).
    hyper = distSpecNew(
        discrete = TRUE,
        d = function(x, o)  dhyper(x, o$dp2, o$dp1 - o$dp2, o$dp3),
        p = function(q, o)  phyper(q, o$dp2, o$dp1 - o$dp2, o$dp3),
        q = function(pr, o) qhyper(pr, o$dp2, o$dp1 - o$dp2, o$dp3),
        # No guard needed: validate() below rejects out-of-range parameters
        # before anything is computed. v1.2.2 clamped them into range first,
        # which made its guard unreachable and answered a question the student
        # had not asked.
        moments = function(o) list(
            mean = o$dp3 * o$dp2 / o$dp1,
            sd   = if (o$dp1 > 1)
                       sqrt(o$dp3 * o$dp2 * (o$dp1 - o$dp2) * (o$dp1 - o$dp3) /
                            (o$dp1^2 * (o$dp1 - 1))) else NaN),
        paramLines = function(o) c(paste0("N = ", o$dp1),
                                   paste0("K = ", o$dp2, ", n = ", o$dp3)),
        momentNote = function(o) list(
            SDColumn = if (o$dp1 <= 1) "The SD is undefined for a population of one"),
        distModes  = c("is", "lower", "higher", "interval"),
        quantModes = c("cumulative", "central"),
        support = function(o) c(max(0, o$dp3 - (o$dp1 - o$dp2)), min(o$dp3, o$dp2)),
        integerParams = list(list(opt = "dp1", label = "Population size (N)"),
                             list(opt = "dp2", label = "Successes in population (K)"),
                             list(opt = "dp3", label = "Sample size (n)")),
        validate = function(o) {
            if (o$dp1 < 1)
                return("Population size (N) must be at least 1")
            if (o$dp2 > o$dp1)
                return("Successes in population (K) cannot exceed population size (N)")
            if (o$dp3 > o$dp1)
                return("Sample size (n) cannot exceed population size (N)")
            NULL
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
