# ---------------------------------------------------------------------------
# Helpers shared by the test files.
#
# These mirror the arithmetic that currently lives inline inside each
# <dist>.b.R $.run() method. They exist so the logic can be exercised without
# jamovi. If/when that arithmetic is lifted out of .run() into real functions
# in R/, these helpers should be deleted and the tests pointed at the real
# ones instead - that is the whole point of the exercise.
# ---------------------------------------------------------------------------

# --- probability of the four "Mode for Distribution" branches ---------------
# d, p are the density/cdf closures for the distribution under test.

prob_lower    <- function(p, x1)        p(x1)
prob_higher_c <- function(p, x1)        1 - p(x1)                   # continuous
prob_higher_d <- function(p, d, x1)     1 - p(x1) + d(x1)           # discrete
prob_interval_c <- function(p, x1, x2)     p(x2) - p(x1)            # continuous
prob_interval_d <- function(p, d, x1, x2)  p(x2) - p(x1) + d(x1)    # discrete
prob_is       <- function(d, x1)        d(x1)

# --- central-interval quantile bounds --------------------------------------
central_bounds <- function(p) {
    lower <- (1 - p) / 2
    c(lower = lower, upper = lower + p)
}

# --- plot windows, transcribed from the current .b.R sources ---------------

window_normal <- function(mean, sd)
    c(lower = mean - 4 * sd, upper = mean + 4 * sd)

window_t <- function(df, ncp) {
    m <- stats::qt(0.5, df = df, ncp = ncp)
    k <- if (df < 2) 10 else if (df < 3) 8 else if (df < 5) 7 else
         if (df < 6) 6 else if (df < 11) 5 else 4
    c(lower = m - k, upper = m + k * (ncp + 1))
}

window_chisq <- function(df, ncp) {
    lower <- 0
    upper <- df * 6 + ncp
    if (df > 4)  upper <- ceiling(stats::qchisq(0.9999, df, ncp))
    if (df > 20) lower <- ceiling(stats::qchisq(0.0001, df, ncp))
    c(lower = lower, upper = upper)
}

window_f <- function(df1, df2, ncp) {
    lower <- 0
    upper <- if (df1 < 5) 15 else 10
    if (((upper - lower) / 2) > (stats::qf(0.9999, df1, df2, ncp) - lower))
        upper <- ceiling(stats::qf(0.9995, df1, df2, ncp) * 2) / 2
    c(lower = lower, upper = upper)
}

# --- x-axis break vectors, transcribed from the current .b.R sources -------

breaks_normal <- function(mean, sd) seq(mean - 4 * sd, mean + 4 * sd, by = sd)
breaks_binom  <- function(size)     seq(0, size, by = 1)
breaks_geom   <- function(prob)     seq(0, ceiling(stats::qgeom(0.99999, prob)), by = 1)
breaks_hyper  <- function(N, K, n)  seq(max(0, n - (N - K)), min(n, K), by = 1)
breaks_chisq  <- function(df, ncp)  { w <- window_chisq(df, ncp); seq(w[["lower"]], w[["upper"]], by = 1) }
breaks_pois   <- function(lambda) {
    N  <- ceiling(stats::qpois(0.99999, lambda))
    bw <- if (N > 100) floor(N / 100) * 10 else if (N > 50) floor(N / 50) * 5 else 1
    seq(0, N, by = bw)
}

# --- grid sizes, transcribed from the current .b.R sources -----------------

grid_rows_binom <- function(size)   size + 1
grid_rows_geom  <- function(prob)   ceiling(stats::qgeom(0.99999, prob)) + 1
grid_rows_pois  <- function(lambda) ceiling(stats::qpois(0.99999, lambda)) + 1

# --- predicates used by the invariant tests --------------------------------

is_usable_window <- function(w) {
    is.finite(w[["lower"]]) && is.finite(w[["upper"]]) && w[["upper"]] > w[["lower"]]
}

window_contains_mass <- function(w, q, coverage = 0.98) {
    # does the plotted window cover the bulk of the distribution?
    tail <- (1 - coverage) / 2
    q(tail) >= w[["lower"]] && q(1 - tail) <= w[["upper"]]
}

# A break vector a person can actually read on a 500px-wide jamovi image.
MAX_READABLE_BREAKS <- 30
