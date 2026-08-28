# ---------------------------------------------------------------------------
# Open defects, pinned.
#
# Each block below asserts the invariant that SHOULD hold, wrapped in
# expect_failure() because it currently does not. That keeps CI green while
# the bug is open, and turns the test red the day someone fixes it - at which
# point delete the expect_failure() wrapper and move the assertion into
# test-plot-window.R.
#
# Every one of these was found by reading the code and confirmed by running it.
# ---------------------------------------------------------------------------

test_that("ISSUE 1: t-distribution window collapses for negative noncentrality", {
    # UpperTail <- qt(.5, df, ncp) + k * (ncp + 1)
    # With ncp <= -1 the multiplier goes non-positive, so the upper bound sits
    # at or below the lower bound. delta = -2 gives a zero-width window (one
    # dot, no curve); delta = -3 inverts it. tdistribution.a.yaml puts no min
    # on dp2, and negative delta is a legitimate input.
    expect_failure(expect_true(is_usable_window(window_t(10, -2))))
    expect_failure(expect_true(is_usable_window(window_t(10, -3))))

    # delta = -1 gives a window that is technically valid but cuts the
    # distribution in half at its own median.
    expect_failure(
        expect_true(window_contains_mass(window_t(10, -1), function(q) qt(q, 10, -1))))
})

test_that("ISSUE 2: chi-square/F density is infinite at the left edge of the grid", {
    # The grid starts at exactly 0, where the density diverges for df < 2
    # (chi-square) and df1 < 2 (F). chi-square with 1 df and F(1, k) are both
    # everyday inputs. The y-axis is then scaled by the singularity and the
    # shaded probability area is flattened to invisibility.
    expect_failure(
        expect_true(all(is.finite(
            dchisq(seq(window_chisq(1, 0)[["lower"]], window_chisq(1, 0)[["upper"]],
                       length = 1000), 1, 0)))))
    expect_failure(
        expect_true(all(is.finite(
            df(seq(window_f(1, 10, 0)[["lower"]], window_f(1, 10, 0)[["upper"]],
                   length = 1000), 1, 10, 0)))))
})

test_that("ISSUE 3: x-axis breaks are unreadable at ordinary parameter values", {
    # Only the Poisson thins its breaks. Everywhere else the step is hard-coded
    # to 1, so the label count grows without bound with the parameter.
    expect_failure(expect_lte(length(breaks_binom(100)),        MAX_READABLE_BREAKS))
    expect_failure(expect_lte(length(breaks_chisq(30, 0)),      MAX_READABLE_BREAKS))
    expect_failure(expect_lte(length(breaks_chisq(100, 0)),     MAX_READABLE_BREAKS))
    expect_failure(expect_lte(length(breaks_geom(0.05)),        MAX_READABLE_BREAKS))
    expect_failure(expect_lte(length(breaks_hyper(500, 250, 200)), MAX_READABLE_BREAKS))
})

test_that("ISSUE 4: geometric grid size explodes for small success probability", {
    # N <- ceiling(qgeom(0.99999, p)) and then a data frame of N+1 rows.
    # geometricdistribution.a.yaml allows p down to 1e-10.
    expect_failure(expect_lt(grid_rows_geom(1e-5), 1e6))    # ~1.15e6 rows
    expect_failure(expect_lt(grid_rows_geom(1e-7), 1e6))    # ~1.15e8 rows, ~4.6 GB
})

test_that("ISSUE 4b: geometric errors out at p = 0", {
    # The generated header R/geometricdistribution.h.R still carries min = 0,
    # not the min = 1e-10 that the yaml was changed to. At p = 0,
    # qgeom(0.99999, 0) is NaN and seq() raises a raw R error.
    expect_error(
        { N <- ceiling(suppressWarnings(qgeom(0.99999, 0))); seq(0, N, length = N + 1) },
        "must be a finite number")
})

test_that("ISSUE 5: hypergeometric's invalid-parameter guard is unreachable", {
    # hypergeometricdistribution.b.R clamps first:
    #     DP1 <- max(1, dp1)
    #     DP2 <- min(max(0, dp2), DP1)
    #     DP3 <- min(max(0, dp3), DP1)
    # and then guards:
    #     if (DP1 > 0 && DP2 >= 0 && DP3 >= 0 && DP2 <= DP1 && DP3 <= DP1)
    #     else -> "undefined (invalid parameters)"   [now NaN]
    # The clamp on the line above makes every conjunct true by construction, so
    # the else branch can never run. The warning was installed and then
    # disabled two lines earlier.
    guard_can_fail <- function(dp1, dp2, dp3) {
        DP1 <- max(1, dp1); DP2 <- min(max(0, dp2), DP1); DP3 <- min(max(0, dp3), DP1)
        !(DP1 > 0 && DP2 >= 0 && DP3 >= 0 && DP2 <= DP1 && DP3 <= DP1)
    }
    grid <- expand.grid(dp1 = c(-5, 0, 1, 20), dp2 = c(-5, 0, 10, 30, 1e6),
                        dp3 = c(-5, 0, 5, 25, 1e6))
    fired <- mapply(guard_can_fail, grid$dp1, grid$dp2, grid$dp3)
    expect_failure(expect_true(any(fired)))   # nothing in this grid can trip it
})

test_that("ISSUE 5b: hypergeometric silently rewrites out-of-range parameters", {
    # DP2 <- min(max(0, dp2), DP1); DP3 <- min(max(0, dp3), DP1)
    # A student who types N = 20, K = 30, n = 25 gets an answer to a different
    # question with no warning - it is reported as if it were theirs.
    N <- 20; K <- 30; n <- 25
    K_used <- min(max(0, K), max(1, N))
    n_used <- min(max(0, n), max(1, N))
    expect_failure(expect_equal(K_used, K))
    expect_failure(expect_equal(n_used, n))
})
