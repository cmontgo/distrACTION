# ---------------------------------------------------------------------------
# Invariants the plot has to satisfy for the picture to mean anything.
#
# Every one of these is cheap, and between them they cover the failure modes
# that are otherwise invisible until someone opens the analysis in jamovi and
# looks at it. The cases that DON'T hold today are pinned in
# test-known-issues.R rather than here.
# ---------------------------------------------------------------------------

test_that("the plotted window is finite and increasing", {
    expect_true(is_usable_window(window_normal(0, 1)))
    expect_true(is_usable_window(window_normal(100, 15)))
    expect_true(is_usable_window(window_chisq(1, 0)))
    expect_true(is_usable_window(window_chisq(30, 0)))
    expect_true(is_usable_window(window_f(1, 10, 0)))
    expect_true(is_usable_window(window_f(50, 50, 0)))

    # t is only well behaved for non-negative noncentrality - see known issues
    for (ncp in c(0, 0.5, 1, 2, 5))
        expect_true(is_usable_window(window_t(10, ncp)), info = paste("ncp =", ncp))
})

test_that("the plotted window actually contains the distribution", {
    expect_true(window_contains_mass(window_normal(0, 1),    function(q) qnorm(q)))
    expect_true(window_contains_mass(window_normal(100, 15), function(q) qnorm(q, 100, 15)))
    expect_true(window_contains_mass(window_chisq(10, 0),    function(q) qchisq(q, 10)))
    expect_true(window_contains_mass(window_chisq(30, 0),    function(q) qchisq(q, 30)))
    expect_true(window_contains_mass(window_f(10, 10, 0),    function(q) qf(q, 10, 10)))
    expect_true(window_contains_mass(window_t(10, 0),        function(q) qt(q, 10)))
})

test_that("x-axis break counts stay readable", {
    # these are the ones that already scale properly
    expect_lte(length(breaks_normal(0, 1)),     MAX_READABLE_BREAKS)
    expect_lte(length(breaks_normal(500, 250)), MAX_READABLE_BREAKS)
    expect_lte(length(breaks_pois(10)),         MAX_READABLE_BREAKS)
    expect_lte(length(breaks_pois(100)),        MAX_READABLE_BREAKS)
    expect_lte(length(breaks_pois(1000)),       MAX_READABLE_BREAKS)
    expect_lte(length(breaks_binom(20)),        MAX_READABLE_BREAKS)
})

test_that("the density grid is finite everywhere it is evaluated", {
    grid <- function(w, n = 1000) seq(w[["lower"]], w[["upper"]], length = n)

    expect_true(all(is.finite(dnorm(grid(window_normal(0, 1)), 0, 1))))
    expect_true(all(is.finite(dt(grid(window_t(10, 0)), 10, 0))))
    expect_true(all(is.finite(dchisq(grid(window_chisq(3, 0)), 3, 0))))
    expect_true(all(is.finite(df(grid(window_f(3, 10, 0)), 3, 10, 0))))
})

test_that("the grid stays small enough to allocate", {
    expect_lt(grid_rows_binom(1000),   1e5)
    expect_lt(grid_rows_pois(1000),    1e5)
    expect_lt(grid_rows_geom(0.01),    1e5)
})

test_that("axis breaks never outrun the plot data frame", {
    # Dataset[1:length(AxisSegments), 5] <- AxisSegments silently grows the
    # data frame when there are more breaks than grid rows, padding the curve
    # with NA rows.
    expect_lte(length(breaks_binom(100)),  grid_rows_binom(100))
    expect_lte(length(breaks_pois(50)),    grid_rows_pois(50))
    expect_lte(length(breaks_chisq(10, 0)), 1000)
    expect_lte(length(breaks_chisq(100, 0)), 1000)
})
