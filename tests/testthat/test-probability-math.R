# ---------------------------------------------------------------------------
# The numbers the module reports in the Results table, checked through the real
# distributionResults() rather than a transcription of it.
#
# The characterization suite proves the refactor did not MOVE these numbers.
# This file proves they were right to begin with: textbook critical values, the
# discrete tail identities, and the closed-form moments against simulation.
# ---------------------------------------------------------------------------

prob <- function(dist, o, mode) distributionResults(dist, o, distMode = mode)$probability

test_that("continuous tails match textbook critical values", {
    # chi-square, 5% critical value at 1 df
    expect_equal(prob("chi2", list(dp1 = 1, dp2 = 0, x1 = 3.8415), "higher"),
                 0.05, tolerance = 1e-4)
    # F, 5% critical value for F(1, 10)
    expect_equal(prob("f", list(dp1 = 1, dp2 = 10, dp3 = 0, x1 = 4.9646), "higher"),
                 0.05, tolerance = 1e-4)
    # t, two-tailed 5% at 10 df
    expect_equal(prob("t", list(dp1 = 10, dp2 = 0, x1 = 2.2281), "higher"),
                 0.025, tolerance = 1e-4)
    # the 1.96 everyone knows
    expect_equal(prob("normal", list(dp1 = 0, dp2 = 1, x1 = 1.959964), "higher"),
                 0.025, tolerance = 1e-6)
    expect_equal(prob("normal", list(dp1 = 0, dp2 = 1, x1 = -1.959964, x2 = 1.959964),
                      "interval"),
                 0.95, tolerance = 1e-6)
})

test_that("discrete tails include their endpoint", {
    # P(X >= x1) must equal the sum over the support from x1 upwards
    expect_equal(prob("binomial", list(dp1 = 10, dp2 = 0.5, x1 = 6), "higher"),
                 sum(dbinom(6:10, 10, 0.5)))
    expect_equal(prob("poisson", list(dp1 = 2, x1 = 3), "higher"),
                 sum(dpois(3:400, 2)))
    expect_equal(prob("geom", list(dp1 = 0.5, x1 = 3), "higher"),
                 sum(dgeom(3:800, 0.5)))
    expect_equal(prob("hyper", list(dp1 = 20, dp2 = 10, dp3 = 5, x1 = 3), "higher"),
                 sum(dhyper(3:5, 10, 10, 5)))

    # and P(x1 <= X <= x2) is closed at both ends
    expect_equal(prob("binomial", list(dp1 = 10, dp2 = 0.5, x1 = 2, x2 = 5), "interval"),
                 sum(dbinom(2:5, 10, 0.5)))
    expect_equal(prob("hyper", list(dp1 = 20, dp2 = 10, dp3 = 5, x1 = 1, x2 = 3), "interval"),
                 sum(dhyper(1:3, 10, 10, 5)))

    # P(X = x1) for the discrete analyses
    expect_equal(prob("binomial", list(dp1 = 10, dp2 = 0.5, x1 = 5), "is"),
                 dbinom(5, 10, 0.5))
    # a value off the support is genuinely zero, not an error
    expect_equal(prob("binomial", list(dp1 = 10, dp2 = 0.5, x1 = 15), "is"), 0)
    expect_equal(prob("binomial", list(dp1 = 10, dp2 = 0.5, x1 = 2.5), "is"), 0)
})

test_that("the four modes partition the distribution", {
    for (cs in list(list("normal",   list(dp1 = 0, dp2 = 1, x1 = 0.7)),
                    list("t",        list(dp1 = 10, dp2 = 0, x1 = 0.7)),
                    list("chi2",     list(dp1 = 3, dp2 = 0, x1 = 2)),
                    list("binomial", list(dp1 = 10, dp2 = 0.5, x1 = 4)),
                    list("poisson",  list(dp1 = 3, x1 = 2)),
                    list("hyper",    list(dp1 = 20, dp2 = 10, dp3 = 5, x1 = 2)))) {
        d <- cs[[1]]; o <- cs[[2]]
        atPoint <- if (distSpec(d)$discrete) prob(d, o, "is") else 0
        # P(X <= x1) + P(X >= x1) - P(X = x1) == 1
        expect_equal(prob(d, o, "lower") + prob(d, o, "higher") - atPoint, 1,
                     tolerance = 1e-12, info = d)
    }
})

test_that("central-interval quantiles are symmetric in the tail mass", {
    b <- centralBounds(0.95)
    expect_equal(unname(b["lower"]), 0.025)
    expect_equal(unname(b["upper"]), 0.975)

    r <- distributionResults("normal", list(dp1 = 0, dp2 = 1, p = 0.95),
                             quantMode = "central")
    expect_equal(r$quantileLower, qnorm(0.025))
    expect_equal(r$quantileUpper, qnorm(0.975))
    expect_equal(r$quantileLower, -r$quantileUpper)

    r <- distributionResults("t", list(dp1 = 10, dp2 = 0, p = 0.95),
                             quantMode = "central")
    expect_equal(r$quantileLower, -r$quantileUpper)
})

test_that("a cumulative quantile inverts the cdf", {
    for (cs in list(list("normal", list(dp1 = 0, dp2 = 1)),
                    list("t",      list(dp1 = 10, dp2 = 0)),
                    list("chi2",   list(dp1 = 3, dp2 = 0)),
                    list("f",      list(dp1 = 3, dp2 = 10, dp3 = 0)))) {
        d <- cs[[1]]; spec <- distSpec(d)
        for (p in c(0.05, 0.5, 0.95)) {
            o <- c(cs[[2]], list(p = p))
            q <- distributionResults(d, o, quantMode = "cumulative")$quantile
            expect_equal(spec$p(q, o), p, tolerance = 1e-8, info = paste(d, p))
        }
    }
})

test_that("distribution statistics match the closed-form moments", {
    set.seed(20260907)
    mom <- function(d, o) distributionResults(d, o)[c("mean", "sd")]

    m <- mom("binomial", list(dp1 = 10, dp2 = 0.3))
    s <- rbinom(4e5, 10, 0.3)
    expect_equal(m$mean, mean(s), tolerance = 0.02)
    expect_equal(m$sd,   sd(s),   tolerance = 0.02)

    m <- mom("chi2", list(dp1 = 4, dp2 = 3))          # noncentral
    s <- rchisq(2e5, 4, 3)
    expect_equal(m$mean, mean(s), tolerance = 0.05)
    expect_equal(m$sd,   sd(s),   tolerance = 0.05)

    m <- mom("t", list(dp1 = 12, dp2 = 1.5))          # noncentral
    s <- rt(2e5, 12, 1.5)
    expect_equal(m$mean, mean(s), tolerance = 0.05)
    expect_equal(m$sd,   sd(s),   tolerance = 0.05)

    m <- mom("hyper", list(dp1 = 50, dp2 = 20, dp3 = 10))
    expect_equal(m$mean, 4)
    expect_equal(m$sd, sd(rhyper(4e5, 20, 30, 10)), tolerance = 0.02)

    m <- mom("geom", list(dp1 = 0.25))                # failures before success
    s <- rgeom(4e5, 0.25)
    expect_equal(m$mean, mean(s), tolerance = 0.03)
    expect_equal(m$sd,   sd(s),   tolerance = 0.03)

    m <- mom("poisson", list(dp1 = 7))
    expect_equal(m$mean, 7)
    expect_equal(m$sd, sqrt(7))
})

test_that("moments that do not exist are reported as NaN, not invented", {
    expect_true(is.nan(distributionResults("t", list(dp1 = 1, dp2 = 0))$mean))
    expect_true(is.nan(distributionResults("t", list(dp1 = 2, dp2 = 0))$sd))
    expect_true(is.nan(distributionResults("f", list(dp1 = 3, dp2 = 2, dp3 = 0))$mean))
    expect_true(is.nan(distributionResults("f", list(dp1 = 3, dp2 = 4, dp3 = 0))$sd))
})
