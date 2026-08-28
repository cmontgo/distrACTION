# ---------------------------------------------------------------------------
# The numbers the module reports in the Results table.
#
# These currently all pass. Their job is to stay passing: they pin the
# probability/quantile arithmetic against textbook values and against the
# tail identities, so that a future refactor (or a change to one of the
# eight near-identical .b.R files) cannot quietly move a result.
# ---------------------------------------------------------------------------

test_that("continuous lower/higher/interval match textbook critical values", {
    # chi-square: 5% critical value, 1 df
    expect_equal(prob_higher_c(function(q) pchisq(q, 1), 3.8415), 0.05, tolerance = 1e-4)
    # F: 5% critical value, F(1, 10)
    expect_equal(prob_higher_c(function(q) pf(q, 1, 10), 4.9646), 0.05, tolerance = 1e-4)
    # t: two-tailed 5%, 10 df
    expect_equal(prob_higher_c(function(q) pt(q, 10), 2.2281), 0.025, tolerance = 1e-4)
    # normal: the 1.96 everyone knows
    expect_equal(prob_higher_c(function(q) pnorm(q), 1.959964), 0.025, tolerance = 1e-6)

    expect_equal(prob_interval_c(function(q) pnorm(q), -1.959964, 1.959964), 0.95,
                 tolerance = 1e-6)
})

test_that("discrete tails include the endpoint (the +d(x1) term)", {
    # P(X >= x1) must equal the sum over the support from x1 up.
    expect_equal(prob_higher_d(function(q) pbinom(q, 10, .5), function(q) dbinom(q, 10, .5), 6),
                 sum(dbinom(6:10, 10, .5)))
    expect_equal(prob_higher_d(function(q) ppois(q, 2), function(q) dpois(q, 2), 3),
                 sum(dpois(3:200, 2)))
    expect_equal(prob_higher_d(function(q) pgeom(q, .5), function(q) dgeom(q, .5), 3),
                 sum(dgeom(3:500, .5)))
    expect_equal(prob_higher_d(function(q) phyper(q, 10, 10, 5), function(q) dhyper(q, 10, 10, 5), 3),
                 sum(dhyper(3:5, 10, 10, 5)))

    # P(x1 <= X <= x2) likewise closed on both ends.
    expect_equal(prob_interval_d(function(q) pbinom(q, 10, .5), function(q) dbinom(q, 10, .5), 2, 5),
                 sum(dbinom(2:5, 10, .5)))
    expect_equal(prob_interval_d(function(q) phyper(q, 10, 10, 5), function(q) dhyper(q, 10, 10, 5), 1, 3),
                 sum(dhyper(1:3, 10, 10, 5)))
})

test_that("central-interval quantiles are symmetric in the tail mass", {
    b <- central_bounds(0.95)
    expect_equal(unname(b["lower"]), 0.025)
    expect_equal(unname(b["upper"]), 0.975)
    expect_equal(qnorm(b[["lower"]]), -qnorm(b[["upper"]]))
    expect_equal(qt(b[["lower"]], 10), -qt(b[["upper"]], 10))
})

test_that("distribution statistics match the closed-form moments", {
    # binomial: mean n p, sd sqrt(n p (1-p))
    s <- rbinom(4e5, 10, 0.3)
    expect_equal(10 * 0.3,                mean(s), tolerance = 0.02)
    expect_equal(sqrt(10 * 0.3 * 0.7),    sd(s),   tolerance = 0.02)

    # noncentral chi-square: mean df + ncp, var 2(df + 2 ncp)
    df <- 4; ncp <- 3
    s  <- rchisq(2e5, df, ncp)
    expect_equal(mean(s), df + ncp,            tolerance = 0.05)
    expect_equal(var(s),  2 * (df + 2 * ncp),  tolerance = 0.10)

    # noncentral t: mean delta sqrt(v/2) Gamma((v-1)/2)/Gamma(v/2)
    v <- 12; d <- 1.5
    mu <- d * sqrt(v / 2) * exp(lgamma((v - 1) / 2) - lgamma(v / 2))
    s  <- rt(2e5, v, d)
    expect_equal(mean(s), mu, tolerance = 0.05)
    expect_equal(var(s), v * (1 + d^2) / (v - 2) - mu^2, tolerance = 0.10)

    # hypergeometric
    N <- 50; K <- 20; n <- 10
    expect_equal(n * K / N, 4)
    expect_equal(n * K * (N - K) * (N - n) / (N^2 * (N - 1)),
                 var(rhyper(4e5, K, N - K, n)), tolerance = 0.05)

    # geometric (R's convention: failures before the first success)
    p <- 0.25
    expect_equal((1 - p) / p, mean(rgeom(4e5, p)), tolerance = 0.05)
    expect_equal(sqrt(1 - p) / p, sd(rgeom(4e5, p)), tolerance = 0.05)
})
