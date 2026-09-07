# ---------------------------------------------------------------------------
# distributionValidate() - the checks that now run BEFORE anything is computed.
#
# In v1.2.2 the only check ran at the end of .run(), after a negative
# probability had already been written into the Results table, and the
# hypergeometric silently rewrote out-of-range parameters instead of reporting
# them.
# ---------------------------------------------------------------------------

opts <- function(...) {
    o <- list(x1 = 1, x2 = 2, p = 0.95, showDist = TRUE, showQuant = TRUE,
              distMode = "lower", quantMode = "cumulative")
    modifyList(o, list(...))
}

test_that("valid option sets pass", {
    expect_null(distributionValidate(distSpec("normal"),   opts(dp1 = 0, dp2 = 1)))
    expect_null(distributionValidate(distSpec("binomial"), opts(dp1 = 10, dp2 = 0.5)))
    expect_null(distributionValidate(distSpec("hyper"),
                                     opts(dp1 = 20, dp2 = 10, dp3 = 5)))
})

test_that("an interval needs x2 above x1", {
    for (d in c("normal", "binomial")) {
        o <- opts(dp1 = 10, dp2 = 0.5, distMode = "interval", x1 = 5, x2 = 3)
        expect_match(distributionValidate(distSpec(d), o), "x2 must be greater")
        # equal endpoints are rejected too; discrete users have the P(X = x1) mode
        o$x2 <- 5
        expect_match(distributionValidate(distSpec(d), o), "x2 must be greater")
        # and it only applies when the probability block is switched on
        o$showDist <- FALSE
        expect_null(distributionValidate(distSpec(d), o))
    }
})

test_that("hypergeometric parameters are reported, not quietly clamped", {
    spec <- distSpec("hyper")
    # K > N: v1.2.2 computed with K = N and said nothing
    expect_match(distributionValidate(spec, opts(dp1 = 20, dp2 = 30, dp3 = 5)),
                 "Successes in population \\(K\\) cannot exceed")
    # n > N
    expect_match(distributionValidate(spec, opts(dp1 = 20, dp2 = 10, dp3 = 25)),
                 "Sample size \\(n\\) cannot exceed")
    expect_match(distributionValidate(spec, opts(dp1 = 0, dp2 = 0, dp3 = 0)),
                 "Population size \\(N\\) must be at least 1")
})

test_that("count parameters must be whole numbers", {
    # dbinom() and dhyper() return NaN for fractional counts rather than
    # complaining, so the module has to say so itself
    expect_match(distributionValidate(distSpec("binomial"), opts(dp1 = 10.5, dp2 = 0.5)),
                 "Size must be a whole number")
    expect_match(distributionValidate(distSpec("hyper"), opts(dp1 = 20.5, dp2 = 10, dp3 = 5)),
                 "Population size \\(N\\) must be a whole number")
    expect_match(distributionValidate(distSpec("chi2"), opts(dp1 = 3.5, dp2 = 0)),
                 "df must be a whole number")
    expect_match(distributionValidate(distSpec("f"), opts(dp1 = 3, dp2 = 10.5, dp3 = 0)),
                 "df2 must be a whole number")

    # lambda and the probabilities are genuinely continuous - leave them be
    expect_null(distributionValidate(distSpec("poisson"),  opts(dp1 = 3.5)))
    expect_null(distributionValidate(distSpec("binomial"), opts(dp1 = 10, dp2 = 0.37)))
    expect_null(distributionValidate(distSpec("geom"),     opts(dp1 = 0.37)))

    # a fractional x1 is NOT an error: P(X = 2.5) really is 0 for a discrete
    # variable, and P(X <= 2.5) really is P(X <= 2)
    expect_null(distributionValidate(distSpec("binomial"), opts(dp1 = 10, dp2 = 0.5, x1 = 2.5)))
})
