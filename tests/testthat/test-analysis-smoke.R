# ---------------------------------------------------------------------------
# End-to-end smoke tests through the real jamovi analysis classes.
#
# Skipped wherever jmvcore is unavailable (it is not on CRAN), so this file is
# harmless on a machine that only has base R. On a dev box with jmvcore
# installed it is the layer that catches "the analysis threw" regressions -
# the kind that only show up as a red banner in jamovi.
# ---------------------------------------------------------------------------

skip_if_no_jmvcore <- function()
    testthat::skip_if_not_installed("jmvcore")

# Every analysis, at its defaults, with both output blocks switched on.
default_calls <- list(
    Normaldistribution         = function() Normaldistribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 1.96, p = 0.95, dp1 = 0, dp2 = 1),
    TDistribution              = function() TDistribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 2.228, p = 0.95, dp1 = 10, dp2 = 0),
    Chi2Distribution           = function() Chi2Distribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 7.81, p = 0.95, dp1 = 3, dp2 = 0),
    FDistribution              = function() FDistribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 4.96, p = 0.95, dp1 = 1, dp2 = 10, dp3 = 0),
    BinomialDistribution       = function() BinomialDistribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 6, p = 0.95, dp1 = 10, dp2 = 0.5),
    PoissonDistribution        = function() PoissonDistribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 3, p = 0.95, dp1 = 2),
    GeometricDistribution      = function() GeometricDistribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 3, p = 0.95, dp1 = 0.5),
    HypergeometricDistribution = function() HypergeometricDistribution(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        x1 = 3, p = 0.95, dp1 = 20, dp2 = 10, dp3 = 5)
)

test_that("every analysis runs at its defaults without erroring", {
    skip_if_no_jmvcore()
    for (nm in names(default_calls))
        expect_no_error(default_calls[[nm]](), message = nm)
})

test_that("every analysis renders its plot without erroring", {
    skip_if_no_jmvcore()
    skip_if_not_installed("ggplot2")
    for (nm in names(default_calls)) {
        res <- default_calls[[nm]]()
        expect_no_error(print(res$plot), message = nm)
    }
})

test_that("the reported probability matches stats:: directly", {
    skip_if_no_jmvcore()

    res <- Normaldistribution(DistributionFunction = TRUE, DistributionFunctionType = "lower",
                              x1 = 1.96, dp1 = 0, dp2 = 1)
    expect_equal(res$Outputs$getCell(rowNo = 1, "DistributionResultColumn")$value,
                 pnorm(1.96), tolerance = 1e-10)

    res <- BinomialDistribution(DistributionFunction = TRUE, DistributionFunctionType = "higher",
                                x1 = 6, dp1 = 10, dp2 = 0.5)
    expect_equal(res$Outputs$getCell(rowNo = 1, "DistributionResultColumn")$value,
                 sum(dbinom(6:10, 10, 0.5)), tolerance = 1e-10)
})

test_that("x2 <= x1 in interval mode is rejected rather than reported", {
    skip_if_no_jmvcore()
    # Currently jmvcore::reject() fires at the END of .run(), after a negative
    # probability has already been written into the Results table. Whatever the
    # mechanism, no negative probability may reach the user.
    expect_error(
        Normaldistribution(DistributionFunction = TRUE, DistributionFunctionType = "interval",
                           x1 = 2, x2 = 1, dp1 = 0, dp2 = 1))
})
