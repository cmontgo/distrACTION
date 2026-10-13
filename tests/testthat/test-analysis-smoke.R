# ---------------------------------------------------------------------------
# End-to-end smoke tests through the real jamovi analysis classes.
#
# jmvcore is on CRAN, so this normally runs: install.packages("jmvcore"), then
# R CMD INSTALL the package. It skips itself where either is missing, so the
# file stays harmless on a machine with only base R.
#
# This is the layer that catches "the analysis threw" regressions - the kind
# that otherwise only show up as a red banner in jamovi.
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

# The public wrappers hand back results, not the analysis, so driving the real
# render function means building the class directly. data must be supplied -
# jmvcore's Analysis$run() reaches for a dataset reader otherwise.
analysisFor <- function(base, args) {
    ns <- asNamespace("distrACTION")
    opts <- do.call(get(paste0(base, "Options"), envir = ns)$new, args)
    a <- get(paste0(base, "Class"), envir = ns)$new(options = opts, data = data.frame())
    a$run()
    a
}

renderCases <- list(
    Normaldistribution         = list(dp1 = 0,   dp2 = 1,  x1 = 1.96,  x2 = 2.5),
    TDistribution              = list(dp1 = 10,  dp2 = -3, x1 = -2,    x2 = 1),
    Chi2Distribution           = list(dp1 = 1,   dp2 = 0,  x1 = 3.84,  x2 = 6),
    FDistribution              = list(dp1 = 1,   dp2 = 10, dp3 = 0, x1 = 4.96, x2 = 8),
    BinomialDistribution       = list(dp1 = 100, dp2 = 0.5, x1 = 60,   x2 = 70),
    PoissonDistribution        = list(dp1 = 1000, x1 = 1050, x2 = 1100),
    GeometricDistribution      = list(dp1 = 0.05, x1 = 20,  x2 = 40),
    HypergeometricDistribution = list(dp1 = 500, dp2 = 250, dp3 = 200, x1 = 90, x2 = 110))

test_that("every analysis renders its plot through the real jamovi objects", {
    skip_if_no_jmvcore()
    skip_if_not_installed("ggplot2")

    # Render to a temp file rather than the default device: ggplot2's print()
    # with no device open drops an Rplots.pdf in the working directory.
    f <- tempfile(fileext = ".png")
    on.exit({ if (grDevices::dev.cur() > 1) grDevices::dev.off()
              unlink(f) }, add = TRUE)

    for (base in names(renderCases)) {
        # the awkward parameter sets on purpose - a t with negative
        # noncentrality, a chi-square with 1 df, a Poisson with lambda 1000
        args <- c(renderCases[[base]],
                  list(DistributionFunction = TRUE, QuantileFunction = TRUE,
                       DistributionFunctionType = "higher", p = 0.95))
        a <- analysisFor(base, args)
        grDevices::png(f, width = 500, height = 400)
        rendered <- tryCatch(a$.__enclos_env__$private$.plot(a$results$plot),
                             error = function(e) conditionMessage(e))
        grDevices::dev.off()
        expect_identical(rendered, TRUE, label = paste(base, "render"))
        expect_gt(file.info(f)$size, 0)
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
