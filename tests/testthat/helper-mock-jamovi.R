# ---------------------------------------------------------------------------
# A stand-in for the handful of jmvcore objects runDistribution() and
# plotDistribution() actually touch.
#
# jmvcore is not on CRAN, so without this the generics - including all the plot
# code - would only ever be exercised on a machine that happens to have jamovi
# installed. The mock covers $options, the two result tables and the image, which
# is the whole surface the core uses.
#
# It deliberately does NOT stub jmvcore::reject(): rejection paths belong to
# test-analysis-smoke.R, where the real thing is present.
# ---------------------------------------------------------------------------

mockTable <- function() {
    rows <- list()
    list(
        setRow = function(rowNo, values) rows[[rowNo]] <<- values,
        rows   = function() rows)
}

# An environment, not a list with an S3 `$` method: plotDistribution() lives in
# a different environment than these helpers, so S3 dispatch on `$` would not be
# found there and image$state would silently read as NULL. Environments give
# field access natively.
mockImage <- function() {
    e <- new.env(parent = emptyenv())
    e$state <- NULL
    e$setState <- function(x) e$state <- x
    e
}

#' Build a mock analysis for one distribution.
#'
#' @param opts named list of jamovi options (dp1.., x1, x2, p,
#'   DistributionFunction, QuantileFunction, DistributionFunctionType,
#'   QuantileFunctionType)
mockAnalysis <- function(opts) {
    defaults <- list(
        DistributionFunction = TRUE, QuantileFunction = TRUE,
        DistributionFunctionType = "lower", QuantileFunctionType = "cumulative",
        x1 = 1, x2 = 2, p = 0.95)
    for (nm in names(defaults))
        if (is.null(opts[[nm]])) opts[[nm]] <- defaults[[nm]]

    img <- mockImage()
    list(options = opts,
         results = list(Inputs = mockTable(), Outputs = mockTable(), plot = img))
}

#' Run an analysis end to end and hand back what it produced.
mockRun <- function(distName, opts) {
    a <- mockAnalysis(opts)
    runDistribution(a, distSpec(distName))
    list(analysis = a,
         inputs   = a$results$Inputs$rows(),
         outputs  = a$results$Outputs$rows()[[1]],
         state    = a$results$plot$state)
}

#' Render an analysis's plot to a temp file; returns the path, or throws.
mockPlot <- function(distName, opts, file = tempfile(fileext = ".png")) {
    a <- mockAnalysis(opts)
    runDistribution(a, distSpec(distName))
    grDevices::png(file, width = 500, height = 400)
    on.exit(grDevices::dev.off(), add = TRUE)
    ok <- plotDistribution(a, a$results$plot, distSpec(distName))
    stopifnot(isTRUE(ok))
    file
}
