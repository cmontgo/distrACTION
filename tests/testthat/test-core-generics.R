# ---------------------------------------------------------------------------
# runDistribution() and plotDistribution() end to end, through the mock jamovi
# objects in helper-mock-jamovi.R.
#
# This is the coverage the module has never had: before the refactor the plot
# code was only reachable by opening the analysis in jamovi and looking at it.
# ---------------------------------------------------------------------------

converted <- function() {
    # Only assert against analyses that have actually been moved onto the core.
    Filter(function(n) {
        f <- testthat::test_path("..", "..", "R",
                                 paste0(switch(n, normal = "normal", t = "t", chi2 = "chi2",
                                                f = "f", binomial = "binomial",
                                                poisson = "poisson", geom = "geometric",
                                                hyper = "hypergeometric"),
                                        "distribution.b.R"))
        file.exists(f) && any(grepl("runDistribution", readLines(f, warn = FALSE)))
    }, names(.distSpecs))
}

# Representative options per distribution, chosen to be valid everywhere.
sane <- list(
    normal   = list(dp1 = 0,  dp2 = 1,  x1 = 1.96, x2 = 2.5),
    t        = list(dp1 = 10, dp2 = 0,  x1 = 2.228, x2 = 2.5),
    chi2     = list(dp1 = 3,  dp2 = 0,  x1 = 7.81, x2 = 12),
    f        = list(dp1 = 3,  dp2 = 10, dp3 = 0, x1 = 3.71, x2 = 6),
    binomial = list(dp1 = 10, dp2 = 0.5, x1 = 6, x2 = 8),
    poisson  = list(dp1 = 2,  x1 = 3, x2 = 5),
    geom     = list(dp1 = 0.5, x1 = 3, x2 = 5),
    hyper    = list(dp1 = 20, dp2 = 10, dp3 = 5, x1 = 2, x2 = 4)
)

test_that("runDistribution fills both tables and sets plot state", {
    for (nm in converted()) {
        spec <- distSpec(nm)
        got  <- mockRun(nm, sane[[nm]])

        # one Input row per parameter line
        expect_length(got$inputs, length(spec$paramLines(sane[[nm]])))
        expect_true(nzchar(got$inputs[[1]]$ParametersColumn), info = nm)
        expect_match(got$inputs[[1]]$DistributionFunctionColumn, "^x1 = ", info = nm)
        expect_match(got$inputs[[1]]$QuantileFunctionColumn, "^p = ", info = nm)

        # the Results row carries real numbers
        expect_true(is.numeric(got$outputs$DistributionResultColumn), info = nm)
        expect_true(is.finite(got$outputs$DistributionResultColumn), info = nm)
        expect_true(is.numeric(got$outputs$MeanColumn), info = nm)

        # plot state is a named list, not values hidden in data-frame cells
        expect_type(got$state, "list")
        expect_setequal(names(got$state),
                        c("x", "density", "shaded", "breaks", "yCap",
                          "discrete", "showDist", "showQuant", "markers"))
        expect_equal(length(got$state$x), length(got$state$density))
        expect_equal(length(got$state$x), length(got$state$shaded))
        expect_identical(got$state$discrete, spec$discrete)
    }
})

test_that("the reported probability matches distributionResults directly", {
    for (nm in converted()) {
        o <- sane[[nm]]
        for (dm in distSpec(nm)$distModes) {
            got  <- mockRun(nm, c(o, list(DistributionFunctionType = dm)))
            want <- distributionResults(nm, o, distMode = dm, quantMode = NA)$probability
            expect_equal(got$outputs$DistributionResultColumn, want, info = paste(nm, dm))
        }
    }
})

test_that("every mode of every converted analysis renders a plot", {
    skip_if_not_installed("ggplot2")
    failures <- character()
    try_render <- function(nm, o, what) {
        r <- tryCatch({ mockPlot(nm, o); NULL },
                      error = function(e) conditionMessage(e))
        if (!is.null(r)) failures <<- c(failures, paste0(nm, " [", what, "]: ", r))
    }
    for (nm in converted()) {
        spec <- distSpec(nm)
        for (dm in spec$distModes)
            for (qm in spec$quantModes)
                try_render(nm, c(sane[[nm]], list(DistributionFunctionType = dm,
                                                  QuantileFunctionType = qm)),
                           paste(dm, qm))
        try_render(nm, c(sane[[nm]], list(DistributionFunction = FALSE)), "no probability")
        try_render(nm, c(sane[[nm]], list(QuantileFunction = FALSE)),     "no quantile")
    }
    if (length(failures)) fail(paste(failures, collapse = "\n")) else succeed()
})

test_that("shading covers exactly the requested region", {
    for (nm in converted()) {
        spec <- distSpec(nm)
        o <- sane[[nm]]
        for (dm in spec$distModes) {
            st <- mockRun(nm, c(o, list(DistributionFunctionType = dm)))$state
            kept <- st$x[!is.na(st$shaded)]
            if (!length(kept)) next
            switch(dm,
                is       = expect_true(all(kept == o$x1), info = paste(nm, dm)),
                lower    = expect_true(all(kept <= o$x1), info = paste(nm, dm)),
                higher   = expect_true(all(kept >= o$x1), info = paste(nm, dm)),
                interval = expect_true(all(kept >= o$x1 & kept <= o$x2),
                                       info = paste(nm, dm)))
        }
    }
})

test_that("an undefined moment is explained, not left as a bare NaN", {
    # t with 1 df has no mean and no SD; the columns are typed `number` and
    # cannot hold "undefined (df <= 1)", so the reason goes in a footnote.
    got <- mockRun("t", list(dp1 = 1, dp2 = 0, x1 = 0, x2 = 1))
    expect_true(is.nan(got$outputs$MeanColumn))
    expect_true(is.nan(got$outputs$SDColumn))
    cols <- vapply(got$notes, function(n) n$col, "")
    expect_setequal(cols, c("MeanColumn", "SDColumn"))
    expect_match(got$notes[[1]]$text, "undefined for df")

    # df of 4 has a mean but no SD
    got <- mockRun("t", list(dp1 = 4, dp2 = 0, x1 = 0, x2 = 1))
    expect_true(is.finite(got$outputs$MeanColumn))
    expect_length(got$notes, 0)

    # F with df2 = 3: mean defined, SD not
    got <- mockRun("f", list(dp1 = 3, dp2 = 3, dp3 = 0, x1 = 1, x2 = 2))
    expect_true(is.finite(got$outputs$MeanColumn))
    expect_true(is.nan(got$outputs$SDColumn))
    expect_identical(vapply(got$notes, function(n) n$col, ""), "SDColumn")

    # a well-behaved analysis gets no footnotes at all
    expect_length(mockRun("normal", list(dp1 = 0, dp2 = 1))$notes, 0)
})
