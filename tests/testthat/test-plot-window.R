# ---------------------------------------------------------------------------
# Invariants the plot has to satisfy for the picture to mean anything.
#
# These now run against the real functions in R/distribution-core.R. Until the
# 1.3 refactor there was nothing to call: the window, grid and axis-break
# arithmetic lived inline inside eight copies of .run(), and these same
# assertions had to be written against a transcription and pinned as known
# failures. Every one of them is a defect the review found.
# ---------------------------------------------------------------------------

# A break vector a person can actually read on a 500px-wide jamovi image.
MAX_READABLE_BREAKS <- 30
# Mirrors .dmaxBars in R/distribution-core.R.
MAX_BARS <- 500

# Parameter sets that broke v1.2.2, plus ordinary ones that did not.
cases <- list(
    list(d = "normal",   o = list(dp1 = 0,   dp2 = 1)),
    list(d = "normal",   o = list(dp1 = 100, dp2 = 15)),
    list(d = "normal",   o = list(dp1 = 0,   dp2 = 250)),
    list(d = "t",        o = list(dp1 = 1,   dp2 = 0)),
    list(d = "t",        o = list(dp1 = 10,  dp2 = 0)),
    list(d = "t",        o = list(dp1 = 10,  dp2 = 2)),
    # noncentrality at or below -1 collapsed the old window to zero width,
    # then inverted it
    list(d = "t",        o = list(dp1 = 10,  dp2 = -1)),
    list(d = "t",        o = list(dp1 = 10,  dp2 = -2)),
    list(d = "t",        o = list(dp1 = 10,  dp2 = -3)),
    list(d = "t",        o = list(dp1 = 30,  dp2 = -5)),
    # df below 2 puts an infinite density at the old window's left edge
    list(d = "chi2",     o = list(dp1 = 1,   dp2 = 0)),
    list(d = "chi2",     o = list(dp1 = 3,   dp2 = 0)),
    list(d = "chi2",     o = list(dp1 = 30,  dp2 = 0)),
    list(d = "chi2",     o = list(dp1 = 100, dp2 = 0)),
    list(d = "chi2",     o = list(dp1 = 3,   dp2 = 20)),
    list(d = "f",        o = list(dp1 = 1,   dp2 = 10, dp3 = 0)),
    list(d = "f",        o = list(dp1 = 3,   dp2 = 10, dp3 = 0)),
    list(d = "f",        o = list(dp1 = 50,  dp2 = 50, dp3 = 0)),
    list(d = "binomial", o = list(dp1 = 10,  dp2 = 0.5)),
    list(d = "binomial", o = list(dp1 = 100, dp2 = 0.5)),
    list(d = "binomial", o = list(dp1 = 500, dp2 = 0.1)),
    list(d = "poisson",  o = list(dp1 = 2)),
    list(d = "poisson",  o = list(dp1 = 100)),
    list(d = "poisson",  o = list(dp1 = 1000)),
    list(d = "geom",     o = list(dp1 = 0.5)),
    list(d = "geom",     o = list(dp1 = 0.05)),
    list(d = "geom",     o = list(dp1 = 0.001)),
    list(d = "geom",     o = list(dp1 = 1e-4)),
    list(d = "hyper",    o = list(dp1 = 20,  dp2 = 10,  dp3 = 5)),
    list(d = "hyper",    o = list(dp1 = 500, dp2 = 250, dp3 = 200))
)

withDefaults <- function(o) {
    for (nm in c("x1", "x2", "p")) if (is.null(o[[nm]])) o[[nm]] <- c(x1 = 1, x2 = 2, p = 0.95)[[nm]]
    o
}

test_that("the plotted window is finite and increasing", {
    for (cs in cases) {
        w <- distributionWindow(distSpec(cs$d), withDefaults(cs$o))
        label <- paste(cs$d, paste(unlist(cs$o), collapse = "/"))
        expect_true(all(is.finite(w)), info = label)
        expect_gt(w[2], w[1])
    }
})

test_that("the plotted window contains the body of the distribution", {
    for (cs in cases) {
        spec <- distSpec(cs$d)
        o <- withDefaults(cs$o)
        w <- distributionWindow(spec, o)
        label <- paste(cs$d, paste(unlist(cs$o), collapse = "/"))

        if (spec$discrete) {
            # The tallest bar is always on the picture. This is the invariant
            # that matters: a Poisson with a large lambda has all its mass far
            # from zero, so a window that counts bars up from the support's
            # lower end shows nothing at all.
            s <- spec$support(o)
            support <- seq(ceiling(s[1]), min(floor(s[2]), ceiling(s[1]) + 2e5))
            mode <- support[which.max(spec$d(support, o))]
            expect_true(mode >= w[1] && mode <= w[2],
                        info = paste(label, "mode outside window"))
        } else {
            # Continuous densities can have no mode to speak of - chi-square
            # with 1 df diverges at zero, and no window can contain an
            # asymptote. The median is the meaningful anchor.
            med <- spec$q(0.5, o)
            expect_true(med >= w[1] && med <= w[2],
                        info = paste(label, "median outside window"))
        }

        # For anything that fits on one screen, the middle 90% is shown too. The
        # exceptions are distributions too spread out to draw at all (a geometric
        # with p = 1e-4 spans 115,000 outcomes), where showing the tallest bars
        # beats showing a slice of the middle.
        spread <- spec$q(0.95, o) - spec$q(0.05, o)
        if (!spec$discrete || spread <= MAX_BARS) {
            expect_lte(spec$q(0.05, o), w[2])
            expect_gte(spec$q(0.95, o), w[1])
        }
    }
})

test_that("the window always contains what the user asked about", {
    spec <- distSpec("normal")
    # x1 far into a tail: v1.2.2 drew a window of +/- 4 SD and shaded off-screen
    o <- list(dp1 = 0, dp2 = 1, x1 = 6, x2 = 7, p = 0.95,
              showDist = TRUE, distMode = "interval", showQuant = FALSE)
    w <- distributionWindow(spec, o)
    expect_lte(w[1], 6)
    expect_gte(w[2], 7)

    # a quantile beyond the body must also be reachable
    o2 <- list(dp1 = 0, dp2 = 1, x1 = 0, x2 = 1, p = 0.999999,
               showDist = FALSE, showQuant = TRUE, quantMode = "cumulative")
    expect_gte(distributionWindow(spec, o2)[2], qnorm(0.999999))
})

test_that("x-axis break counts stay readable", {
    for (cs in cases) {
        spec <- distSpec(cs$d)
        o <- withDefaults(cs$o)
        b <- distributionBreaks(spec, o, distributionWindow(spec, o))
        expect_lte(length(b), MAX_READABLE_BREAKS)
        expect_true(all(is.finite(b)), info = cs$d)
    }
})

test_that("the density grid is finite everywhere it is evaluated", {
    for (cs in cases) {
        spec <- distSpec(cs$d)
        o <- withDefaults(cs$o)
        dens <- distributionCurve(spec, o)$Prob
        expect_true(all(is.finite(dens)),
                    info = paste(cs$d, paste(unlist(cs$o), collapse = "/")))
    }
})

test_that("the grid stays small enough to allocate", {
    for (cs in cases) {
        spec <- distSpec(cs$d)
        expect_lt(nrow(distributionCurve(spec, withDefaults(cs$o))), 5000)
    }
})

test_that("the y axis is capped only where a density diverges", {
    capOf <- function(d, o) {
        spec <- distSpec(d); o <- withDefaults(o)
        distributionYCap(spec, o, distributionCurve(spec, o))
    }
    # chi-square below 2 df and F below 2 df1 both diverge at zero
    expect_true(is.finite(capOf("chi2", list(dp1 = 1, dp2 = 0))))
    expect_true(is.finite(capOf("f", list(dp1 = 1, dp2 = 10, dp3 = 0))))
    # everything well behaved is left alone
    for (cs in list(list("normal", list(dp1 = 0, dp2 = 1)),
                    list("t", list(dp1 = 10, dp2 = 0)),
                    list("chi2", list(dp1 = 3, dp2 = 0)),
                    list("chi2", list(dp1 = 30, dp2 = 0)),
                    list("f", list(dp1 = 3, dp2 = 10, dp3 = 0))))
        expect_true(is.na(capOf(cs[[1]], cs[[2]])), info = cs[[1]])
})

test_that("the cap keeps the distribution's body visible", {
    spec <- distSpec("chi2")
    o <- withDefaults(list(dp1 = 1, dp2 = 0))
    cap <- distributionYCap(spec, o, distributionCurve(spec, o))
    # tall enough to show the curve from the lower quartile onwards, and far
    # below the raw maximum the singularity produces
    expect_gt(cap, dchisq(qchisq(0.25, 1), 1))
    expect_lt(cap, max(dchisq(seq(1e-8, 8, length = 1000), 1)) / 100)
})
