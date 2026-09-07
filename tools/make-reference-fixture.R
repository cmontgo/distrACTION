#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Generate tests/testthat/fixtures/reference-values.rds - the characterization
# snapshot the 1.3 refactor has to reproduce exactly.
#
# Two independent paths compute every value:
#   1. reference_run()  - a line-for-line transcription of the v1.2.2 .b.R files
#   2. oracle_run()     - the same quantities from first principles, by direct
#                         summation over the support (discrete) or by
#                         complementary-tail / integration routines (continuous)
#
# The fixture is only written if the two agree. A transcription typo therefore
# cannot become the specification.
#
# Usage:  Rscript tools/make-reference-fixture.R
# ---------------------------------------------------------------------------

source("tests/testthat/helper-reference-run.R")

set.seed(20260907)
TOL        <- 1e-9    # probabilities and quantiles: exact agreement
MOMENT_TOL <- 0.02    # moments: Monte Carlo, 2% relative or 5 standard errors
MC_N       <- 4e5

# --- independent oracle ----------------------------------------------------

# Full integer support of each discrete distribution, wide enough to hold
# essentially all the mass.
oracle_support <- function(dist, o) {
    switch(dist,
        binomial = 0:o$dp1,
        poisson  = 0:max(50, ceiling(o$dp1 + 12 * sqrt(o$dp1))),
        geom     = 0:max(50, ceiling(qgeom(1 - 1e-12, o$dp1))),
        hyper    = max(0, o$dp3 - (o$dp1 - o$dp2)):min(o$dp3, o$dp2),
        stop("not discrete: ", dist))
}

oracle_run <- function(dist, o, distMode = NA, quantMode = NA) {
    o  <- reference_clamp(dist, o)
    fn <- reference_fns[[dist]]
    discrete <- reference_discrete[[dist]]

    probability <- NA_real_
    if (!is.na(distMode)) {
        if (discrete) {
            s <- oracle_support(dist, o)
            d <- fn$d(s, o)
            probability <- switch(distMode,
                is       = sum(d[s == o$x1]),
                lower    = sum(d[s <= o$x1]),
                higher   = sum(d[s >= o$x1]),
                interval = sum(d[s >= o$x1 & s <= o$x2]))
        } else {
            # complementary tail via lower.tail = FALSE, a different routine in
            # R's C code than 1 - p(x), and the integral for the interval case
            upper <- switch(dist,
                normal = pnorm(o$x1, o$dp1, o$dp2, lower.tail = FALSE),
                t      = pt(o$x1, o$dp1, o$dp2, lower.tail = FALSE),
                chi2   = pchisq(o$x1, o$dp1, o$dp2, lower.tail = FALSE),
                f      = pf(o$x1, o$dp1, o$dp2, o$dp3, lower.tail = FALSE))
            probability <- switch(distMode,
                lower    = 1 - upper,
                higher   = upper,
                interval = tryCatch(
                    stats::integrate(function(z) fn$d(z, o), o$x1, o$x2,
                                     rel.tol = 1e-10)$value,
                    error = function(e) NA_real_))
        }
    }

    quantile <- quantileLower <- quantileUpper <- NA_real_
    if (!is.na(quantMode)) {
        if (identical(quantMode, "cumulative")) {
            quantile <- fn$q(o$p, o)
        } else {
            lo <- (1 - o$p) / 2
            quantileLower <- fn$q(lo, o)
            quantileUpper <- fn$q(1 - lo, o)   # note: 1-lo, not lo+p
        }
    }

    # Moments: exact summation over the support for discrete, Monte Carlo for
    # continuous. Numerical integration is not a usable oracle here - heavy
    # tails (t with small df, F with small df2) make adaptive quadrature give
    # confident nonsense, so it gets checked statistically instead. See the
    # split tolerances in the comparison loop below.
    mom <- if (discrete) {
        s <- oracle_support(dist, o); d <- fn$d(s, o)
        mu <- sum(s * d); list(mean = mu, sd = sqrt(sum((s - mu)^2 * d)))
    } else {
        n <- MC_N
        smp <- switch(dist,
            normal = rnorm(n, o$dp1, o$dp2),
            t      = rt(n, o$dp1, o$dp2),
            chi2   = rchisq(n, o$dp1, o$dp2),
            f      = rf(n, o$dp1, o$dp2, o$dp3))
        list(mean = mean(smp), sd = sd(smp))
    }
    list(probability = probability, quantile = quantile,
         quantileLower = quantileLower, quantileUpper = quantileUpper,
         mean = mom$mean, sd = mom$sd)
}

# --- parameter sweep -------------------------------------------------------

grids <- list(
    normal   = expand.grid(dp1 = c(0, 100), dp2 = c(1, 15), x1 = c(-1.96, 0, 1.96),
                           x2 = 2.5, p = c(0.5, 0.95)),
    t        = expand.grid(dp1 = c(1, 10, 30), dp2 = c(0, 2), x1 = c(-2.228, 0, 2.228),
                           x2 = 2.5, p = c(0.5, 0.95)),
    chi2     = expand.grid(dp1 = c(1, 3, 10, 30), dp2 = c(0, 2), x1 = c(1, 3.84, 7.81),
                           x2 = 12, p = c(0.5, 0.95)),
    f        = expand.grid(dp1 = c(1, 3, 10), dp2 = c(5, 10, 30), dp3 = c(0, 2),
                           x1 = c(1, 4.96), x2 = 6, p = c(0.5, 0.95)),
    binomial = expand.grid(dp1 = c(10, 20, 100), dp2 = c(0.1, 0.5, 0.9),
                           x1 = c(0, 3, 6), x2 = 8, p = c(0.5, 0.95)),
    poisson  = expand.grid(dp1 = c(0.5, 2, 10, 100), x1 = c(0, 3, 12),
                           x2 = 15, p = c(0.5, 0.95)),
    geom     = expand.grid(dp1 = c(0.1, 0.5, 0.9), x1 = c(0, 3, 7),
                           x2 = 9, p = c(0.5, 0.95)),
    hyper    = expand.grid(dp1 = c(20, 50), dp2 = c(5, 10, 30), dp3 = c(5, 25),
                           x1 = c(0, 2, 4), x2 = 5, p = c(0.5, 0.95))
)

rows <- list(); disagreements <- list()

for (dist in names(grids)) {
    g <- grids[[dist]]
    modes <- reference_modes[[dist]]
    for (i in seq_len(nrow(g))) {
        o <- as.list(g[i, , drop = FALSE])
        for (dm in c(modes$dist, NA)) {
            for (qm in c(modes$quantile, NA)) {
                if (is.na(dm) && is.na(qm)) next
                # the module rejects x1 >= x2 in interval mode; skip those
                if (!is.na(dm) && dm == "interval" && o$x1 >= o$x2) next
                ref <- reference_run(dist, o, dm, qm)
                orc <- oracle_run(dist, o, dm, qm)
                for (k in names(ref)) {
                    a <- ref[[k]]; b <- orc[[k]]
                    if (is.na(a) && is.na(b)) next
                    exact <- k %in% c("probability", "quantile",
                                      "quantileLower", "quantileUpper")
                    if (!exact) {
                        # Moments: the transcription is authoritative about
                        # whether a moment EXISTS (NaN for t df<=1, F df2<=2,
                        # ...), and Monte Carlo cannot speak to that. Skip those,
                        # and skip the heavy-tailed cases where the sample mean
                        # has no useful precision.
                        if (is.nan(a) || is.na(b)) next
                        if (dist == "t" && o$dp1 <= 4) next
                        if (dist == "f" && o$dp2 <= 8) next
                    }
                    # Exact fields get a relative tolerance; moments get one
                    # scaled by the Monte Carlo standard error, since a true
                    # mean of zero has no meaningful relative tolerance.
                    tolAbs <- if (exact) TOL * max(1, abs(a), abs(b))
                              else max(MOMENT_TOL * abs(a),
                                       5 * orc$sd / sqrt(MC_N))
                    if (is.na(a) != is.na(b) ||
                        (is.finite(a) && is.finite(b) && abs(a - b) > tolAbs)) {
                        disagreements[[length(disagreements) + 1]] <- data.frame(
                            dist = dist, distMode = dm, quantMode = qm,
                            field = k, transcription = a, oracle = b,
                            stringsAsFactors = FALSE)
                    }
                }
                rows[[length(rows) + 1]] <- cbind(
                    data.frame(dist = dist, distMode = dm, quantMode = qm,
                               stringsAsFactors = FALSE),
                    as.data.frame(o),
                    as.data.frame(ref))
            }
        }
    }
}

fixture <- do.call(rbind, lapply(rows, function(r) {
    for (cn in c("dp1", "dp2", "dp3", "x1", "x2", "p"))
        if (is.null(r[[cn]])) r[[cn]] <- NA_real_
    r[, c("dist", "distMode", "quantMode", "dp1", "dp2", "dp3", "x1", "x2", "p",
          "probability", "quantile", "quantileLower", "quantileUpper", "mean", "sd")]
}))

cat(sprintf("swept %d option combinations across %d analyses\n",
            nrow(fixture), length(grids)))

if (length(disagreements)) {
    d <- do.call(rbind, disagreements)
    cat("\n!! transcription and oracle disagree - fixture NOT written\n\n")
    print(utils::head(d, 40))
    cat(sprintf("\n%d disagreement(s) total\n", nrow(d)))
    saveRDS(d, "tools/disagreements.rds")
    quit(status = 1)
}

saveRDS(fixture, "tests/testthat/fixtures/reference-values.rds")
cat("both paths agree to", TOL, "- wrote tests/testthat/fixtures/reference-values.rds\n")
