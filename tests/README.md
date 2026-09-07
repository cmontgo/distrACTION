# Tests

## Running

```r
testthat::test_dir("tests/testthat")     # no installation needed
testthat::test_local()                   # once the package is installed
```

Roughly 520 assertions, a few seconds. Only the smoke tests need jamovi; they
skip themselves when jmvcore is absent (it is not on CRAN).

## Layout

| File | Needs | Covers |
| --- | --- | --- |
| `test-characterization.R` | base R | every Results-table number, against the v1.2.2 fixture |
| `test-probability-math.R` | base R | the arithmetic itself: critical values, tail identities, moments |
| `test-validation.R` | base R | what gets rejected, and what deliberately does not |
| `test-plot-window.R` | base R | invariants the plot must satisfy |
| `test-core-generics.R` | ggplot2 | `runDistribution()` / `plotDistribution()` end to end |
| `test-analysis-smoke.R` | jmvcore | the real R6 classes run and render |

Helpers: `helper-core.R` sources `R/` directly, `helper-mock-jamovi.R` stands in
for the jmvcore objects the generics touch, and `helper-reference-run.R` holds
the v1.2.2 transcription the fixture was built from.

## The characterization fixture

`fixtures/reference-values.rds` records what v1.2.2 computed for 3,732 option
combinations — every analysis crossed with every distribution mode and every
quantile mode, over a parameter grid. It is the safety net for the 1.3 refactor:
the arithmetic was correct before, so the job was to prove it did not move.

Regenerate with `Rscript tools/make-reference-fixture.R`. That script computes
every value twice by independent routes — the transcription in
`helper-reference-run.R`, and direct summation over the support (discrete) or
complementary-tail routines and Monte Carlo (continuous) — and refuses to write
the file unless the two agree. A typo in the transcription cannot become the
specification.

Probabilities and quantiles are checked exactly; moments statistically, with the
tolerance scaled by the Monte Carlo standard error rather than relatively, since
a true mean of zero admits no relative tolerance.

`test-characterization.R` also asserts that the *only* fixture rows v1.3 no
longer computes are the hypergeometric ones with K or n exceeding N — the case
v1.2.2 silently clamped and v1.3 rejects. Anything else dropping out is a
regression, not a design decision.

## Notes

`helper-mock-jamovi.R` builds its image as an environment rather than a list
with an S3 `$` method. `plotDistribution()` lives in a different environment, so
dispatch would not reach it and `image$state` would silently read as `NULL` —
which looks exactly like a plot that legitimately has no state yet.

`test-known-issues.R` and `helper-distributions.R` are gone. They existed
because the window and axis-break arithmetic was buried inside eight copies of
`.run()` with nothing to call, so the invariants had to be written against a
transcription and pinned with `expect_failure()`. Both are now real assertions
against `R/distribution-core.R` in `test-plot-window.R`.
