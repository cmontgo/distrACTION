# Tests

## Why this shape

The eight `R/<dist>.b.R` files are between 67% and 86% textually identical, and
each one defines exactly two functions: `.run()` and `.plot()`. All ~3,000 lines
of logic live inside those sixteen methods, which means there is currently
nothing a unit test can call. Everything is reachable only by standing up a full
jamovi analysis.

That is also why the same bug tends to exist in seven copies. The Poisson module
learned to thin its x-axis breaks when the parameter gets large; the other seven
never did.

So the suite is deliberately layered, cheapest first:

| File | Needs | Covers |
| --- | --- | --- |
| `test-probability-math.R` | base R only | the numbers in the Results table |
| `test-plot-window.R` | base R only | invariants the plot must satisfy |
| `test-known-issues.R` | base R only | open defects, pinned |
| `test-analysis-smoke.R` | jmvcore | the analyses actually run and render |

The first three run anywhere, in about a second. The fourth skips itself when
jmvcore is not installed.

## Running

```r
# no installation needed
testthat::test_dir("tests/testthat")

# or, once the package is installed
testthat::test_local()
```

## The helper layer is temporary

`helper-distributions.R` re-implements the plot-window, grid and axis-break
arithmetic that currently lives inline inside each `.run()`. That duplication is
a smell, and an intentional one: it is the scaffolding that lets the invariants
be tested today.

The intended next step is to lift that arithmetic out of the eight `.b.R` files
into shared functions in `R/` - something like `R/distribution-core.R` holding a
per-distribution spec (density, cdf, quantile, support, plot window, break rule)
plus one generic `.run()` and one generic `.plot()`. At that point:

- delete `helper-distributions.R` and point the tests at the real functions,
- the seven remaining copies of each bug collapse into one place to fix,
- adding the negative binomial becomes a table entry rather than a 400-line file.

## `test-known-issues.R`

Each block asserts the invariant that *should* hold, wrapped in
`expect_failure()` because it currently does not. The suite stays green while
the bug is open and turns red the day it is fixed - at which point delete the
wrapper and move the assertion into `test-plot-window.R`.
