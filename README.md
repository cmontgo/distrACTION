# distrACTION

[![version](https://img.shields.io/badge/version-1.3.0-blue)](NEWS.md)
[![license](https://img.shields.io/badge/license-GPL--3-blue)](https://www.gnu.org/licenses/gpl-3.0)
[![tests](https://github.com/cmontgo/distrACTION/actions/workflows/tests.yml/badge.svg)](https://github.com/cmontgo/distrACTION/actions/workflows/tests.yml)

**Quantiles and probabilities of continuous and discrete distributions** — a
[jamovi](https://www.jamovi.org) module for teaching and checking distribution
work. Enter the parameters of a distribution and get the probability of a
region, or the quantile cutting off a given area, together with a plot of the
distribution with that region shaded.

Built for statistics teaching at the University of Bern.

## Installing

**From the jamovi library** — in jamovi, open the modules menu (**+**, top
right), choose **jamovi library**, find *distrACTION*, and click **Install**.

**From a module file** — download a `.jmo` built for your jamovi version, then
in the modules menu choose **Install from file** and select it.

**From source** — needs R and
[jmvtools](https://dev.jamovi.org/tuts0101-getting-started.html):

```r
install.packages("jmvtools", repos = "https://repo.jamovi.org")
jmvtools::install()     # from a clone of this repository
```

Requires **jamovi 1.0.8 or later**.

## The analyses

All eight live under **distrACTION** in the jamovi *Analyses* ribbon, split into
*Continuous distributions* and *Discrete distributions*.

| Distribution | Parameters | Probability modes | Quantile modes |
| --- | --- | --- | --- |
| Normal | Mean, SD | ≤ x1, ≥ x1, interval | cumulative, central |
| t | df, δ (noncentrality) | ≤ x1, ≥ x1, interval | cumulative, central |
| χ² | df, λ (noncentrality) | ≤ x1, ≥ x1, interval | cumulative |
| F | df1, df2, λ | ≤ x1, ≥ x1, interval | cumulative |
| Binomial | Size, Probability | = x1, ≤ x1, ≥ x1, interval | cumulative, central |
| Poisson | λ | = x1, ≤ x1, ≥ x1, interval | cumulative |
| Geometric | Probability | = x1, ≤ x1, ≥ x1, interval | cumulative, central |
| Hypergeometric | N, K, n | = x1, ≤ x1, ≥ x1, interval | cumulative, central |

Each analysis reports the probability or quantile you asked for, the
distribution's mean and standard deviation, and a plot with the region shaded
and the quantiles marked.

**Conventions worth knowing.** The geometric distribution counts *failures
before the first success*, so its support starts at 0 — some texts define it as
the number of trials and start at 1. Tail probabilities on the discrete
distributions include their endpoint: P(X ≥ x1) counts x1 itself. Degrees of
freedom may be fractional, so a Welch–Satterthwaite value can be entered
directly.

## Changes

See [NEWS.md](NEWS.md). Version 1.3.0 changes how every plot is drawn and
rejects some parameter sets earlier versions silently accepted — worth reading
before updating a course that depends on it.

## Development

```sh
R CMD INSTALL .                                   # needs jmvcore, R6, ggplot2
Rscript -e 'library(distrACTION)
            testthat::test_dir("tests/testthat")'
```

The suite also runs without installing anything — the four smoke tests that
drive the real jamovi classes skip themselves, the rest run:

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

See [tests/README.md](tests/README.md) for how it is laid out and why. In short:
the eight analyses share one engine in `R/distribution-core.R`, described by a
table of per-distribution specs in `R/distribution-specs.R`; each
`R/<dist>.b.R` is a shim naming its spec. Adding a distribution means adding a
spec entry and the three `jamovi/*.yaml` files, not writing a backend.

`R/*.h.R` are generated from `jamovi/*.a.yaml` by `jmvtools::prepare()` — edit
the YAML, not the headers.

## Authors

Boris Mayer, Michael Rihs and Cameron Redsell-Montgomerie.
Maintained by Boris Mayer (<boris.mayer@unibe.ch>).

## Citation

> Mayer, B., Rihs, M., & Redsell-Montgomerie, C. (2026). *distrACTION:
> Quantiles and probabilities of continuous and discrete distributions*
> (Version 1.3.0) [jamovi module]. https://github.com/methodenlehre/distrACTION

## Issues

Bug reports and feature requests:
<https://github.com/methodenlehre/distrACTION/issues>

## Licence

GPL-3. See <https://www.gnu.org/licenses/gpl-3.0>.
