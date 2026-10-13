# distrACTION 1.3.0

Released from `release/1.3`. This is the first release with a changelog;
versions before it have no recorded notes.

A maintenance release. No new distributions and no new options, but it changes
how every plot is drawn and it starts rejecting some parameter sets that earlier
versions silently accepted — so read the first section before updating a course
that depends on it.

## Changes you may notice

**The hypergeometric now reports invalid parameters instead of correcting
them.** If you entered more successes than the population holds (K > N), or a
sample larger than the population (n > N), earlier versions quietly replaced
your value with N and answered that question instead. The Input table showed the
replaced value, but nothing said it had been changed. Those parameter sets now
produce an explanatory error.

*This affects saved analyses.* An `.omv` file holding, say, N = 20, K = 30 used
to display a number; it will now display an error. The number it used to display
was the answer to a different question.

**Count parameters must be whole numbers.** Binomial *Size* and hypergeometric
*N*, *K* and *n* are now integer options. Entering 10.5 previously filled the
results table with `NaN` and no explanation, because the underlying density is
undefined for fractional counts.

Degrees of freedom are deliberately unaffected and remain real-valued — the
Welch–Satterthwaite correction produces fractional df routinely, and checking a
Welch *t*-test is a common reason to open the t analysis.

**The geometric distribution's minimum success probability is now 0.0001**
(was effectively unbounded). Values below that describe a distribution spread
over tens of thousands of outcomes, and the module used to attempt to draw every
one of them.

**Validation runs before results are computed.** An invalid interval
(x2 ≤ x1) previously wrote a negative probability into the results table before
reporting the problem.

## Plots

Every plot is drawn from shared rules now rather than per-distribution
arithmetic, so these apply across all eight analyses.

* **The t distribution plots correctly for negative noncentrality.** With
  δ ≤ −1 the plotted range used to collapse to zero width or invert, giving a
  blank panel. The results table was unaffected.
* **χ² with df < 2 and F with df1 < 2 are readable.** Both densities are
  infinite at zero, which drove the y-axis to thousands and flattened the curve
  — and the shaded probability with it — onto the axis. χ² with 1 df is the
  commonest χ² there is. The axis is now capped so the body of the distribution,
  and the area you asked for, are visible.
* **The x-axis no longer overflows with labels.** Only the Poisson used to thin
  its tick labels. A binomial with Size 100 drew 101 overlapping numbers, a χ²
  with df 100 drew 107, a geometric with p = 0.01 drew 1,146.
* **The plotted range always includes what you asked about.** Values of x1 or x2
  outside the drawn range used to be shaded off-screen.
* **A large λ no longer draws an empty Poisson.** The range follows the mass
  rather than counting up from zero.

## Additions

* A footnote now explains why a mean or standard deviation is undefined
  (t with df ≤ 1, F with df2 ≤ 2, and so on) rather than showing a bare `NaN`.
* Validation messages name the parameter at fault.

## Under the hood

Nothing here changes what the module computes.

* The eight distribution backends, previously 67–86% identical copies of one
  another totalling about 3,000 lines, now share one engine. Each analysis is a
  short shim naming its distribution.
* A test suite run on every push. It includes a
  characterization fixture recording the results of 3,732 option combinations
  from v1.2.2, so the refactor could be shown not to move any number it should
  not have moved.
* The generated `R/*.h.R` headers, which had drifted from the `jamovi/*.a.yaml`
  files they come from, were brought back into line.

## Known limitations

* The geometric distribution counts failures before the first success
  (R's `dgeom` convention), so its support starts at 0. Texts that define it as
  the number of trials start at 1.
* Far upper-tail probabilities on the continuous distributions lose precision,
  because P(X ≥ x) is computed as 1 − P(X ≤ x). For a standard normal the error
  reaches about 7% at x = 8, and beyond roughly x = 8.2 the result is reported
  as 0 rather than the true value (P(X ≥ 9) is 1.13e-19, reported as 0).
  Anything in the range a course would use is unaffected. Fixing it means
  computing the upper tail directly, which changes recorded results and so is
  held for its own release.
