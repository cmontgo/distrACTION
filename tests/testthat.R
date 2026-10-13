library(testthat)

# The suite is split so that most of it runs without installing the package.
# Only test-analysis-smoke.R needs the R6 analysis classes, and so jmvcore and
# an installed distrACTION; it skips itself when they are absent:
#
#   test-probability-math.R  pure stats::d/p/q calls - no jamovi needed
#   test-plot-window.R       pure plot-window/grid/axis logic - no jamovi needed
#   test-analysis-smoke.R    end-to-end through jmvcore - skipped if absent
#
# Run everything:  Rscript -e 'testthat::test_dir("tests/testthat")'

test_check("distrACTION")
