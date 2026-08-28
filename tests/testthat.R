library(testthat)

# distrACTION's analysis classes need jmvcore, which is not on CRAN. The suite
# below is deliberately split so that most of it runs anywhere:
#
#   test-probability-math.R  pure stats::d/p/q calls - no jamovi needed
#   test-plot-window.R       pure plot-window/grid/axis logic - no jamovi needed
#   test-analysis-smoke.R    end-to-end through jmvcore - skipped if absent
#
# Run everything:  Rscript -e 'testthat::test_dir("tests/testthat")'

test_check("distrACTION")
