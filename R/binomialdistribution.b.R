BinomialDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "BinomialDistributionClass",
  inherit = BinomialDistributionBase,
  private = list(
    .spec = function() distSpec("binomial"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
