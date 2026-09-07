TDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "TDistributionClass",
  inherit = TDistributionBase,
  private = list(
    .spec = function() distSpec("t"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
