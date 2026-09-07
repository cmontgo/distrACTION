HypergeometricDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "HypergeometricDistributionClass",
  inherit = HypergeometricDistributionBase,
  private = list(
    .spec = function() distSpec("hyper"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
