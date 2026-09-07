GeometricDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "GeometricDistributionClass",
  inherit = GeometricDistributionBase,
  private = list(
    .spec = function() distSpec("geom"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
