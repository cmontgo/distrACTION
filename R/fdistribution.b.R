FDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "FDistributionClass",
  inherit = FDistributionBase,
  private = list(
    .spec = function() distSpec("f"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
