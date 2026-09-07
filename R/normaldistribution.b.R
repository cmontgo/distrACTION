NormaldistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "NormaldistributionClass",
  inherit = NormaldistributionBase,
  private = list(
    .spec = function() distSpec("normal"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
