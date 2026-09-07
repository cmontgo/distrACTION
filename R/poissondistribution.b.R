PoissonDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "PoissonDistributionClass",
  inherit = PoissonDistributionBase,
  private = list(
    .spec = function() distSpec("poisson"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
