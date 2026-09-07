Chi2DistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "Chi2DistributionClass",
  inherit = Chi2DistributionBase,
  private = list(
    .spec = function() distSpec("chi2"),
    .run  = function() runDistribution(self, private$.spec()),
    .plot = function(image, ...) plotDistribution(self, image, private$.spec())))
