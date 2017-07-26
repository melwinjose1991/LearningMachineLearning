features_prefix = "featureSelection_"

source("feature selection/all_features.R")
source("feature selection/feature_clusters.R")
source("feature selection/filters_wrappers.R")
source("feature selection/feature_selection.R")



## UI Elements
featureSelection_tabAllFeatures = initAllFeaturesUI()
featureSelection_tabFeatureClusters = initFeatureClustersUI()
featureSelection_tabFeatureSelection = initFeatureSelectionUI()
#featureSelection_tabFinalFeatures = tabPanel(title = "Final Features", "...")

feature_selection_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 8),
  featureSelection_tabAllFeatures,
  featureSelection_tabFeatureClusters,
  featureSelection_tabFeatureSelection
)
