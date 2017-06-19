features_prefix = "featureSelection_"

source("feature selection/all_features.R")
source("feature selection/feature_selection.R")



## UI Elements
featureSelection_tabFeatureSelection = initFeatureSelectionUI()
featureSelection_tabFilterFeatures = tabPanel(title = "Filter Feature", "...")
featureSelection_tabFinalFeatures = tabPanel(title = "Final Features", "...")

feature_selection_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 8),
  featureSelection_tabAllFeatures,
  featureSelection_tabFilterFeatures,
  featureSelection_tabFeatureSelection,
  featureSelection_tabFinalFeatures
)
