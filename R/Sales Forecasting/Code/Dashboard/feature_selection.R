source("feature_selection_all_features.R")
#source("feature_selection_filter_features.R")


feature_selection_all_features = tabPanel(title="Features", 
                                          actionButton(inputId="all_features_show", label="Show"),
                                          actionButton(inputId="all_features_select", label="Select Checked"),
                                          uiOutput("all_features_box")
                                          )

feature_selection_filter_features = tabPanel(title="Filter Features","...")

feature_selection_selection = tabPanel(title="Feature Selection","...")

feature_selection_final = tabPanel(title="Final Features","...")


feature_selection_navbar = navlistPanel(
  well=FALSE,
  widths=c(2,8), 
  feature_selection_all_features,
  feature_selection_filter_features,
  feature_selection_selection,
  feature_selection_final
)
