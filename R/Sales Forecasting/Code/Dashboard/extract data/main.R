extract_tab_prefix = "extractDataTab_"
external_data_folder = paste0(data_folder,"/External Data")

source("extract data/FRED.R")



## UI Elements
extract_fred_tab = getFREDUI()

extract_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 10),
  extract_fred_tab
)