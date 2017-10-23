feature_clusters_prefix = paste0(features_prefix, "featureClusters_")



## UI Elements
initFeatureClustersUI = function(){

  # row - 1 : Options
  id = paste0(feature_clusters_prefix, "clusterMethod")
  select_method = selectInput(
    id,
    "Methdods",
    c(
      "ward.D" = "ward.D",
      "ward.D2" = "ward.D2",
      "single" = "single",
      "complete" = "complete",
      "average" = "average",
      "mcquitty" = "mcquitty",
      "median" = "median",
      "centroid" = "centroid"
    ),
    selected = "complete"
  )
  
  id = paste0(feature_clusters_prefix, "height")
  numeric_height = numericInput(id, "Height", value=0.5, min=0, max=12)
  
  id = paste0(feature_clusters_prefix, "performClustering")
  button_do_clustering = actionButton(id, label="Build Clusters")
  
  row_1 = fluidRow(column(2, select_method),
                   column(2, numeric_height),
                   column(2, button_do_clustering))
    
  
  # row - 2 : Clusters
  id = paste0(feature_clusters_prefix, "clusters")
  ui_clusters = uiOutput(id)
  row_2 = fluidRow(ui_clusters)
  
  
  tabPanel(title = "Feature Clusters", row_1, row_2)
  
}



## subsetting those rows within the time-period of 
## product selected
filterByProductPeriod = function(data_series){
  
  data_series[,"month"] = sapply(data_series$date, 
                                 function(date){ 
                                   as.numeric(strsplit(as.character(date),"-")[[1]][2]) 
                                 }
  )
  data_series[,"year"] = sapply(data_series$date, 
                                function(date){ 
                                  as.numeric(strsplit(as.character(date),"-")[[1]][1])
                                }
  )
  
  data_series = data_series[(data_series$year > product_start_date[1]) |
                              (data_series$year == product_start_date[1] & 
                                 data_series$month >= product_start_date[2]), ]
  
  data_series = data_series[(data_series$year < product_end_date[1]) |
                              (data_series$year == product_end_date[1] & 
                                 data_series$month <= product_end_date[2]), ]
  
  data_series = data_series[ ,!(names(data_series) %in% c("month","year"))]
  dim(data_series)
  
  data_series
}



## Server Function
readDataForClustering = function(input, output, session) {
  print("Feature Selection :: Feature Selection :: readDataForClustering() :: INIT")
  
  input_value = reactiveValuesToList(input)
  selected_vars = vector('character')
  for (key in names(input_value)) {
    #print(key)
    if (grepl(paste0(all_features_prefix, "fId"),key) && 
        is.logical(input_value[[key]]) && input_value[[key]] == TRUE) {
      selected_vars = c(selected_vars, unlist(strsplit(key, "\\|"))[3])
    }
  }
  
  config_data = meta_data[meta_data$series_id %in% selected_vars, ]
  
  # Y
  revenue_file = paste0(data_folder, "/", product_line, "/Revenue.csv")
  data_revenue = read.csv(revenue_file, header = TRUE, sep = ",")
  data = data_revenue[, c("month", "t")]
  
  # X
  for (sub_category_id in unique(config_data$sub_category_id)) {
    category_name = unique(config_data[config_data$sub_category_id == sub_category_id, "category_name"])
    sub_category_name = unique(config_data[config_data$sub_category_id ==
                                             sub_category_id, "sub_category_name"])
    
    
    file = paste0( FRED_folder, "/", as.character(category_name), 
                   "/", as.character(sub_category_name) )
    
    if (sa_OR_nsa == "Not Seasonally Adjusted") {
      file = paste0(file, "_nsa.csv")
    } else{
      file = paste0(file, "_sa.csv")
    }
    print(paste0("Reading file : ", file))
    data_series = read.csv(file, header = TRUE, sep = ",")
    
    data_series = filterByProductPeriod(data_series)
    
    series_vars = intersect(names(data_series), selected_vars)
    if(is.null(dim(data_series[, series_vars]))){
      ## there is just one column from the series
      data[,series_vars] = data_series[, series_vars]
    }else{
      data = cbind(data, data_series[, series_vars])  
    }
    
  }
  
  print("Feature Selection :: Feature Selection :: readDataForClustering() :: EXIT")
  
  data = data[,!names(data) %in% c("month", "t")]
  doHClust(input, output, session, data)
  
}



doHClust = function(input, output, session, data){
  
  print("Feature Selection :: Feature Selection :: doHClust() :: START")
  
  sd_zero_cols = apply(data, 2, function(col){ sd(col)==0 })
  data = data[, !(sd_zero_cols)]
  
  cor_matrix = cor(scale(data))
  abs_cor_matrix = abs(cor_matrix)
  d = dist(abs_cor_matrix)
  
  method = input[[paste0(feature_clusters_prefix, "clusterMethod")]]
  h = hclust(d, method=method)
  
  height = input[[paste0(feature_clusters_prefix, "height")]]
  cuts = cutree(h, h=height)
  
  clusters = as.numeric(names(which(table(cuts)>1)))
  
  
  output[[paste0(feature_clusters_prefix, "clusters")]] = renderUI({
    
    lapply(unique(clusters), function(cluster_no){
      
      cluster_members = names(cuts[cuts == cluster_no])
      
      ui_members = lapply(cluster_members, function(var){
        
        select_var_id = paste0(feature_clusters_prefix, var, "|select")
        var_name = meta_data[meta_data$series_id==var, "title"]
        input_select_var = checkboxInput(select_var_id, label=var_name, value=TRUE)
        observeEvent(input[[select_var_id]],{
          group_id = meta_data[meta_data$series_id==var, "sub_category_id"]
          id = paste0(all_features_prefix, "fId|", group_id, "|", var)
          if(input[[select_var_id]]){
            updateTextInput(session, id, value=TRUE)
          }else{
            updateTextInput(session, id, value=FALSE)
          }
        })
        
        input_select_var
      })
      
      cluster_header = tags$h4(paste0("Cluster#:",cluster_no))
      
      cluster_corr = renderTable(cor(data[,cluster_members]))
      
      fluidRow(cluster_header, ui_members, cluster_corr)
    })
    
  })
  
  print("Feature Selection :: Feature Selection :: doHClust() :: EXIT")
  
}



attachFeatureClustersObservers = function(input, output, session){
  
  id = paste0(feature_clusters_prefix, "performClustering")
  observeEvent(input[[id]], {
    readDataForClustering(input, output, session)
  })
  
}