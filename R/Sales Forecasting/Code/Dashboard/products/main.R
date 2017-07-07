# products > main.R

products_prefix = "products_"
data_folder = paste0("../../Data")
data_folders_to_skip = c("External Data")

products = vector('character')
product_line = "2401" # default
product_data = vector('numeric')
product_data_column = "orders_rcvd"
columns_to_skip = c("period_id", "month", "year", "month_str", "t")




## UI Elements
getProductData = function(product, column_name){
  product_folder = paste0(data_folder,"/",product)
  revenue_file = list.files(product_folder, full.names=TRUE)
  product_df = read.csv(revenue_file)
  product_df[,column_name]
}


getProductTabPanel = function(product){
  print(paste0("getProductTabPanel() :: INIT"))
  
  product_folder = paste0(data_folder,"/",product)
  revenue_file = list.files(product_folder, full.names=TRUE)
  product_df = read.csv(revenue_file)
  y_df = product_df[,!names(product_df) %in% columns_to_skip]
  revenue_cols = names(y_df)
  
  # To display revenue columns
  button_id = paste0(products_prefix, "pId|",product,"|Columns")
  radio_buttons = radioButtons(button_id, label="Series within the Product", inline=TRUE,
                              choiceNames=revenue_cols, choiceValues=revenue_cols)
  row_1 = fluidRow(radio_buttons)
  
  # To display plots
  plot_id = paste0(products_prefix, product, "|plot")
  output_plot = plotOutput(plot_id)
  row_2 = fluidRow(output_plot)
  
  # To Select product line
  button_select_product_id = paste0(products_prefix, "pId|", product, "|Select")
  button_select_product = actionButton(button_select_product_id, "Select")
  row_3 = fluidRow(button_select_product)
  
  tabPanel(title=product, row_1, row_2, row_3)
  
}



getProductsUI = function(){
  print(paste0("getProductUI() :: INIT"))
  
  dirs = list.dirs(data_folder, recursive=FALSE)
  products = vector('character')
  for(dir in dirs){
    
    parts = unlist(strsplit(dir,"/"))
    product = tail(parts,1)
    if(product %in% data_folders_to_skip){
      next
    }
    print(product)
    products = c(products, product)
  }
  
  lapply(products, getProductTabPanel)
  ##TODO: FIX THIS
}



products_navbar = navlistPanel(
  well = FALSE,
  widths = c(2, 8),
  getProductTabPanel("2401"),
  getProductTabPanel("2404")
)



## Server Functions
getProducts = function(){
  print(paste0("getProducts() :: INIT"))
  
  dirs = list.dirs(data_folder, recursive=FALSE)
  products = vector('character')
  for(dir in dirs){
    
    parts = unlist(strsplit(dir,"/"))
    product = tail(parts,1)
    if(product %in% data_folders_to_skip){
      next
    }
    print(product)
    products = c(products, product)
  }
  products
}



attachProductsObservers = function(input, output, session, reactive_vars){
  
  products <<- getProducts()
  
  lapply(products, FUN=function(product){
    
    button_id = paste0(products_prefix, "pId|",product,"|Columns")
    observeEvent(input[[button_id]],{
      plot_id = paste0(products_prefix, product, "|plot")
      column_name =  input[[button_id]]
      output[[plot_id]] = renderPlot({
        y = getProductData(product, column_name)
        start_year = min(getProductData(product, "year"))
        time_series = ts(y, frequency=12, start=c(start_year, 1))
        plot(time_series, xlab="Time", ylab=column_name)
      })
    })
    
    button_select_product_id = paste0(products_prefix, "pId|", 
                                      product, "|Select")
    observeEvent(input[[button_select_product_id]],{
      column_name =  input[[button_id]]
      
      product_line <<- product
      product_data <<- getProductData(product, column_name)
      product_data_column <<- column_name
      
    })
    
  })
  
}




