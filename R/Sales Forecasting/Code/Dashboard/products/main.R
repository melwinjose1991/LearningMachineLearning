# products > main.R



products_prefix = "products_"

data_folder = paste0("../../Data")
data_folders_to_skip = c("External Data")

products = vector('character')

product_line = "1234"
product_start_date = vector('integer')
product_end_date = vector('integer')
product_last_year_index = 0
product_data = vector('numeric')
product_data_column = "1234"
product_data_obeservations = 48

product_forecast_data = vector('numeric')
product_forecast_start_date = vector('numeric')
product_forecast_end_date = vector('numeric')

columns_to_skip = c("period_id", "month", "year", "month_str", "t")
product_code_mapping = list("2401"="2401 - DTR", "2404"="2404 - SPT")



## UI Elements
getProductData = function(product, column_name){
  product_folder = paste0(data_folder,"/",product)
  revenue_file = paste0(product_folder,"/Revenue.csv")
  product_df = read.csv(revenue_file)
  product_df[,column_name]
}



getProductTabPanel = function(product){
  print(paste0("getProductTabPanel() :: INIT"))
  
  product_folder = paste0(data_folder,"/",product)
  revenue_file = paste0(product_folder,"/Revenue.csv")
  product_df = read.csv(revenue_file)
  y_df = product_df[,!names(product_df) %in% columns_to_skip]
  revenue_cols = names(y_df)
  
  # To display revenue columns
  button_id = paste0(products_prefix, "pId|",product,"|Columns")
  radio_buttons = radioButtons(button_id, label="Series within the Product", inline=TRUE,
                              choiceNames=revenue_cols, choiceValues=revenue_cols, 
                              selected=revenue_cols[1])
  row_1 = fluidRow(radio_buttons)
  
  # To display plots
  plot_id = paste0(products_prefix, product, "@plot")
  plot_click_id = paste0(products_prefix, product, "|plot_click")
  plot_hover_id = paste0(products_prefix, product, "|plot_hover")
  output_plot = plotOutput(plot_id, click=plot_click_id, 
                           hover=hoverOpts(plot_hover_id,delay=250 ))
  row_2 = fluidRow(output_plot)
  
  # Obersvations to avoid & Hovered-Over observation
  id = paste0(products_prefix, product, "|avoid")
  text_avoid_obs = textInput(id, label="Exclude Observations#:")
  id = paste0(products_prefix, product, "|hovered")
  text_current_obs = uiOutput(id)
  row_3 = fluidRow(column(6, text_avoid_obs),
                   column(6, text_current_obs))
  
  # To Select product line
  button_select_product_id = paste0(products_prefix, "pId|", product, "|Select")
  button_select_product = actionButton(button_select_product_id, "Select")
  row_4 = fluidRow(button_select_product)
  
  tabPanel(title=product_code_mapping[[product]], row_1, row_2, row_3, row_4)
  
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
    
    product_folder = paste0(data_folder,"/",product)
    revenue_file = paste0(product_folder,"/Revenue.csv")
    product_df = read.csv(revenue_file)
    
    y_df = product_df[,!names(product_df) %in% columns_to_skip]
    revenue_cols = names(y_df)
    
    start_year = product_df[, "year"][1]  
    start_month = product_df[, "month"][1] 
    
    end_year = tail(product_df[, "year"], n=1)
    end_month = tail(product_df[, "month"], n=1)
    
    x = product_df$t
    y = y_df[,revenue_cols[1]]

        
    # selecting columns within product
    button_id = paste0(products_prefix, "pId|",product,"|Columns")
    observeEvent(input[[button_id]],{
      plot_id = paste0(products_prefix, product, "@plot")
      column_name =  input[[button_id]]
      y = y_df[,column_name]
      output[[plot_id]] = renderPlot({
        years = product_df[,"year"]
        period_id = product_df[,"period_id"]
        
        ticks_at = (0:(max(years)-min(years)+1))*12
        ticks_label = ticks_at
        ticks_label[1] = 1
        
        ggplot(mapping=aes(x, y)) + geom_line() +
          scale_x_continuous(breaks = ticks_at, labels=period_id[ticks_label]) +
          labs(x="Time",y=column_name) + geom_point()
          
      })
    })
    
    
    # clicks on plots
    plot_click_id = paste0(products_prefix, product, "|plot_click")
    observeEvent(input[[plot_click_id]],{
      column_name =  input[[button_id]]
      row = nearPoints(df=product_df, input[[plot_click_id]], 
                       xvar="t", yvar=column_name, threshold=10, maxpoints=1)
      
      if(nrow(row)>0){
        id = paste0(products_prefix, product, "|avoid")
        current_obs_str = input[[id]]
        
        current_obs = as.numeric(unlist(strsplit(current_obs_str,",")))
        new_obs = as.numeric(row["t"])  
        #new_obs = 1
        if(length(current_obs)>0){
          if(new_obs %in% current_obs){
            current_obs = current_obs[current_obs!=new_obs]
            new_obs = paste0(current_obs, collapse=",")
          }else{
            new_obs = paste0(current_obs_str, ",", new_obs)
          }
        }
        
        updateTextInput(session, id, value=new_obs)
      }
      
    })
    
    
    # Hover on plots
    plot_hover_id = paste0(products_prefix, product, "|plot_hover")
    observeEvent(input[[plot_hover_id]],{
      column_name =  input[[button_id]]
      row = nearPoints(df=product_df, input[[plot_hover_id]], 
                       xvar="t", yvar=column_name, threshold=10, maxpoints=1)
      
      id = paste0(products_prefix, product, "|hovered")
      if(nrow(row)>0){
        result = paste0("observation# : ", row$t, "<br/>")
        result = paste0(result, column_name," : ",row[,column_name],"<br/>")
        result = paste0(result, "period:", row$period_id)
      }else{
        result = ""
      }
      output[[id]] = renderUI({
        HTML(result)
      })
    })
    
    
    # select a product
    button_select_product_id = paste0(products_prefix, "pId|", 
                                      product, "|Select")
    observeEvent(input[[button_select_product_id]],{
      column_name =  input[[button_id]]
      tmp = getProductData(product, column_name)
      
      product_line <<- product
      product_start_date <<- c(start_year, start_month)
      product_end_date <<- c(end_year, end_month)
      product_last_year_index <<- product_end_date[1] - product_start_date[1] + 1
      product_data <<- tmp
      product_data_column <<- column_name
      product_data_obeservations <<- length(tmp)
      
      forecast_file = paste0(product_folder,"/Forecast.csv")
      if(file.exists(forecast_file)){
        
        forecast_df = read.csv(forecast_file)  
        
        forecast_start_year = forecast_df[1,"year"]
        forecast_start_month = forecast_df[1,"month"]
        
        forecast_end_year = tail(forecast_df[,"year"], n=1)
        forecast_end_month = tail(forecast_df[,"month"], n=1)
        
        product_forecast_start_date <<- c(forecast_start_year, forecast_start_month)
        product_forecast_end_date <<- c(forecast_end_year, forecast_end_month)
        product_forecast_data <<- ts(forecast_df[,"forecast"], frequency=12, 
                                     start=product_forecast_start_date, end=product_forecast_end_date)
                
      }
      
    })
    
  })
  
}




