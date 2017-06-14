

data_folder = "../../Data/"
revenue_file = paste0(data_folder, product, "/2401_Revenue.csv")
sa_OR_nsa = "Not Seasonally Adjusted"



# Loading Data
data_revenue = read.csv(revenue_file, header=TRUE, sep=",")
data = data_revenue[,c("orders_rcvd","month","t")]
data$month = as.factor(data$month)

for(sub_category_id in config_data$sub_category_id){
  
  category_name = config_data[config_data$sub_category_id==sub_category_id,"category_name"]
  sub_category_name = config_data[config_data$sub_category_id==sub_category_id,"sub_category_name"]
  
  
  file = paste0(data_folder ,as.character(category_name), "/", as.character(sub_category_name))
  if(sa_OR_nsa=="Not Seasonally Adjusted"){
    file = paste0(file, "_nsa.csv")
  }else{
    file = paste0(file, "_sa.csv")
  }
  print(paste0("Reading file : ",file))
  data_vars = read.csv(file, header=TRUE, sep=",")
  
  data_vars = data_vars[,!names(data_vars) %in% c("date")]
  data = cbind(data, data_vars)
  
}
