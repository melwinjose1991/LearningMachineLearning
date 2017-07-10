print("Models :: Regression :: Init")
library(car)
library(lmtest)


## Globals
regression_prefix = paste0(models_prefix, "regression_")



## UI Elements
getRegressionUI = function() {
  # row 1 - variables
  text_variables = tags$h4("Variables")
  input_vars = uiOutput(paste0(regression_prefix, "selectedVariables"))
  row_1 = fluidRow(text_variables, input_vars)
  
  # row 2 - Buttons
  button_build_regression_id = paste0(regression_prefix, "buttonBuildRegression")
  button_build_regression = actionButton(inputId = button_build_regression_id,
                                         label = "Build Regression")
  
  text_tests_id = paste0(regression_prefix, "testResults")
  text_tests = textInput(inputId=text_tests_id, label="", value="")
  
  row_2 = fluidRow(button_build_regression, text_tests)
  
  # row 3 - plots
  id = paste0(regression_prefix, "graphResidualVsFitted")
  output_graph_res.vs.fitted = plotOutput(id)
  
  id = paste0(regression_prefix, "graphQQ")
  output_graph_qq = plotOutput(id)
  
  id = paste0(regression_prefix, "graphStdResVsFitted")
  output_graph_stdres.vs.fitted = plotOutput(id)
  
  id = paste0(regression_prefix, "graphStdResVsLeverage")
  output_graph_stdres.vs.leverage = plotOutput(id)
  
  row_3 = fluidRow(
    column(width = 6, output_graph_res.vs.fitted),
    column(width = 6, output_graph_qq),
    column(width = 6, output_graph_stdres.vs.fitted),
    column(width = 6, output_graph_stdres.vs.leverage)
  )
  
  # row 4 - ???
  row_4 = fluidRow()
  
  tabPanel(title = "Regression", row_1, row_2, row_3, row_4)
}



## Helper functions
verifyRegressionModel=function(fit, df_vars){
  
  ## Source : http://r-statistics.co/Assumptions-of-Linear-Regression.html
  
  failed_tests = list()
  
  # 1 Regression model is linear in parameters
  
  #2 
  print("Checking Sum of residual")
  sum_res = sum(fit$residuals)
  if(sum_res>0.0001){
    print(paste0("   FAIL : sum(errors)=",sum_res))
    failed_tests[2] = "sum of residuals not zero"
  }else{
    print(paste0("   PASS : sum(errors)=",sum_res))
  }
  
  
  #3
  print("Verifying constant vairance of residuals")
  print("   Null Hypothesis : residuals have constant variance")
  if(ncvTest(fit)$p<0.05){
    print("   FAIL : Rejected null hypothesis")
    failed_tests[3] = "non-constant variance of residuals"
  }else{
    print("   OK : Failed to reject null hypothesis")
  }
  
  
  #4
  print("Verifying no correlation amongst residuals")
  print("   Null Hypothesis : errors are serially uncorrelated")
  if(dwtest(fit)$p.value<0.05){
    print("   FAIL : Rejected null hypothesis")
    failed_tests[4] = "correlation amongst residuals"
  }else{
    print("   OK : Failed to reject null hypothesis")
  }
  
  
  #5
  print("Verifying no correlation b/w variable and errors")
  print("   Null Hypothesis : Correlation is 0")
  for(vars_name in names(df_vars)){
    #print(paste0("   Checking:",vars_name))
    if(cor.test(df_vars[,vars_name], fit$residuals)$p.value<0.05){
      print("      FAIL : Rejected null hypothesis")
      failed_tests[5] = "correlation b/w variables"
    }else{
      print("      OK : Failed to reject null hypothesis")
    }
  }
  
    
  # 6 : The number of observations must be greater than number of Xs
  
  
  # 7
  print("Checking variability in variables")
  for(vars_name in names(df_vars)){
    #print(paste0("   Checking:",vars_name))
    variability = var(df_vars[,vars_name])
    if(variability<=0){
      print("      FAIL : Variability = 0")
      failed_tests[7] = "no variablity in one/more variables"
    }else{
      print("      PASS : Variability > 0 ")
    }
  }
  
  
  # 8 : the regression model is correctly specified
  
  
  # 9 : No perfect multicollinearity
  print("Checking multicollinearity")
  vif_threshold = 4
  if(dim(df_vars)[2]>=2){
    vifs = vif(fit)
    if(sum(vifs>vif_threshold)>1){
      print(paste0("   FAIL for vars : ",unlist(names(df_vars)[vifs>vif_threshold])))
      failed_tests[9] = "collinearity b/w one/more variables"
    }else{
      print("   PASS : No Collinearity")
    }
  }else{
    print("   Model doesnt have 2 or more vars")
  }
  
  
  # 10
  print("Normality of residuals")
  print("   Null Hypothesis : errors are normally distributed")
  if(shapiro.test(fit$residuals)$p.value<0.05){
    print("   FAIL : Rejected null hypothesis")
    failed_tests[10] = "residuals not normally distributed"
  }else{
    print("   OK : Failed to reject null hypothesis")
  }
  
  failed_tests
  
}



getData = function(vars_id, y_name){

  config_data = meta_data[meta_data$series_id %in% vars_id, ]
  
  # Y
  revenue_file = paste0(data_folder, "/", product_line, "/Revenue.csv")
  data = read.csv(revenue_file, header = TRUE, sep = ",")
  data$month = as.factor(data$month)
  data = data[,!names(data) %in% c("month_str")]
  
  # X
  for (sub_category_id in unique(config_data$sub_category_id)) {
    
    category_name = unique(config_data[config_data$sub_category_id==sub_category_id, "category_name"])
    sub_category_name = unique(config_data[config_data$sub_category_id==sub_category_id, "sub_category_name"])
    
    file = paste0(FRED_folder, "/", as.character(category_name), 
                  "/", as.character(sub_category_name) )
    
    if (sa_OR_nsa == "Not Seasonally Adjusted") {
      file = paste0(file, "_nsa.csv")
    } else{
      file = paste0(file, "_sa.csv")
    }
    print(paste0("Reading file : ", file))
    data_series = read.csv(file, header = TRUE, sep = ",")
    
    series_vars = intersect(names(data_series), vars_id)
    if(is.null(dim(data_series[, series_vars]))){
      ## there is just one column from the series
      data[,series_vars] = data_series[, series_vars]
    }else{
      data = cbind(data, data_series[, series_vars])  
    }
    
    
  }
  
  form = as.formula("period_id~.")
  data_mat = model.matrix(form, data=data)
  print(c(vars_id, y_name))
  data_mat = data_mat[,c(vars_id, y_name)]
  data_mat
}



doRegression = function(selected_vars, y_name, h=0) {
  
  print(selected_vars)
  variables = vector('character')
  for(var in selected_vars){
    if(var == "(Intercept)"){
      next
    }else{
      variables = c(variables, var)
    }
  }
  print(variables)
  
  data = getData(variables, y_name)
  
  form = as.formula(paste0(y_name,"~",paste(variables, collapse="+")))

  if(h==0){
    
    fit = lm(form, data=as.data.frame(data))
    
    df_vars = as.data.frame(data)
    df_vars = df_vars[, !names(df_vars) %in% y_name]
    test_results = verifyRegressionModel(fit, df_vars)
    print(paste0("#Tests failed : ", sum(lengths(test_results))))  
    
    list(regression=fit, failed_tests=test_results)
    
  }else{
    
    train_till = dim(data)[1] - h
    fit = lm(form, data=as.data.frame(data[1:train_till,]))
    pred = predict(fit, newdata=as.data.frame(data[(train_till+1):dim(data)[1],]) )
    list(regression=fit, forecast=pred)
    
  }
}

createVariableTable = function(variables){
  
  vars_titles = list(
    fluidRow(
      column(width=2, tags$h5("Variable")),
      column(width=2, tags$h5("Estimate")),
      column(width=2, tags$h5("Std. Error")),
      column(width=2, tags$h5("t value")),
      column(width=2, tags$h5("Pr(>|t|)"))
    )
  )
  
  vars_rows = lapply(variables, function(var){
    
    var_id = paste0(regression_prefix,"varId|",var)
    var_name = meta_data[meta_data$series_id==var,"title"]
    if(length(var_name)==0){
      var_name = var
    }
    fluidRow(
      column(width=2, tags$div(title=var, tags$h5(var_name))),
      column(width=2, textInput(paste0(var_id, "|Est"), label="")),
      column(width=2, textInput(paste0(var_id, "|StdErr"), label="")),
      column(width=2, textInput(paste0(var_id, "|t"), label="")),
      column(width=2, textInput(paste0(var_id, "|p"), label=""))
    )
    
  })
  
  list(vars_titles, vars_rows)
  
}



fillVariableTable = function(session, fit){
  
  s = summary(fit)
  coefs = as.data.frame(s$coefficients)
  
  for(var in row.names(coefs)){
    var_id = paste0(regression_prefix,"varId|",var)
    updateTextInput(session, paste0(var_id, "|Est"), value=coefs[var, "Estimate"])
    updateTextInput(session, paste0(var_id, "|StdErr"), value=coefs[var, "Std. Error"])
    updateTextInput(session, paste0(var_id, "|t"), value=coefs[var, "t value"])
    updateTextInput(session, paste0(var_id, "|p"), value=coefs[var, "Pr(>|t|)"])
  }
  
}