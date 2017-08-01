print("Models :: Regression :: Init")
library(car)
library(lmtest)


## Globals
regression_prefix = paste0(models_prefix, "regression_")
test_color_code = c("#6ACA52", "#C5F274", "#FFF81E", "#F4D214", "#F48414", "#FF5733", "#C70039")



## UI Elements
getRegressionUI = function() {
  
  # row - 0 - Use feature of ?
  id = paste0(regression_prefix, "setsOfFeatures")
  select_feature_set = selectInput(id, label="Features Sets:", choices=FEAUTRE_SELECTION_ALGORITHMS,
                                   selected="LASSO_best")
  
  id = paste0(regression_prefix, "useFeaturesButton")
  button_features = actionButton(id, label="Fetch Features")
  
  row_0 = fluidRow(column(3, select_feature_set), 
                   column(3, button_features))
  
  # row 1 - variables
  text_variables = tags$h4("Variables")
  input_vars = uiOutput(paste0(regression_prefix, "selectedVariables"))
  row_1 = fluidRow(text_variables, input_vars)
  
  # row 2 - Buttons
  button_build_regression_id = paste0(regression_prefix, "buttonBuildRegression")
  button_build_regression = actionButton(inputId = button_build_regression_id,
                                         label = "Build Regression")
  
  text_tests_id = paste0(regression_prefix, "testResults")
  text_tests = htmlOutput(text_tests_id)
  
  row_2 = fluidRow(button_build_regression, text_tests)
  
  # row 3 - plots
  id = paste0(regression_prefix, "graphObservedVsFitted")
  output_graph_obs.vs.fitted = plotOutput(id)
  
  id = paste0(regression_prefix, "graphResidualVsFitted")
  output_graph_res.vs.fitted = plotOutput(id)
  
  id = paste0(regression_prefix, "graphQQ")
  output_graph_qq = plotOutput(id)
  
  id = paste0(regression_prefix, "graphStdResVsFitted")
  output_graph_stdres.vs.fitted = plotOutput(id)
  
  id = paste0(regression_prefix, "graphStdResVsLeverage")
  output_graph_stdres.vs.leverage = plotOutput(id)
  
  row_3 = fluidRow(
    column(width = 6, output_graph_obs.vs.fitted),
    column(width = 6, output_graph_res.vs.fitted),
    column(width = 6, output_graph_qq),
    column(width = 6, output_graph_stdres.vs.fitted),
    column(width = 6, output_graph_stdres.vs.leverage)
  )
  
  tabPanel(title = "LRegression", row_0,  row_1, row_2, row_3)
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
    failed_tests["2"] = "Sum of residuals not zero"
  }else{
    print(paste0("   PASS : sum(errors)=",sum_res))
  }
  
  
  #3
  print("Verifying constant vairance of residuals")
  print("   Null Hypothesis : residuals have constant variance")
  if(ncvTest(fit)$p<0.05){
    print("   FAIL : Rejected null hypothesis")
    failed_tests["3"] = "Non-constant variance of residuals"
  }else{
    print("   OK : Failed to reject null hypothesis")
  }
  
  
  #4
  print("Verifying no correlation amongst residuals")
  print("   Null Hypothesis : errors are serially uncorrelated")
  if(dwtest(fit)$p.value<0.05){
    print("   FAIL : Rejected null hypothesis")
    failed_tests["4"] = "Correlation amongst residuals"
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
      failed_tests["5"] = "Correlation b/w variables and residuals"
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
      failed_tests["7"] = "No variablity in one/more variables"
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
      failed_tests["9"] = "Collinearity b/w one/more variables"
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
    failed_tests["10"] = "Residuals not normally distributed"
  }else{
    print("   OK : Failed to reject null hypothesis")
  }
  
  failed_tests
  
}



doRegression = function(input, selected_vars, y_name, h=0) {
  
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
    
    obs_to_exclude = input[[paste0(products_prefix, product_line, "|avoid")]]
    obs_to_exclude = as.numeric(unlist(strsplit(obs_to_exclude,",")))
    use_rows = setdiff(1:nrow(data), obs_to_exclude)
    
    fit = lm(form, data=as.data.frame(data), subset=use_rows)
    
    df_vars = as.data.frame(data)
    df_vars = df_vars[use_rows, !names(df_vars) %in% y_name]
    test_results = verifyRegressionModel(fit, df_vars)
    print(paste0("#Tests failed : ", sum(lengths(test_results))))  
    
    list(regression=fit, failed_tests=test_results)
    
  }else{
    
    train_till = dim(data)[1] - h
    fit = lm(form, data=as.data.frame(data[1:train_till,]))
    pred = predict(fit, newdata=as.data.frame(data[(train_till+1):dim(data)[1],]), 
                   interval="predict" )
    list(regression=fit, forecast=pred)
    
  }
}



createVariableTable = function(variables, input, session){
  
  vars_titles = list(
    fluidRow(
      column(width=2, tags$h5("Variable")),
      column(width=2, tags$h5("Estimate")),
      column(width=2, tags$h5("Std. Error")),
      column(width=1, tags$h5("t value")),
      column(width=1, tags$h5("Pr(>|t|)")),
      column(width=2, tags$h5("VIF")),
      column(width=2, tags$h5("Keep?"))
    )
  )
  
  vars_rows = lapply(variables, function(var){
    
    var_id = paste0(regression_prefix,"varId|",var)
    var_name = meta_data[meta_data$series_id==var,"title"]
    if(length(var_name)==0){
      var_name = var
    }
    
    select_var_id = paste0(regression_prefix,"varId|", var, "|select")
    input_select_var = checkboxInput(select_var_id, label="", value=TRUE)
    observeEvent(input[[select_var_id]],{
      group_id = meta_data[meta_data$series_id==var, "sub_category_id"]
      id = paste0(all_features_prefix, "fId|", group_id, "|", var)
      if(input[[select_var_id]]){
        updateTextInput(session, id, value=TRUE)
      }else{
        updateTextInput(session, id, value=FALSE)
      }
    })
    
    fluidRow(
      column(width=2, tags$div(title=var, tags$h5(var_name))),
      column(width=2, textInput(paste0(var_id, "|Est"), label="")),
      column(width=2, textInput(paste0(var_id, "|StdErr"), label="")),
      column(width=1, textInput(paste0(var_id, "|t"), label="")),
      column(width=1, textInput(paste0(var_id, "|p"), label="")),
      column(width=2, textInput(paste0(var_id, "|VIF"), label="")),
      column(width=2, input_select_var)
    )
    
  })
  
  list(vars_titles, vars_rows)
  
}



fillVariableTable = function(session, fit){
  
  s = summary(fit)
  coefs = as.data.frame(s$coefficients)
  vifs = vif(fit)
  
  for(var in row.names(coefs)){
    var_id = paste0(regression_prefix,"varId|",var)
    updateTextInput(session, paste0(var_id, "|Est"), value=coefs[var, "Estimate"])
    updateTextInput(session, paste0(var_id, "|StdErr"), value=coefs[var, "Std. Error"])
    updateTextInput(session, paste0(var_id, "|t"), value=coefs[var, "t value"])
    updateTextInput(session, paste0(var_id, "|p"), value=coefs[var, "Pr(>|t|)"])
    if(var!="(Intercept)"){
      updateTextInput(session, paste0(var_id, "|VIF"), value=vifs[[var]])
    }
  }
  
}



attachRegressionObservers = function(input, output, session, reactive_vars){
  
  id_1 = paste0(regression_prefix, "useFeaturesButton")
  observeEvent(input[[id_1]], {
    
    model_name = input[[paste0(regression_prefix, "setsOfFeatures")]]
    selected_vars_id = paste0(SELECTED_VARS_, model_name)
    
    table_regression_varaibles = paste0(regression_prefix, "selectedVariables")
    output[[table_regression_varaibles]] = renderUI({
      createVariableTable(reactive_vars[[selected_vars_id]], input, session)
    })
    
    table_forecast_varaibles = paste0(forecast_prefix, "forecastVariables")
    output[[table_forecast_varaibles]] = renderUI({
      createForecastVariableTable(reactive_vars[[selected_vars_id]], input, output, session)
    })
    
    reactive_vars[[MODEL_LINEAR_REGRESSION_VARS]] = reactive_vars[[selected_vars_id]]
    
  })
  
  
  id_2 = paste0(regression_prefix, "buttonBuildRegression")
  observeEvent(input[[id_2]], {
    
    model_name = input[[paste0(regression_prefix, "setsOfFeatures")]]
    selected_vars_id = paste0(SELECTED_VARS_, model_name)
    
    fit = doRegression(input, reactive_vars[[selected_vars_id]], product_data_column)
    reactive_vars[[MODEL_LINEAR_REGRESSION]] = fit[['regression']]
    
    text_tests_id = paste0(regression_prefix, "testResults")
    output[[text_tests_id]] = renderUI({
      list_failed_tests = fit[['failed_tests']]
      no_of_failed_tests = sum(lengths(list_failed_tests))
      text_failed_tests_count = tags$h4(paste0("#Test Failed : ", no_of_failed_tests), 
                                        style=paste0("color:", test_color_code[no_of_failed_tests+1],"; font-weight: bold;") )
      
      text_failed_tests = vector('character')
      for(index in names(list_failed_tests)){
        text_failed_test = list_failed_tests[[index]]
        text_failed_tests = c(text_failed_tests, text_failed_test)
      }
      text_failed_tests = paste0(text_failed_tests, collapse="<br/>")
      
      text_reg_summary_F = paste0("<br/><br/>F-statistic : ", summary(fit[['regression']])$fstatistic[1], "<br/>")
      text_reg_summary_R = paste0("R2 : ", summary(fit[['regression']])$r.squared, "<br/>")
      text_reg_summary = paste0(text_reg_summary_F, text_reg_summary_R)
      
      HTML(paste0(text_failed_tests_count, text_failed_tests, text_reg_summary))
    })
    
    output_regression_graph_1 = paste0(regression_prefix, "graphResidualVsFitted")
    output[[output_regression_graph_1]] = renderPlot({
      plot(fit[["regression"]], which=1)
    })
    
    output_regression_graph_2 = paste0(regression_prefix, "graphQQ")
    output[[output_regression_graph_2]] = renderPlot({
      plot(fit[["regression"]], which=2)
    })
    
    output_regression_graph_3 = paste0(regression_prefix, "graphStdResVsFitted")
    output[[output_regression_graph_3]] = renderPlot({
      plot(fit[["regression"]], which=3)
    })
    
    output_regression_graph_4 = paste0(regression_prefix, "graphStdResVsLeverage")
    output[[output_regression_graph_4]] = renderPlot({
      plot(fit[["regression"]], which=4)
    })
    
    output_regression_graph_5 = paste0(regression_prefix, "graphObservedVsFitted")
    output[[output_regression_graph_5]] = renderPlot({
      
      obs_to_exclude = input[[paste0(products_prefix, product_line, "|avoid")]]
      obs_to_exclude = unlist(strsplit(obs_to_exclude,","))
      obs_to_exclude = as.numeric(obs_to_exclude)
      use_rows = setdiff(1:length(product_data), obs_to_exclude)
      
      plot(product_data[use_rows], fit[["regression"]]$fitted.values,
           xlab="Actuals", ylab="Fitted" )
      lines(product_data[use_rows], product_data[use_rows])
    })
    
    fillVariableTable(session, fit[['regression']])
    
  })
  
}