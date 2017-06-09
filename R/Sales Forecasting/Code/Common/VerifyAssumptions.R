library(car)
library(lmtest)



verifyRegressionModel=function(fit, df_vars){
  
  ## Source : http://r-statistics.co/Assumptions-of-Linear-Regression.html
  
  failed_tests = list()
  
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
    print(paste0("   Checking:",vars_name))
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
    print(paste0("   Checking:",vars_name))
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
  if(dim(df_vars)[2]>=2){
    vifs = vif(fit)
    if(sum(vifs>2)>1){
      print(paste0("   FAIL for vars : ",unlist(names(df_vars)[vifs>2])))
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
