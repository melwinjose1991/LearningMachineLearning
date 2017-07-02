library(forecast)



## Parameters
data_folder = "../../Data/2401/"
revenue_file = paste0(data_folder,"2401_Revenue.csv")



# Loading Data
data_revenue = read.csv(revenue_file, header=TRUE, sep=",")
data = data_revenue[,c("orders_rcvd")]
data_ts = ts(data, frequency=12)
plot(data_ts)



# Forecast using decomposition : Just using naive
h=3
t.windows = 1:15
s.windows = c(7,9,11,13)
maes = vector('numeric')
configs = vector('character')
for(s.window in s.windows){
  for(t.window in t.windows){
    
    window_ts = window(data_ts, end=4+((12-(h+1))/12))  
    fit <- stl(window_ts, t.window=t.window, s.window=s.window, robust=TRUE)
    sa_adj <- seasadj(fit)
    
    forecast = naive(sa_adj, h=h)
    # plot(forecast)
    print(forecast$mean)
    
    mae = mean(abs(tail(data,n=h)-forecast$mean))
    print(mae)
    maes = c(maes, mae)
    
    configs = c(configs,paste0("t.window:",t.window," s.window:",s.window))
    
  }
}
maes[which.min(maes)]
configs[which.min(maes)]
# 3636



# Forecast using decomposition : Using snaive
h=3
t.windows = 1:15
s.windows = c(7,9,11,13)
maes = vector('numeric')
configs = vector('character')
for(s.window in s.windows){
  for(t.window in t.windows){
    
    window_ts = window(data_ts, end=4+((12-(h+1))/12))  
    fit <- stl(window_ts, t.window=t.window, s.window=s.window, robust=TRUE)

    forecast = forecast(fit, method="naive", h=h)
    # plot(forecast)
    print(forecast$mean)
    
    mae = mean(abs(tail(data,n=h)-forecast$mean))
    print(mae)
    maes = c(maes, mae)
    
    configs = c(configs,paste0("t.window:",t.window," s.window:",s.window))
    
  }
}
maes[which.min(maes)]
configs[which.min(maes)]
# 4053



# Simple Exponential Smoothing
h=3
alphas = seq(0.1, 0.9, length=10)
maes = vector('numeric')
for(alpha in alphas){
  
  window_ts = window(data_ts, end=4+((12-(h+1))/12))  
  fit = ses(window_ts, alpha=alpha, initial="simple", h=h)
  #plot(fit)

  mae = mean(abs(tail(data,n=h)-fit$mean))
  print(mae)
  maes = c(maes, mae)
}
plot(alphas, maes)
lines(alphas, maes)
alphas[which.min(maes)]
maes[which.min(maes)]
# 4462



# Double Exponential Smoothing OR Holt's Linear Model : Additive / Multiplicative
h=3
alphas = seq(0.1, 0.9, length=10)
betas = seq(0.1, 0.9, length=10)
t_multiplicative = FALSE
configs = vector('character')
maes = vector('numeric')
for(beta in betas){
  for(alpha in alphas){
    
    window_ts = window(data_ts, end=4+((12-(h+1))/12))  
    fit = holt(window_ts, alpha=alpha, beta=beta, initial="simple", 
               h=h, exponential=t_multiplicative)
    # plot(fit)
    
    mae = mean(abs(tail(data,n=h)-fit$mean))
    print(mae)
    maes = c(maes, mae)
    
    configs = c(configs, paste0("alpha:",alpha," beta:",beta))
  }
}
configs[which.min(maes)]
maes[which.min(maes)]
# Additive : 2290
# Multiplicative : 2457



# Damped : Additive and Multiplicative
h=3
t_multiplicative = FALSE
window_ts = window(data_ts, end=4+((12-(h+1))/12))  

fit_tuned = holt(window_ts, alpha=0.1, beta=0.1, phi=0.98, damped=TRUE, h=h, exponential=t_multiplicative)
mae = mean(abs(tail(data,n=h)-fit_tuned$mean))
print(mae) #3745

fit_auto = holt(window_ts, damped=TRUE, h=h, exponential=t_multiplicative)
mae = mean(abs(tail(data,n=h)-fit_auto$mean))
print(mae) #4238



# Holts Winters : Additive and Multiplicative
h=3
t_multiplicative = FALSE
window_ts = window(data_ts, end=4+((12-(h+1))/12))  

fit_add = hw(window_ts, seasonal="additive", h=h)
plot(fit_add)
mae = mean(abs(tail(data,n=h)-fit_add$mean))
print(mae) #4341

fit_multi = hw(window_ts, seasonal="multiplicative", h=h)
plot(fit_multi)
mae = mean(abs(tail(data,n=h)-fit_multi$mean))
print(mae) #4304

fit_add_damped = hw(window_ts, seasonal="additive", h=h, damped=TRUE)
plot(fit_add_damped)
mae = mean(abs(tail(data,n=h)-fit_add_damped$mean))
print(mae) #4380

fit_multi_damped = hw(window_ts, seasonal="multiplicative", h=h, damped=TRUE)
plot(fit_multi_damped)
mae = mean(abs(tail(data,n=h)-fit_multi_damped$mean))
print(mae) #4047



# ETS
best_fit = ets(window_ts, opt.crit="mae", allow.multiplicative.trend=TRUE)
best_fit
f = forecast(best_fit, h=3)$mean
mae = mean(abs(tail(data,n=h)-f))
print(mae) #4467
