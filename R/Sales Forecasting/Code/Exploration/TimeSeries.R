library(forecast)



## Parameters
data_folder = "../../Data/2401/"
revenue_file = paste0(data_folder,"Revenue.csv")

data_revenue = read.csv(revenue_file)
data = data_revenue[,c("orders_rcvd")]
data_ts = ts(data, frequency=12)
plot(data_ts)



# Forecast using decomposition : Just using naive
h=6
t.windows = 1:15
s.windows = c(7,9,11,13)
maes = vector('numeric')
configs = vector('character')
for(s.window in s.windows){
  for(t.window in t.windows){
    
    window_ts = window(data_ts, end=4+((12-(h+1))/12))  
    # plot(window_ts)
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
t.window=1
s.window=7


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
t.window=4
s.window=11


# Simple Exponential Smoothing
h=3
alphas = seq(0.1, 0.9, length=10)
valid_maes = vector('numeric')
train_maes = vector('numeric')
for(alpha in alphas){
  
  window_ts = window(data_ts, end=4+((12-(h+1))/12))  
  fit = ses(window_ts, alpha=alpha, initial="simple", h=h)
  #plot(fit)

  train_mae = mean(abs(fit$residuals))
  valid_mae = mean(abs(tail(data,n=h)-fit$mean))
  print(paste0("train-",train_mae," valid-",valid_mae))
  valid_maes = c(valid_maes, valid_mae)
  train_maes = c(train_maes, train_mae)
  
}
which.min(train_maes)
configs[which.min(train_maes)]
train_maes[which.min(train_maes)]
# 3660 

which.min(valid_maes)
configs[which.min(valid_maes)]
valid_maes[which.min(valid_maes)]
# 4462

fit = ses(window_ts, initial="simple", h=h)
train_mae = mean(abs(fit$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
plot(fit)
# 3678, 4897



# Double Exponential Smoothing OR Holt's Linear Model : Additive / Multiplicative
h=3
alphas = seq(0.1, 0.9, length=10)
betas = seq(0.1, 0.9, length=10)
t_multiplicative = FALSE
configs = vector('character')
valid_maes = vector('numeric')
train_maes = vector('numeric')
for(beta in betas){
  for(alpha in alphas){
    
    window_ts = window(data_ts, end=4+((12-(h+1))/12))  
    fit = holt(window_ts, alpha=alpha, beta=beta, initial="simple", 
               h=h, exponential=t_multiplicative)
    # plot(fit)
    
    train_mae = mean(abs(fit$residuals))
    valid_mae = mean(abs(tail(data,n=h)-fit$mean))
    print(paste0("train-",train_mae," valid-",valid_mae))
    valid_maes = c(valid_maes, valid_mae)
    train_maes = c(train_maes, train_mae)
    
    configs = c(configs, paste0("alpha:",alpha," beta:",beta))
    
  }
}
which.min(train_maes)
configs[which.min(train_maes)]
train_maes[which.min(train_maes)]
# 4603

which.min(valid_maes)
configs[which.min(valid_maes)]
valid_maes[which.min(valid_maes)]
# 2290

fit = holt(window_ts, initial="simple", h=h, exponential=t_multiplicative)
plot(data_ts)
lines(fit$fitted, col="red", lwd=2, lty=2)
lines(fit$mean, col="green", lwd=2, lty=2)
train_mae = mean(abs(fit$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
# 4735, 7594



# Damped : Additive and Multiplicative
h=3
window_ts = window(data_ts, end=4+((12-(h+1))/12))  

t_multiplicative = FALSE
fit_add = holt(window_ts, damped=TRUE, h=h, exponential=t_multiplicative)
plot(data_ts, lwd=2)
lines(fit_add$fitted, col="red", lwd=2, lty=2)
lines(fit_add$mean, col="green", lwd=2, lty=2)
train_mae = mean(abs(fit_add$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit_add$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
# 3575, 4238

t_multiplicative = TRUE
fit_multi = holt(window_ts, damped=TRUE, h=h, exponential=t_multiplicative)
plot(data_ts, lwd=2)
lines(fit_multi$fitted, col="red", lwd=2, lty=2)
lines(fit_multi$mean, col="green", lwd=2, lty=2)
train_mae = mean(abs(fit_multi$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit_multi$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
# 0.106, 4726



# Holts Winters : Additive and Multiplicative
h=3
t_multiplicative = FALSE
window_ts = window(data_ts, end=4+((12-(h+1))/12))  

fit_add = hw(window_ts, seasonal="additive", h=h)
plot(data_ts, lwd=2)
lines(fit_add$fitted, col="red", lwd=2, lty=2)
lines(fit_add$mean, col="green", lwd=2, lty=2)
train_mae = mean(abs(fit_add$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit_add$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
# 3203, 4341

fit_multi = hw(window_ts, seasonal="multiplicative", h=h)
plot(data_ts, lwd=2)
lines(fit_multi$fitted, col="red", lwd=2, lty=2)
lines(fit_multi$mean, col="green", lwd=2, lty=2)
train_mae = mean(abs(fit_multi$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit_multi$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
# 0.085, 4304

fit_add_damped = hw(window_ts, seasonal="additive", h=h, damped=TRUE)
plot(data_ts, lwd=2)
lines(fit_add_damped$fitted, col="red", lwd=2, lty=2)
lines(fit_add_damped$mean, col="green", lwd=2, lty=2)
train_mae = mean(abs(fit_add_damped$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit_add_damped$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
# 3177, 4380

fit_multi_damped = hw(window_ts, seasonal="multiplicative", h=h, damped=TRUE)
plot(data_ts, lwd=2)
lines(fit_multi_damped$fitted, col="red", lwd=2, lty=2)
lines(fit_multi_damped$mean, col="green", lwd=2, lty=2)
train_mae = mean(abs(fit_multi_damped$residuals))
valid_mae = mean(abs(tail(data,n=h)-fit_multi_damped$mean))
print(paste0("train-",train_mae," valid-",valid_mae))
# 0.084, 4047


# ETS
best_fit = ets(window_ts, opt.crit="mae", allow.multiplicative.trend=TRUE)
plot(best_fit)
f = forecast(best_fit, h=3)$mean
mae = mean(abs(tail(data,n=h)-f))
print(mae) #4467

