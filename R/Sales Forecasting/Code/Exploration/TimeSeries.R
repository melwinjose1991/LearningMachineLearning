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


# Double Exponential Smoothing OR Holt's Linear Model
h=3
alphas = seq(0.1, 0.9, length=10)
betas = seq(0.1, 0.9, length=10)
configs = vector('character')
maes = vector('numeric')
for(beta in betas){
  for(alpha in alphas){
    
    window_ts = window(data_ts, end=4+((12-(h+1))/12))  
    fit = holt(window_ts, alpha=alpha, beta=beta, initial="simple", h=h)
    # plot(fit)
    
    mae = mean(abs(tail(data,n=h)-fit$mean))
    print(mae)
    maes = c(maes, mae)
    
    configs = c(configs, paste0("alpha:",alpha," beta:",beta))
  }
}
configs[which.min(maes)]
maes[which.min(maes)]


# Additive Damped
h=3
alphas = seq(0.1, 0.9, length=10)
betas = seq(0.1, 0.9, length=10)
phis = seq(0.8, 0.98, length=10)
configs = vector('character')
maes = vector('numeric')

for(phi in phis){
  for(beta in betas){
    for(alpha in alphas){
      
      print(paste0("alpha:",alpha," beta:",beta, " phi:",phi))
      if(beta>alpha)
        next
      
      window_ts = window(data_ts, end=4+((12-(h+1))/12))  
      fit = holt(window_ts, alpha=alpha, beta=beta, phi=phi, damped=TRUE, h=h)
      # plot(fit)
      
      mae = mean(abs(tail(data,n=h)-fit$mean))
      print(mae)
      maes = c(maes, mae)
      
      configs = c(configs, paste0("alpha:",alpha," beta:",beta, " phi:",phi))
    }
  }
}

configs[which.min(maes)]
maes[which.min(maes)]

