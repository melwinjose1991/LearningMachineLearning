library(parallel)
library(lubridate)

get_train = TRUE
pickup_dropoff_timetaken = 2

no_cores = detectCores() - 1
cluster = makeCluster(no_cores)

if(get_train){
  print("Getting train")
  csv = read.csv("data/train.csv", header=TRUE, sep=",")
}else{
  print("Getting test")
  csv = read.csv("data/test.csv", header=TRUE, sep=",")  
}

tid = csv$TID
if(pickup_dropoff_timetaken==1 || pickup_dropoff_timetaken==3){
  print("Getting pickup_date")
  pickup_date = csv$pickup_datetime  
}
if(pickup_dropoff_timetaken==2 || pickup_dropoff_timetaken==3){
  print("Getting dropoff_date")
  dropoff_date = csv$dropoff_datetime
}
rm(csv)



getPOSIXTime=function(t_factor, fast=TRUE){
  #t_factor = train_pickup_date[1]
  t_str = as.character(t_factor)
  if(fast){
    t = fast_strptime(t_str,"%Y-%m-%d %H:%M:%S", tz="EST")
  }else{
    t = strptime(t_str,"%Y-%m-%d %H:%M:%S", tz="EST")  
  }
  t
}

getTimeDifference=function(t1, t2){
  #t1_str = as.character(t1_factor)
  #t2_Str = as.character(t2_factor)
  
  #t1 = strptime(t1_str,"%Y-%m-%d %H:%M:%S", tz="EST")
  #t2 = strptime(t2_Str,"%Y-%m-%d %H:%M:%S", tz="EST")
  
  as.numeric(difftime(t2, t1, units="secs"))
}


if(FALSE){
  clusterExport(cluster, "fast_strptime")
  clusterExport(cluster, "getPOSIXTime")
  
  df = data.frame(TID=tid)
  
  if(pickup_dropoff_timetaken==1 || pickup_dropoff_timetaken==3){
    print("Getting pickup_date_t")
    pickup_datetime_t = parLapply(cluster, pickup_date, function(d) getPOSIXTime(d,fast=FALSE))
  }
  if(pickup_dropoff_timetaken==2 || pickup_dropoff_timetaken==3){
    print("Getting dropoff_date_t")
    dropoff_datetime_t = parLapply(cluster, dropoff_date, function(d) getPOSIXTime(d,fast=FALSE))
  }
  
  ## time_taken
  if(pickup_dropoff_timetaken==3){
    
    clusterExport(cluster, "getTimeDifference")  
    
    print("Getting time_taken")
    time_taken = mcmapply(function(d1,d2) getTimeDifference(d1,d2), 
                                pickup_datetime_t, dropoff_datetime_t)
    #time_taken = clusterMap(cluster, function(d1,d2) getTimeDifference(d1,d2), 
    #                              pickup_date, test_dropoff_date)
    
    df_time_taken = cbind(df, time_taken)  
    
    if(get_train){
      write.csv(df_time_taken, "data/train_time_taken.csv", quote=FALSE, row.names=FALSE)
    }else{
      write.csv(df_time_taken, "data/test_time_taken.csv", quote=FALSE, row.names=FALSE)
    }
    
  }else{
    
    clusterExport(cluster, "minute")
    clusterExport(cluster, "hour")
    clusterExport(cluster, "yday")
    clusterExport(cluster, "week")
    clusterExport(cluster, "month")
    clusterExport(cluster, "year")
    
    if(pickup_dropoff_timetaken==1){
      
      pickup_min = unlist(parLapply(cluster, pickup_datetime_t, function(x) minute(x)))
      df_pickup = cbind(df, pickup_min)
      
      pickup_hour = unlist(parLapply(cluster, pickup_datetime_t, function(x) hour(x)))
      df_pickup = cbind(df_pickup, pickup_hour)
      
      pickup_yday = unlist(parLapply(cluster, pickup_datetime_t, function(x) yday(x)))
      df_pickup = cbind(df_pickup, pickup_yday)
      
      pickup_week = unlist(parLapply(cluster, pickup_datetime_t, function(x) week(x)))
      df_pickup = cbind(df_pickup, pickup_week)
      
      pickup_month = unlist(parLapply(cluster, pickup_datetime_t, function(x) month(x)))
      df_pickup = cbind(df_pickup, pickup_month)
      
      pickup_year = unlist(parLapply(cluster, pickup_datetime_t, function(x) year(x)))
      df_pickup = cbind(df_pickup, pickup_year)
      
      if(get_train){
        write.csv(df_pickup, "data/train_pickup.csv", quote=FALSE, row.names=FALSE)
      }else{
        write.csv(df_pickup, "data/test_pickup.csv", quote=FALSE, row.names=FALSE)
      }
      
    }
      
    if(pickup_dropoff_timetaken==2){
      
      dropoff_min = unlist(parLapply(cluster, dropoff_datetime_t, function(x) minute(x)))
      df_dropoff = cbind(df, dropoff_min)
      
      dropoff_hour = unlist(parLapply(cluster, dropoff_datetime_t, function(x) hour(x)))
      df_dropoff = cbind(df_dropoff, dropoff_hour)
      
      dropoff_yday = unlist(parLapply(cluster, dropoff_datetime_t, function(x) yday(x)))
      df_dropoff = cbind(df_dropoff, dropoff_yday)
      
      dropoff_week = unlist(parLapply(cluster, dropoff_datetime_t, function(x) week(x)))
      df_dropoff = cbind(df_dropoff, dropoff_week)
      
      dropoff_month = unlist(parLapply(cluster, dropoff_datetime_t, function(x) month(x)))
      df_dropoff = cbind(df_dropoff, dropoff_month)
      
      dropoff_year = unlist(parLapply(cluster, dropoff_datetime_t, function(x) year(x)))
      df_dropoff = cbind(df_dropoff, dropoff_year)
      
      if(get_train){
        write.csv(df_dropoff, "data/train_dropoff.csv", quote=FALSE, row.names=FALSE)
      }else{
        write.csv(df_dropoff, "data/test_dropoff.csv", quote=FALSE, row.names=FALSE)
      }
      
    }
  }
  
}