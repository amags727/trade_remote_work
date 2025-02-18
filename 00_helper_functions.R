



# dummy variable makers ---------------------------------------------------

simulate_discrete_vars = function(data, data_dummy, group_vars, interest_vars){
  
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.))
  data = merge(data, group_keys)
  data_dummy = merge(data_dummy, group_keys)
  
  # for each group use the joint empirical distribution of values to generate 
  data_dummy = lapply(1:nrow(group_keys), function(i){
    temp_dummy = data_dummy[group_code == i]; num_in_dummy = nrow(temp_dummy)
    if (num_in_dummy > 0){
      temp = data[group_code == i, ..interest_vars]; num_in_temp = nrow(temp);
      temp_dummy = cbind(temp_dummy,temp[sample(1:num_in_temp, num_in_dummy, T)])
    }
    return(temp_dummy)
  }) %>% rbindlist(fill =T, use.names = T)
  data[, group_code := NULL]; data_dummy[, group_code := NULL]
  return(data_dummy)
}

simulate_continuous_vars = function(data, data_dummy, group_vars, interest_vars){
  
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.))
  data = merge(data, group_keys)
  data_dummy = merge(data_dummy, group_keys, by = group_vars)
  
  # generate the mins and maxes for the whole dataset, these will serve as bounds
  # for the simulation draws
  mins = apply(data[,..interest_vars],2, NA_min); maxes = apply(data[,..interest_vars],2, NA_max)
  
  # for each group generate the multivariate normal distribution of the variables of interest 
  data_dummy = lapply(1:nrow(group_keys), function(i){
    temp_dummy = data_dummy[group_code == i]; num_in_dummy = nrow(temp_dummy)
    if (num_in_dummy > 0){
      temp_dummy = tryCatch({
        temp = data[group_code == i, ..interest_vars] 
        ## ensure covariance matrix is positive definite
        noise = rnorm(nrow(temp)*length(interest_vars),0, 1e-6) %>% matrix(., nrow = nrow(temp))
        noise = noise - min(noise);temp = temp+ noise
        cov_matrix = cov(temp, use = 'pairwise.complete.obs') %>% nearPD()
        cov_matrix = cov_matrix$mat
        
        # simulate data 
        draws = rtmvnorm(num_in_dummy, colMeans(temp, na.rm = T),cov_matrix,mins,maxes)
        temp_dummy[,(interest_vars) := as.data.table(draws)] 
        return(temp_dummy)
      }, error = function(e){return(temp_dummy)})
    }
    return(temp_dummy)
  }) %>% rbindlist(fill =T, use.names = T)
  
  return(data_dummy)
}


# misc --------------------------------------------------------------------
exclude_from = function(base_list, exclude_elements){
  return(base_list[!base_list %in% exclude_elements])
}

NA_sum = function(x){
  ifelse(all(is.na(x)), NA, sum(x,na.rm = T))
}

NA_mean = function(x){
  ifelse(all(is.na(x)), NA, mean(x,na.rm = T))
}

NA_median = function(x){
  ifelse(all(is.na(x)), NA, median(x,na.rm = T))
}

NA_sd = function(x){
  ifelse(all(is.na(x)), NA, sd(x,na.rm = T))
}

NA_max = function(x){
  ifelse(all(is.na(x)), NA, max(x,na.rm = T))
}

NA_min = function(x){
  ifelse(all(is.na(x)), NA, min(x,na.rm = T))
}
NA_IQR = function(x){
  ifelse(all(is.na(x)), NA, IQR(x,na.rm = T))
}

NA_coef_var = function(x){
  NA_sd(x)/NA_mean(x)
}

replicate_var = function(data, data_dummy, var, discrete){
  if (discrete){ 
    data_dummy[[var]] = sample(exclude_from(data[[var]],NA), size = nrow(data), replace = T)
  } else{
    data$var = data[[var]] 
    data_dummy$var =  rnorm(nrow(data), mean(na.rm=T, data$var[data$var !=0 ]), sd(na.rm=T, data$var[data$var !=0 ]))
    if (NA_min(data$var) >= 0){
      data_dummy = data_dummy[var !=0, var := var - min(var,0)]
    }
    data_dummy[[var]]= data_dummy$var; data_dummy$var = NULL; data$var = NULL
  }
  return(data_dummy)
}

unbalanced_lag = function(data,id_var, time_var, value_var, lag_amount){
  data = as.data.table(data)
  data[, id:= .GRP , by = mget(id_var)]
  
  data[, `:=`(value = get(value_var), time = get(time_var), time_lag = get(time_var)+ lag_amount)]
  data = merge(data, data[, .(time_lag, id, value)] %>% rename(value_lag = value),
               by.x = c('id', 'time'), by.y = c('id', 'time_lag'), all.x = T)
  
  if (lag_amount >0){
    data[, paste0(value_var,"_lag", lag_amount) := value_lag]
  }else{
    data[, paste0(value_var,"_lead", (-1)*lag_amount) := value_lag]
  }
  extra = c('value', 'value_lag', 'time', 'time_lag', 'id')
  data[, (extra):= NULL]
}

standardize = function(x){
  x = (x - mean(x,na.rm =T)) / sd(x, na.rm = T)
}

positive_standardize = function(x){
  x = standardize(x)
  min_value = min(x, na.rm =T) 
  if (min_value < 1){
    x= x + (1 - min_value)
  }
}
coef_var = function(x){
  sd(x, na.rm =T) / mean(x, na.rm =T)
}

generate_distribution_graphs = function(data, interest_var, granularity){
  data[, interest := get(interest_var)]
  data[, rank := cut(interest, breaks = granularity, labels = F)]
  data = data[, .(count = .N), by = rank]
  setorder(data, rank) 
  data[, cum_share := cumsum(count)/ sum(count)]
  
  PDF = ggplot(data, aes(x = rank, y =count)) + geom_point()
  CDF = ggplot(data, aes(x = rank, y = cum_share)) + geom_point()
  output = list(PDF, CDF, data)
}

