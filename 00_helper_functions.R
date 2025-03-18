


# Table Cleanup ----------------------------------------------------
model_to_df = function(model){
  data.frame(regressor = names(model$coefficients)) %>% mutate(
    year =  as.numeric(str_extract(regressor, '\\d{4}')),
    coef = as.numeric(model$coefficients),
    se = as.numeric(summary(model)$se), 
    p_val = round(summary(model)$coeftable[, "Pr(>|t|)"],3),
    lb = coef - 1.96*se, ub = coef + 1.96*se)
}

split_var = function(table, var, replacement){
  index = grep(var, table, fixed = TRUE)[1]
  table[index] = gsub(var, replacement[1], table[index])
  table[index +1] = paste0(replacement[2], table[index +1])
  return(table)
}
insert_column_space <- function(input_string,after_column, og_columns) {
  # Count the number of '&' characters
  amp_count <- str_count(input_string, "&")
  
  # If there are at least four '&' characters
  if (amp_count >= after_column) {
    # Locate the positions of all '&' characters
    amp_positions <- str_locate_all(input_string, "&")[[1]]
    
    # Get the position of the fourth '&'
    fourth_amp_position <- amp_positions[after_column, 1]
    
    # Replace the fourth '&' with '& \hspace{5 pt} &'
    modified_string <- str_sub(input_string, 1, fourth_amp_position - 1) %>%
      paste0("& \\hspace{5 pt} &", str_sub(input_string, fourth_amp_position + 1))
    
    return(modified_string)
  }else{
    input_string = input_string %>% gsub( paste0('\\{',og_columns +1, '\\}'),  paste0('\\{',og_columns+2, '\\}'),. ) %>%
      gsub( paste0('\\{',og_columns, '\\}'),  paste0('\\{',og_columns+1, '\\}'),. )
    return(input_string)
  }
}
gen_table_line= function(items,space_after){
  line_output = paste0(paste(items, collapse = "&"),"\\\\")
  if (space_after){
    line_output = paste(line_output, '[1em]')
  }
  return(line_output)
}

format_table = function(model_inputs,label, coef_names = NA, column_names = NA, custom_rows =NA,  headers = NA,
                        divisions_before = NA, notes = NA, note_width = .5, output_path = NA, caption = 'NULL'){
  
  num_columns = length(model_inputs)
  custom_block = paste0(", caption = '", caption, "'")
  if (any(!is.na(coef_names))) custom_block =paste0(custom_block, ', custom.coef.names = coef_names')
  if (all(is.na(column_names))) column_names = gpaste('(', 1:num_columns, ")")
  command = paste0('capture.output(texreg(model_inputs, label = paste0("tab:", label),
            stars = c(0.01, 0.05, 0.1), include.groups = F, include.rsquared = FALSE,
            custom.model.names = column_names, include.adjrs = FALSE, include.loglik = FALSE,
            caption.above = T,include.deviance = F, include.pseudors =F, include.aic = FALSE,',custom_block, '))')
  table = eval(parse(text = command))
  
  ### Add custom rows 
  table = append(table, "[1em]", after = (which(table == "\\hline")[3]-1))
  if (any(!is.na(custom_rows))){
    for (i in 1:length(custom_rows)){
      table = append(table,paste0(paste(custom_rows[[i]], collapse = " & "),"\\\\"),
                     after = (which(table == "\\hline")[3]-1))
    }
  } 
  ### Insert Divisions for Ease of Reading 
  if (!is.na(divisions_before)){
    table[5] = gsub("\\}\\{l", '}{l c', table[5])
    for (i in 1:length(table)){
      table[i] = insert_column_space(table[i],divisions_before,num_columns)
    }}
  ### Add Headers 
  if (!is.na(headers)){table<- append(table, headers, after = 6)}
  
  
  ## UPDATE THE NOTES
  if (any(!is.na(notes))){
    notes_index = grep("^{***}p", table, fixed = T)[1]
    note_base = table[notes_index]
    multi_col = regmatches(note_base, regexpr("\\\\multicolumn\\{\\d+\\}\\{l\\}", note_base))
    p_vals =  gsub("\\\\multicolumn\\{\\d+\\}\\{l\\}\\{\\\\scriptsize\\{", "", note_base) %>% 
      gsub("\\}\\}$", "", .)
    note_line = paste0(multi_col,'{\\parbox{',note_width, 
                       '\\linewidth}{\\scriptsize\\\\ \\vspace{5 pt}', 
                       p_vals, " ", notes, '}}}')
    table[notes_index] = note_line
  }
  
  
  
  
  ### Update so that the table actually shows up where it's supposed to
  table[2] = "\\begin{table}[h]"    
  if (is.na(output_path)){
    return(table)
  }else{
    writeLines(table, output_path)
  }
}
format_summary_table = function(base_table, divisions_before = NA, headers = NA,
                                notes = NA, note_width = .5,output_path =NA){
  kable(base_table, format = "latex", booktabs = TRUE) %>% cat(., file = "table.tex")
  table = readLines('table.tex')  
  file.remove('table.tex')
  ### adjust the alignment and note the number of data cols 
  table[2] =  gsub("\\\\begin\\{tabular\\}\\{l", "", table[2]) %>%
    gsub('l', 'c',.) %>% paste0("\\begin{tabular}{l",.) 
  num_data_cols = str_count(table[2], "c")
  
  ## add a division between columns to enhance readability if necessary
  if (!is.na(divisions_before)){
    table[2] = gsub("\\}\\{l", '}{l c', table[2])
    for (i in 1:length(table)){
      table[i] = insert_column_space(table[i],divisions_before,num_data_cols)
    }}
  
  ## add headers if necessary 
  if (!is.na(headers)){table<- append(table, headers, after = 3)}
  
  ## add notes if necessary 
  if (!is.na(notes)){
    table = append(
      table,paste0("\\multicolumn{",
                   num_data_cols + 1 + length(divisions_before),
                   "}{l}{\\parbox{",note_width,
                   "\\linewidth}{\\scriptsize \\\\ \\vspace{5 pt}", notes, "}}}"),
      after = length(table)-1)
  }
  
  ## export if necessary 
  if (is.na(output_path)){
    return(table)
  }else{
    writeLines(table, output_path)
  }
}

# dummy variable makers ---------------------------------------------------

simulate_discrete_vars = function(data, data_dummy, group_vars, interest_vars){
  if("group_code" %in% names(data)){
    data = data %>% select(-group_code)
  }
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.)) 
  data = merge(data, group_keys, by = group_vars)
  data_dummy = merge(data_dummy, group_keys, by = group_vars)
  
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
  if("group_code" %in% names(data)){
    data = data %>% select(-group_code)
  }
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.))
  data = merge(data, group_keys, by = group_vars)
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
sub_regression = function(...,output_model= F, predicted = F, residuals = F, ssr = F){
  col_list = list(...)
  
  max_length <- max(sapply(col_list, length))
  
  # Standardize column lengths by padding with NAs
  col_list <- lapply(col_list, function(col) {
    length(col) <- max_length  # Expands or truncates the vector
    return(col)
  })
  df <- as.data.table(do.call(cbind, col_list)) %>% na.omit()
  
  if(nrow(df)<= length(col_list)) return(NA_real_) 
  command = paste0('model = lm(data = df, V1 ~',
                   gpaste('V',2:length(col_list), collapse_str = "+" ),
                   ")")
  eval(parse(text = command))
  if(output_model) return(model)
  if(predicted) return(predict(model) %>% as.data.frame() %>% .[,1])
  if(residuals) return(resid(model))
  if(ssr) return(sum(resid(model)^2))
}

expand = function(..., names, order = NULL){
  input = list(...)
  if(is.null(order)){

  output = expand.grid(rev(input), stringsAsFactors = F) %>%
    select(rev(everything())) %>% rename_with(~names) %>% as.data.table()
  }else{
    rev_order = rep(0,length(order))
    for (i in seq_along(order)) rev_order[i] = which(order == i)[1]
    rev_order = length(input) + 1 - rev_order
    output = expand.grid(rev(input[order]), stringsAsFactors = F)
    output = output[, rev_order] %>% rename_with(~names) %>% as.data.table()
  }
  return(output)
}
gpaste <- function(..., order = NA, collapse_str = NA, no_expand = F) {
  # Get the list of arguments as input
  args <- list(...)
  if (!no_expand){
  # Create a data frame with all combinations of the arguments
  if (any(is.na(order))){
    
    combinations <- expand.grid(rev(args), stringsAsFactors = FALSE)
    combinations = combinations[, rev(seq_along(combinations))]
  }else{
    rev_order = rep(0,length(order))
    for (i in seq_along(order)) rev_order[i] = which(order == i)[1]
    rev_order = length(args) + 1 - rev_order
    combinations = expand.grid(rev(args[order]), stringsAsFactors = F)
    combinations = combinations[, rev_order]
  }
  # Concatenate the combinations row-wise
  output <- apply(combinations, 1, paste0, collapse = "")
  }else {output = do.call(paste0, args)}
  if (!is.na(collapse_str)) output = paste(output, collapse = collapse_str)
  return(output)
}
import_parquet = function(file, col_select = NULL, data_table = T){
  if (!is.null(col_select)){
    file = read_parquet(file, col_select = col_select)
  } else{
    file = read_parquet(file)
  }
  
  if (data_table) file = as.data.table(file)
  return(file)
}
import_csv = function(file, col_select = NULL, char_vars = NULL){
  if(!is.null(col_select)) col_select = paste0(", select = c('", paste(col_select, collapse = "','"), "')")
  if(!is.null(char_vars)) char_vars =  paste0(", colClasses = list(character=  c('", 
                                              paste(char_vars, collapse = "','"), "'))")
  command = paste0("fread('",file,"'", col_select, char_vars, ")")
  return(eval(parse(text = command)))
}

import_file <- function(filepath, col_select = NULL, data_table = T, char_vars = NULL){
  if (grepl("\\.parquet$", filepath, ignore.case = TRUE)) {
    file <- import_parquet(filepath, col_select = col_select, data_table = T) %>%
      mutate(across(char_vars, ~as.character(.)))
    } else if (grepl("\\.xlsx$|\\.xls$", filepath, ignore.case = TRUE)) {
    file <- read_excel(filepath) %>% as.data.table()
  } else if (grepl("\\.csv$", filepath, ignore.case = TRUE)) {
    file <- import_csv(filepath, col_select = col_select, char_vars = char_vars)
  } else if (grepl("\\.rds$", filepath, ignore.case = TRUE)) {
    file <- readRDS(filepath)
  } else {
    stop("Unsupported file type")
  }
  return(file)
}
copy_directory <- function(from_dir, to_dir) {
  # Ensure the source directory exists
  if (!dir.exists(from_dir)) {
    stop("Source directory does not exist.")
  }
  
  # Create the destination directory if it doesn't exist
  if (!dir.exists(to_dir)) {
    dir.create(to_dir, recursive = TRUE)
  }
  
  # List all files and directories (recursive)
  files <- list.files(from_dir, full.names = TRUE, recursive = TRUE)
  
  # Recreate subdirectory structure in the destination folder
  sub_dirs <- unique(dirname(files))
  sub_dirs <- gsub(from_dir, to_dir, sub_dirs)  # Adjust paths to match new root
  lapply(sub_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # Copy each file while preserving its subdirectory structure
  dest_files <- gsub(from_dir, to_dir, files)  # Adjust paths for new destination
  file.copy(files, dest_files, overwrite = TRUE)
  
  print("Directory copied successfully with all subfolders and files.")
}








exclude_from = function(base_list, exclude_elements){
  return(base_list[!base_list %in% exclude_elements])
}

# NA_var functions 
{
NA_sum = function(x){
  ifelse(all(is.na(x)), NA_real_, sum(x,na.rm = T))
}

NA_mean = function(x){
  ifelse(all(is.na(x)), NA_real_, mean(x,na.rm = T))
}

NA_median = function(x){
  ifelse(all(is.na(x)), NA_real_, median(x,na.rm = T))
}

NA_sd = function(x){
  ifelse(all(is.na(x)), NA_real_, sd(x,na.rm = T))
}

NA_max = function(x){
  ifelse(all(is.na(x)), NA_real_, max(x,na.rm = T))
}

NA_min = function(x){
  ifelse(all(is.na(x)), NA_real_, min(x,na.rm = T))
}
NA_IQR = function(x){
  ifelse(all(is.na(x)), NA_real_, IQR(x,na.rm = T))
}

NA_coef_var = function(x){
  NA_sd(x)/NA_mean(x)
}
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

unbalanced_lag = function(data,id_var,time_var, value_vars, lag_amounts,
                          expand = F,expand_value = 0, birth_var =NA, death_var = NA){
  # returns a dataframe with lag and log variables that account for potentially missing observations
  # also if expand is true will set lags from before the id's birth / after it's death to zero if it lies within the 
  # time range of the dataset (otherwise will remain na)
  
  ### assign short haand versions of the variable names 
  if(('value' %in% value_vars) & length(value_vars) > 1) stop("you're about to clobber the value var; rename it")
  data = as.data.table(data)
  og_columns = names(data)
  
  final_columns = og_columns; 
  if(any( lag_amounts <0)) final_columns = c(final_columns, gpaste(value_vars,"_", paste0("lead",abs(lag_amounts %>% .[.<0]))))
  if(any( lag_amounts >0)) final_columns = c(final_columns, gpaste(value_vars,"_", paste0("lag",lag_amounts %>% .[.>0])))
  
  min_time = min(data[[time_var]]); max_time  = max(data[[time_var]]); 
  missing_time = setdiff(min_time:max_time, unique(data[[time_var]]))
  data$expand = expand
  data$time = data[[time_var]]
  data[, id := get(id_var[1])]; if (length(id_var) > 1) for (i in 2:length(id_var)){data[, id := paste(id, get(id_var[i]))] }  
  if(is.na(birth_var)){data[,birth_var := min(time), by = id_var][birth_var == min_time, birth_var :=NA]}else{data$birth_var = data[[birth_var]]}
  if(is.na(death_var)){data[,death_var := max(time), by = id_var][death_var == max_time, death_var :=NA]}else{data$death_var = data[[death_var]]}
  
  
  ### generate the lag / lead variables 
  for (value_var  in value_vars){for (lag_amount in lag_amounts){
    data[, `:=`(value = get(value_var), time_lag = get(time_var) +lag_amount)]
    
    data = merge(data, data[, .(time_lag, id, value)] %>% rename(value_lag= value),
                 by.x = c('id','time'), by.y = c('id', 'time_lag'), all.x = T) 
    
    if (lag_amount > 0) {
      data[, paste0(value_var,"_",'lag',lag_amount) := case_when(
        !is.na(value_lag) | is.na(birth_var) | !expand ~ value_lag,
        (time + 1 - lag_amount == birth_var) &! (time %in% c(missing_time + lag_amount)) ~ expand_value,
        T~ value_lag)]
      
      
    }else{
      lead_amount = -1*lag_amount
      data[, paste0(value_var,"_",'lead',lead_amount) := case_when(
        !is.na(value_lag) | is.na(death_var) | !expand ~ value_lag,
        (time -1 + lead_amount == death_var) &! (time_var %in% c(missing_time + lead_amount)) ~ expand_value,
        T~ value_lag)]
    }
    extra = setdiff(c('value','value_lag', 'time_lag'), value_var)
    data[, (extra) := NULL]
  }}
  
  return(data %>% select(final_columns))
  
}

unbalanced_growth_rate = function(data,id_var, time_var, value_vars, time_horizon,
                                  birth_var = NA,death_var = NA, alt_suffix = NA,expand = F, keep_lags = F){
  
  og_columns = names(data)
  ## generate the initial leads and lags    
  data = unbalanced_lag(data, id_var,time_var,value_vars, lag_amounts = setdiff(-1*time_horizon,0), expand = T,
                        birth_var = birth_var, death_var = death_var)  
  lag_amount = abs(time_horizon[1]); lead_amount = time_horizon[2]
  
  ## generate the growth rates for all variables 
  suffix = ifelse(is.na(alt_suffix), '_growth_rate', alt_suffix)
  fwd = ifelse(lead_amount == 0, "",paste0('_lead', lead_amount))
  bwd =  ifelse(lag_amount == 0, "",paste0('_lag', lag_amount))
  command = paste0(suffix, " = ifelse(fwd - bwd == 0, 0, .5*(fwd - bwd)/(fwd + bwd))")
  command = lapply(value_vars, function(var){paste0(var, gsub('bwd',paste0(var, bwd),gsub('fwd', paste0(var, fwd), command)))}) %>%
    unlist() %>% paste(.,collapse = ",") %>% paste0("data = data[,`:=`(", .,")]")
  eval(parse(text = command))
  
  # drop lags if unwanted 
  final_columns = c(og_columns, paste0(value_vars, suffix))
  if(keep_lags) final_columns = names(data)
  data = data %>% select(final_columns)
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

