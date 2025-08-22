# setup -------------------------------------------------------------------
if(exists('base_env')){rm(list= setdiff(ls(), base_env))}else{rm(list = rm(list = ls()))}; gc();
## set working directory dynamically 
{
  library(dplyr)
  root = case_when(
    ## AZM running locally and not testing if it will work CASD 
    grepl("/Users/amagnuson",getwd()) & !grepl('4) exports-imports',getwd()) ~ "/Users/amagnuson/Library/CloudStorage/GoogleDrive-amagnuson@g.harvard.edu/My Drive/Grad School/8) all projects/Trade/Big Data",
    
    ## update as makes sense for CASD / your own use 
    T ~ "C:/Users/Public/Documents/Big data Project")
  setwd(root)
}

## import helper functions 
source('2) code/0_set_parameter_values.R')

# import and clean dta -----------------------------------------------------------------------
min_streak_length = 8
earliest_streak_start = 2000


combined_dta = import_file('1) data/11_parameter_calibration/clean/combined_firm_dta.parquet') %>% 
  
  # remove firms prior to our earliest start date 
  .[fiscal_year >= earliest_streak_start & comp_data !=0] %>% 
  
  # define our streaks as continuous chains of comp_data after a range forecast
  unbalanced_lag(. , 'isin', 'fiscal_year', 'comp_data', 1) %>%
  .[,streak_id := cumsum(is.na(comp_data_lag1))] %>% 
  .[,streak_start := min(ifelse(full_info_for_start, fiscal_year, Inf)), by = streak_id] %>% 
  .[, is_streak_start := fiscal_year == streak_start] %>% 
  
  # remove observations before first range observation 
  .[fiscal_year >= streak_start] %>% 
  .[, streak_length :=.N, by = 'streak_id'] %>% 
  
  # remove observations that are not from long enough streaks 
  .[streak_length >= min_streak_length] %>% 
  
  # define key values 
  .[, x_bar := NA_mean(sales_actual), by = 'streak_id'] %>% 
  .[, t := fiscal_year] %>% 
  .[, L_t := comp_data] %>% 
  .[, FE_t1_t := forecast_error] %>% 
  .[  is_streak_start== F, forecast_range := NA] %>%
  .[is_streak_start== T, E_x_t1_t := sales_forecast] %>%
  .[,firm_num := .GRP, by = isin] %>% 
  .[, .(t, streak_id,is_streak_start, x_bar, L_t, FE_t1_t, E_x_t1_t)] %>% 
  arrange(streak_id, t) %>% 
  .[, age := seq_len(.N) - 1L, by = streak_id] %>% 
  .[, idx := .I] %>% 
  .[, prev_row_idx := shift(idx, 1), by = streak_id]




# perform calibration -----------------------------------------------------
# Define helper functions 
softplus <- function(x) log1p(exp(-abs(x))) + pmax(x, 0)
pospow <- function(x, a) { x <- pmax(x, EPS); if (abs(a) < EPS) rep(1.0, length(x)) else x^a }
log_likelihood_wrapper= function(params, likelihood_type = c("gaussian", "exact")){
  likelihood_type <- match.arg(likelihood_type)
  # unpack params and check feasibility 
  for (name in names(params)) assign(name, params[[name]], envir = environment())
  sig2_a <- sigma_a^2
  th2 = theta_d^2
  denom <- 1 - th2
  Sigma_ub <- Q_d / denom
  A_bar <- mu_a * (Sigma_ub + sig2_a) + softplus(kappa)
  
  
  if (mu_a <= 0 || sigma_a <= 0 || Q_d <= 0 || theta_d <= 0 || theta_d >=1 || alpha_1 < 0 || alpha_2 < 0 || phi_d < 0) return(-BIGPEN)
  if (denom <= EPS) return(-BIGPEN)
  n = nrow(DT)
  Sigma_tt  = numeric(n); Sigma_t1t = numeric(n); Sigma_t1t1 = numeric(n)
  Ex_tt = numeric(n); Ex_t1t1 = numeric(n)
  
  ## Kalman Iteration 
  for (k in ages){
    
    idx = rows_by_age[[k+1]]
    # IF K= 0 invert E_x10 to obtain Sigma_tt and Ex_tt
    if(k== 0){
      E_x10 = Ex_t1t[idx]
      x_bar0 = x_bar[idx]
      Sigma_00 <- ((A_bar - E_x10 / pmax(x_bar0, EPS)) / pmax(mu_a, EPS) - sig2_a - Q_d) / pmax(th2, EPS)
      Sigma_00[!is.finite(Sigma_00)] <- 0.5 * Sigma_ub
      Sigma_00 = pmin(pmax(Sigma_00, EPS), Sigma_ub - 1e-6)
      Sigma_tt[idx] =Sigma_00
      Ex_tt[idx] = x_bar0 * (A_bar - mu_a*(Sigma_00 + sig2_a)) 
    }
    # IF K!= 0 obtain Sigma_tt and Ex_tt from the previous iteration 
    if (k!=0){ 
      Sigma_tt[idx] = Sigma_t1t1[prev_idx[idx]]
      Ex_tt[idx] =  Ex_t1t1[prev_idx[idx]]
    }
    
    # Apply our kalman filter results 
    Sigma_t1t[idx] = pmax(th2 *Sigma_tt[idx] + Q_d, EPS)
    R_t1 = phi_d*pospow(L_t[idx], alpha_1)*pospow(Ex_tt[idx], alpha_2) + 1/sig2_a
    if (any(!is.finite(R_t1) | R_t1 <= 0)) return(-BIGPEN)
    K_t1 = Sigma_t1t[idx] / (Sigma_t1t[idx] + 1/R_t1)
    
    if (k != last_age){
      Sigma_t1t1[idx] = pmin(pmax((1 - K_t1)*Sigma_t1t[idx], EPS), Sigma_ub - 1e-6)
      Ex_t1t1[idx] = x_bar[idx] * (A_bar - mu_a*(Sigma_t1t1[idx] + sig2_a)) 
    }
  }
  
  ## drop first observation of streak
  idx_FE = which(DT$age >= 4 & !is.na(DT$FE_t1_t))
  
  # Build S = xbar * mu_a * (Sigma_{t+1|t} + sigma_a^2)
  Sig_t1t = Sigma_t1t[idx_FE]
  xbar_i = pmax(x_bar[idx_FE], EPS)
  S = xbar_i * mu_a * (Sig_t1t + sig2_a)
  
  ## calculate the likelihood
  if(likelihood_type == 'gaussian'){
    ll = sum(dnorm(FE_t1t[idx_FE], mean = 0, sd = sqrt(2)*S, log = TRUE))
  }
  if(likelihood_type == 'exact'){

     U <- 1 - FE_t1t[idx_FE] / pmax(S, EPS)
     if (any(!is.finite(U) | U <= 0)) {
       ll = -BIGPEN
     } else {
       # f_FE(f) = f_chi2(U; df=1) * (1 / S)
       ll = sum(dchisq(U, df = 1, log = TRUE) - log(pmax(S, EPS)))
     }
  }
  #cat(sprintf("LogLik (%s): %.6f\n", likelihood_type, ll))
  return(ll)
}
make_start = function(){
  # rough data-driven scales
  fe_sd  <- sqrt(mean(FE_t1t^2, na.rm = TRUE))
  xb_med <- median(x_bar, na.rm = TRUE)
  L_med  <- median(pmax(L_t, 1), na.rm = TRUE)
  
  c(
    kappa   = 0,                               # softplus(0) ~ 0.69
    mu_a    = runif(1, 0.01, 2),               # positive
    phi_d   = 1 / max(L_med, 1),               # scale with L
    alpha_1 = runif(1, 0.1, .9),
    alpha_2 = runif(1, 0.1, .9),
    sigma_a = max(fe_sd / max(xb_med, 1), 0.3),
    Q_d     = runif(1, 0.05, 1.0),
    theta_d = runif(1, 0.2, 0.9)               # inside (0,1)
  )
}
nll_obj = function(params, likelihood_type){
  params = params[names(LB)] # enforce order
  params = as.list(params)
  ll = log_likelihood_wrapper(params, likelihood_type = likelihood_type)
  if (!is.finite(ll)) return(1e12)
  -ll
}
run_lbfgsb <- function(start, likelihood_type, control = list()) {
  start <- pmin(pmax(start, LB), UB)  # clip to box
  optim(
    par     = start[names(LB)],
    fn      = nll_obj,
    likelihood_type = likelihood_type,
    method  = "L-BFGS-B",
    lower   = LB,
    upper   = UB,
    control = modifyList(list(maxit = 4000, factr = 1e8, pgtol = 1e-8,lmm = 8, parscale = abs(start) + 1),
                         control)
  )
}
run_nlminb <- function(start, likelihood_type){
  obj <- function(p) {
    p <- p[names(LB)]
    params <- as.list(p)
    ll <- log_likelihood_wrapper(params, likelihood_type = likelihood_type)
    if (!is.finite(ll)) return(1e12)
    -ll
  }
  nlminb(
    start   = pmin(pmax(start[names(LB)], LB), UB),
    objective = obj,
    lower   = LB,
    upper   = UB,
    control = list(eval.max = 4000, iter.max = 4000, rel.tol = 1e-8, step.min = 1e-8)
  )
}
run_mle <- function(likelihood_type = c("gaussian","exact"), n_starts = 8, control = list()) {
  likelihood_type <- match.arg(likelihood_type)
  set.seed(1)
  
  #Generate the random parameter starts 
  starts <- replicate(n_starts, make_start(), simplify = FALSE)
  
  # evaluate each
  fits <- lapply(1:n_starts, function(s){print(paste0('start: ', s,"/", n_starts)); run_lbfgsb(starts[[s]], likelihood_type, control)}) 
  nlls <- vapply(fits, `[[`, numeric(1), "value")
  best <- which.min(nlls)
  fit  <- fits[[best]]
  if (fit$convergence != 0) {
    fit <- run_lbfgsb(fit$par, likelihood_type, control = list(maxit=6000, factr=1e9, pgtol=1e-9, lmm=6))
  }
 
  # tidy output
  par_hat <- fit$par
  nll_hat <- fit$value
  conv    <- fit$convergence
  
  # output A_Bar
  th2_hat      <- par_hat["theta_d"]^2
  denom_hat    <- 1 - th2_hat
  Sigma_ub_hat <- par_hat["Q_d"] / denom_hat
  sig2_hat     <- par_hat["sigma_a"]^2
  par_hat['A_bar'] = par_hat["mu_a"] * (Sigma_ub_hat + sig2_hat) + softplus(par_hat["kappa"])
  
  output = list(
    likelihood_type = likelihood_type,
    par = par_hat,
    nll = nll_hat,
    ll  = -nll_hat,
    convergence = conv,        # 0 == success
    message = fit$message
  )
  return(output)
}


## SETUP 
ages <- sort(unique(combined_dta$age))
rows_by_age <- lapply(ages, function(a) combined_dta[age == a, idx])
x_bar <- combined_dta$x_bar
L_t    <- combined_dta$L_t
FE_t1t   <- combined_dta$FE_t1_t
Ex_t1t   <- combined_dta$E_x_t1_t
prev_idx <- combined_dta$prev_row_idx
EPS    <- 1e-10
BIGPEN <- 1e8
DT = combined_dta
last_age <- max(ages)
LB <- c(kappa = -Inf, mu_a = 1e-6, phi_d = 1e-6, 
        alpha_1 = 0, alpha_2 = 0, sigma_a = 1e-6,
        Q_d = 1e-8, theta_d = 1e-6)

UB <- c(kappa = Inf, mu_a = Inf, phi_d = Inf,
        alpha_1 = 1 - 1e-6, alpha_2 = 1 - 1e-6, # guarantee decreasing marginal returns 
        sigma_a = Inf, Q_d = Inf, theta_d = 1 - 1e-6) # < 1 for stationarity

## RUN MLE 
run_mle("gaussian", n_starts = 10)





# MLE ---------------------------------------------------------------------









  


