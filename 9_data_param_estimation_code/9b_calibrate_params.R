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

# helper functions --------------------------------------------------------
map_params = function(eta, theta_max = .999, alpha_max = .999){
 names(eta) = eta_names
   list(
    mu_a    = softplus(eta[["mu_a_raw"]]),
    phi_d_tilde   = softplus(eta[["phi_d_tilde_raw"]]),
    phi_d = softplus(eta[["phi_d_tilde_raw"]])/(scale_L*scale_E),
    alpha_1 = alpha_max *logistic(eta[["alpha_1_raw"]]),
    alpha_2 = alpha_max*logistic(eta[["alpha_2_raw"]]),
    sigma_a = softplus(eta[["sigma_a_raw"]]),
    Q_d     = softplus(eta[["Q_d_raw"]]),
    theta_d = theta_max * logistic(eta[["theta_d_raw"]]),
    #kappa   = eta[["kappa_raw"]],
    f0      = logistic(eta[["f0_raw"]])   
  )
}

# import and clean dta -----------------------------------------------------------------------
min_streak_length = 8
interest_year_range = year_range
vars_to_windsorize = c('L_t', 'E_xt_tminus1') #, 'FE_tplus1_t'
min_age_for_likelihood = 3;
dta = import_file('1) data/11_parameter_calibration/clean/combined_firm_dta.parquet') %>% 
  # remove observations with no data, prior to our earliest start date, with extreme forecast errors  
  .[fiscal_year %in% interest_year_range & comp_data !=0 & !is.na(E_xt_tminus1)] %>%
  arrange(isin, fiscal_year) %>%
  unbalanced_lag(. , 'isin', 'fiscal_year', 'comp_data', 1) %>%
  .[,streak_id := cumsum(is.na(comp_data_lag1))] %>% .[,comp_data_lag1 := NULL] %>% 
  .[,streak_start := min(fiscal_year), by = streak_id] %>% 
  .[, is_streak_start := fiscal_year == streak_start] %>% 
  
  # remove observations before first range observation 
  .[fiscal_year >= streak_start] %>% 
  .[,streak_length :=.N, by = 'streak_id'] %>% 
  
  # remove observations that are not from long enough streaks 
  .[streak_length >= min_streak_length] %>% 
  
  # define key values 
  .[, x_bar := NA_median(xt), by = 'streak_id'] %>% 
  .[, L_t := comp_data]  %>% 
  .[, age := seq_len(.N) - 1L, by = streak_id] %>% 
  .[, idx := .I] %>% 
  .[, prev_row_idx := shift(idx, 1), by = streak_id] %>%

  # windsorize variables relative to x_bar 
  .[, (vars_to_windsorize) := lapply(vars_to_windsorize, function(x) windsorize(., x, 'x_bar', c(.05,.95)))]


## define key vectors 
ages <- sort(unique(dta$age))
rows_by_age <- lapply(ages, function(a) dta[age == a, idx])
prev_idx <- dta$prev_row_idx
n = nrow(dta)
scale_L = median(dta$L_t, na.rm=TRUE)
scale_E = median(dta$E_xt_tminus1, na.rm=TRUE)
L_t = dta$L_t / scale_L # we use scaled versions of L_t and E_xt_tminus1 which means what we estimate is tilde_phi_d = phi_d*scale_L*scale_E
E_xt_tminus1 = dta$E_xt_tminus1 / scale_E
FE_tplus1_t = dta$FE_tplus1_t
idx_FE = dta$age >= min_age_for_likelihood  & !is.na(FE_tplus1_t)
x_bar = dta$x_bar
EPS    <- 1e-10

eta0 = list(mu_a_raw = softplus_inv(1.2), 
           phi_d_tilde_raw = .1*scale_L*scale_E,
           alpha_1_raw = qlogis(.5),
           alpha_2_raw = qlogis(.1), 
           sigma_a_raw = softplus_inv(1.1777),
           Q_d_raw = softplus_inv(.66),
           theta_d_raw = qlogis(.45),
           #kappa_raw = 0,
           f0_raw = qlogis(.5))
eta_names = names(eta0)


# log_likelihood calc  -------------------------------------------------------
calc_log_likelihood = function(eta, mode = c('calc', 'diagnostic')){
mode = match.arg(mode)
params = map_params(eta)

## ACTUAL FUNCTION 
for (name in names(params)) assign(name, params[[name]], envir = environment())
sig2_a <- sigma_a^2
th2 = theta_d^2
Sigma_ub <- Q_d / (1 - th2) #theta is bounded (0,1)
#A_bar <- mu_a * (Sigma_ub + sig2_a) + softplus(kappa)

#Initialize Sigma_tt --> set all firms initial uncertainty to .5*Sigma_ub
idx = rows_by_age[[1]]
Sigma_t_t = numeric(n); Sigma_tplus1_t = numeric(n); Sigma_t_tminus1 = numeric(n); R_t = numeric(n); K_t = numeric(n)
Sigma_t_t[idx] = f0*Sigma_ub
Sigma_tplus1_t[idx] = Sigma_t_t[idx]*th2 + Q_d

for (k in ages[-1]){
  idx = rows_by_age[[k+1]]
  Sigma_t_tminus1[idx] = pmax(Sigma_tplus1_t[prev_idx[idx]],0)
  R_t[idx] = phi_d_tilde*L_t[idx]^alpha_1*E_xt_tminus1[idx]^alpha_2 + sig2_a^(-1) #note that transformed min L_t =.019, min E_xt_tminus1 = .0095 so not going to do pos protections 
  R_t[idx] = pmax(R_t[idx], EPS)
  K_t[idx] = Sigma_t_tminus1[idx]/ (Sigma_t_tminus1[idx] + R_t[idx]^(-1))
  Sigma_t_t[idx] = (1- K_t[idx]) * Sigma_t_tminus1[idx]; Sigma_t_t[idx] = pmax(Sigma_t_t[idx], 0)
  Sigma_tplus1_t[idx] = pmax(Sigma_t_t[idx]*th2 + Q_d, EPS)
}

## perform the likelihood 
pi_mix <- 0.01   # amount of guassian in estimation. Stops us falling off cliffs 
S = x_bar[idx_FE] * mu_a * (Sigma_tplus1_t[idx_FE] + sig2_a) # note with our earlier bounding of Sigma_tplus1_t, this is guaranteed positive 
U = 1 - FE_tplus1_t[idx_FE] / S 


log_exact <- dchisq(U, df=1, log=TRUE) - log(S)
log_norm  <- dnorm(FE_tplus1_t[idx_FE], mean=0, sd=sqrt(2)*S, log=TRUE)

log_a <- log1p(-pi_mix) + log_exact
log_b <- log(pi_mix)    + log_norm
ll    <- matrixStats::rowLogSumExps(cbind(log_a, log_b))  
ll_sum <- sum(ll)
nll = -ll_sum
if (mode == 'calc'){ return(nll)}
if (mode == 'diagnostic'){
output = dta %>% 
  .[,.(isin,x_bar, fiscal_year,age, FE_tplus1_t,
       L_t,E_xt_tminus1, Sigma_tplus1_t, Sigma_t_t)] %>% 
  .[idx_FE == T, ll := ll]
return(list(output_df = output, output_params = params))
}
}                               
obj <- function(eta) calc_log_likelihood(eta, 'calc' )
res <- nloptr::nloptr(
  x0 = as.numeric(eta0),
  eval_f = function(x) list(objective = obj(x), gradient = numeric(length(x))),
  opts = list(algorithm = "NLOPT_LN_BOBYQA", maxeval = 5000L, xtol_rel = 1e-8)
)
par_hat <- map_params(res$solution)
output = calc_log_likelihood(par_hat, 'diagnostic')
output_params = output$output_params
output_df =output$output_df
NA_mean(dta$E_xtplus1_t / dta$x_bar +
          output_params$mu_a*(output_df$Sigma_tplus1_t + output_params$sigma_a^2))

output_params$Q_d / output_params$theta_d^2 + output_params$sigma_a^2 


