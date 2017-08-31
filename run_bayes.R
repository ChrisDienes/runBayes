
# Bayesian Analysis of Running Goal
# Circa 2016
# By https://chrisdienes.github.io/

# ---------------- #
# Load Stuff
# ---------------- #

load(file="C:/Users/Chris/Desktop/runBayes/myResults.RDA")
myPath = "C:/Users/Chris/Desktop/runBayes/"
myData = read.csv(file=paste0(myPath,"myData.csv"), header=TRUE, stringsAsFactors = FALSE)
myData$Total = cumsum(myData$Value)
myData$Goal  = (1000/52)*(1:52)
myData$GoalDiff = myData$Total - myData$Goal
myData$DateOfInterest = ifelse(((1:52) %% 5 == 0) ,1,0)
library(fanplot)
library(animation)

sum(myData$Value,na.rm=TRUE)
sum(myData$TM,na.rm=TRUE)
100 - 100*sum(myData$TM,na.rm=TRUE)/sum(myData$Value,na.rm=TRUE)

# ---------------------------- #
# Functions for analysis
# ---------------------------- #
update_p_posterior = function(x, a_prior, b_prior){
  t = length(x)
  alpha =  a_prior  + sum(x)
  beta  =  b_prior  + t - sum(x)
  return(c(alpha = alpha, beta = beta))
}
my_beta_params = function(beta_mean, beta_var){
  beta  = (beta_mean*(1-beta_mean)^2)/beta_var + beta_mean - 1
  alpha = (beta_mean/(1-beta_mean))*beta
  return(c(alpha = alpha, beta = beta))
}
update_beta = function(upper_limit, lower_limit, new_mean, new_var){
  my_beta_params(beta_mean = (new_mean - lower_limit)/(upper_limit - lower_limit), beta_var = new_var/(up_upper_limit - up_lower_limit)^2)
}
wmean.RSS <- function(data, x_0, par){
  t = length(data)
  tmp = 1:t
  mean_t = x_0
  tmp[1] = (data[1] - mean_t)^2
  for(nn in 2:t){
    mean_t = par*mean_t + (1-par)*data[nn-1]
    tmp[nn] = (data[nn] - mean_t)^2
  }
  sum(tmp)
}
wvar <- function(data, var_0, x_0, wmean, par){
  t = length(data)
  tmp1 = 1:t
  tmp2 = 1:(t-1)
  var_t = var_0
  mean_t = x_0
  tmp1[1] = data[1] - mean_t
  for(nn in 2:t){
    mean_t = wmean*mean_t + (1-wmean)*data[nn-1]
    tmp1[nn] = data[nn] - mean_t
    tmp2[nn-1] = (var(tmp1[1:nn]) - var_t)^2
    var_t = par*var_t + (1-par)*var(tmp1[1:nn])
    #tmp2[nn-1] = (tmp1[nn]^2 - var_t)^2
    #var_t = par*var_t + (1-par)*(tmp1[nn]^2)
  }
  sum(tmp2)
}
report_new_params = function(data, var_0, x_0, wmean, wvar){
  t = length(data)
  if(t > 1){
    tmp1 = 1:t
    #tmp2 = 1:(t-1)
    var_t = var_0
    mean_t = x_0
    tmp1[1] = data[1] - mean_t
    for(nn in 2:t){
      mean_t = wmean*mean_t + (1-wmean)*data[nn-1]
      tmp1[nn] = data[nn] - mean_t
      #tmp2[nn-1] = (var(tmp1[1:nn]) - var_t)^2
      var_t = wvar*var_t + (1-wvar)*var(tmp1[1:nn])
      #var_t = wvar*var_t + (1-wvar)*(tmp1[nn]^2)
    }
  }
  if(t < 2){
    mean_t = x_0
    var_t  = var_0
  }
  return(c(mean = mean_t, var = var_t))
}

# ---------------------------- #
# Setting Initial Conditions
# ---------------------------- #
# Initials: Risk of complete failure:
catastrophe_p = 1-(.95)^(1/52)
# Initials: Evolving probability of down week:
p_prior_mean = 8/52
p_prior_sample_size =  10
alpha_prior = p_prior_mean*p_prior_sample_size
beta_prior  = p_prior_sample_size*(1-p_prior_mean)
# Initials: Evolving up distribution
up_upper_limit = 50
up_lower_limit = 10
up_initial_mean = 21.3
up_initial_sd = 3
up_beta_mean = (up_initial_mean - up_lower_limit)/(up_upper_limit - up_lower_limit)
up_beta_var   = (up_initial_sd/(up_upper_limit - up_lower_limit))^2
up_params = my_beta_params(beta_mean = up_beta_mean, beta_var = up_beta_var)
# Initials: Evolving down distribution
down_upper_limit = 20
down_lower_limit = 0
down_initial_mean = 8
down_initial_sd = 3
down_beta_mean = (down_initial_mean - down_lower_limit)/(down_upper_limit - down_lower_limit)
down_beta_var   = (down_initial_sd/(down_upper_limit - down_lower_limit))^2
down_params = my_beta_params(beta_mean = down_beta_mean, beta_var = down_beta_var)
# Misc plots:
# upp = update_p_posterior(x = x, a_prior = alpha_prior, b_prior = beta_prior)
# windows()
# par(mfrow=c(3,1))
# hist(rbeta(10000, alpha_prior, beta_prior), xlim=c(0,1))
# hist(rbeta(10000, upp[1], upp[2]), xlim=c(0,1))
# p_prior_mean
# mean(x)
# upp[1]/sum(upp)
#hist(rbeta(10000, down_a, down_b), xlim=c(0,1))
# ------------------------------------------------------------------- #
# Below is the update to myResults.RDA ran at the end of each week
# ------------------------------------------------------------------- #
#options(warn=2)
# Get Data:
#S = 0:sum(!is.na(myData$Value))
#S = 35:36
# I've done 0-51:
S = sum(!is.na(myData$Value))
S 
E = 52
I = 1000
hold = list()
for(ss in 1:length(S)){
  set.seed(1000)
  N = S[ss]
  print(N)
  pred = matrix(0, ncol=E, nrow=I)
  for(pp in 1:I){
    N = S[ss]
    if(N > 0){
      y = myData$Value[1:N]
      x = c(myData$X[1:N], rep(NA, E-N))
    }
    if(N == 0){
      N = 1
      x = rbinom(size=1, n=1, prob=p_prior_mean)
      y = ifelse(x == 0, 
                 (up_upper_limit-up_lower_limit)*rbeta(n=1, shape1=up_params[1],shape2=up_params[2]) + up_lower_limit, 
                 (down_upper_limit-down_lower_limit)*rbeta(n=1, shape1=down_params[1],shape2=down_params[2]) + down_lower_limit)
    }
    # start analysis:
    indep_catastrophe = which(rbinom(size = 1, n = E - N, prob = catastrophe_p) == 1)
    indep_catastrophe = ifelse(length(indep_catastrophe) == 0, E+1, N + indep_catastrophe[1])
    #indep_catastrophe
    old_up_beta_params = c(up_params[1], up_params[2])
    old_down_beta_params = c(down_params[1], down_params[2])
    for(nn in (N+1):min(E,indep_catastrophe)){
      upp = update_p_posterior(x = x[1:(nn-1)], a_prior = alpha_prior, b_prior = beta_prior)
      x[nn] = rbinom(size=1, n=1, prob = rbeta(1, upp[1], upp[2]))
      if(x[nn] == 0){
        Y = y[x[1:(nn-1)]==0]
        if(length(Y) > 1){
          optim_wmean = optimize(f = wmean.RSS, interval = c(0,1), data = Y, x_0 = up_initial_mean)$minimum
          optim_wvar = optimize(f = wvar, wmean = optim_wmean,interval = c(0,1), data = Y, x_0 = up_initial_mean, var_0 = up_initial_sd^2)$minimum
        }
        if(length(Y) <= 1){
          optim_wmean = optim_wvar = 1
        }
        new_params = report_new_params(data = Y,  x_0 = up_initial_mean, var_0 = up_initial_sd^2, wmean = optim_wmean, wvar = optim_wvar)
        new_up_beta_params = update_beta(upper_limit = up_upper_limit, lower_limit = up_lower_limit, new_mean = new_params[1], new_var = new_params[2])
        if(any(new_up_beta_params <= 0)){new_up_beta_params = old_up_beta_params}
        old_up_beta_params = new_up_beta_params
        y = c(y, rbeta(n=1, new_up_beta_params[1], new_up_beta_params[2])*(up_upper_limit - up_lower_limit) + up_lower_limit)
      }
      if(x[nn] == 1){
        Y = y[x[1:(nn-1)]==1]
        if(length(Y) > 1){
          optim_wmean = optimize(f = wmean.RSS, interval = c(0,1), data = Y, x_0 = down_initial_mean)$minimum
          optim_wvar = optimize(f = wvar, wmean = optim_wmean,interval = c(0,1), data = Y, x_0 = down_initial_mean, var_0 = down_initial_sd^2)$minimum
        }
        if(length(Y) <= 1){
          optim_wmean = optim_wvar = 1
        }
        new_params = report_new_params(data = Y,  x_0 = down_initial_mean, var_0 = down_initial_sd^2, wmean = optim_wmean, wvar = optim_wvar)
        new_down_beta_params = update_beta(upper_limit = down_upper_limit, lower_limit = down_lower_limit, new_mean = new_params[1], new_var = new_params[2])
        if(any(new_down_beta_params <= 0)){new_down_beta_params = old_down_beta_params}
        old_down_beta_params = new_down_beta_params
        y = c(y, rbeta(n=1, new_down_beta_params[1], new_down_beta_params[2])*(down_upper_limit - down_lower_limit) + down_lower_limit)
      }
    }
    y = cumsum(y)
    L = length(y)
    if(L < E){y = c(y,rep(y[L], E-indep_catastrophe))}
    pred[pp,] = y
  }
  hold[[ss]] = pred
}

#100*mean(hold[[1]][,52] >= 1000)
#hist(hold[[1]][,52])
#summary(hold[[1]][,52])

hold2 = hold
load(file="C:/Users/Chris/Desktop/runBayes/myResults.RDA")
hold = c(hold, hold2)
length(hold)
#save(hold, file="C:/Users/Chris/Desktop/myResults.RDA")

