#*************************************************************
#### Advanced Econometrics - Assignment 2 #####
##   Paola Sastre Dordelly
#*************************************************************


# In economics, the law of one price and the purchase power parity (PPP) hypothesis both
# predict that, under certain conditions, real exchange rates should remain fixed over time.
# In particular, these predictions should hold true if there are no transaction costs of any
# kind.
# Given the presence of transport costs and barriers to trade, there is wide consensus
# among economists that, in practice, real exchange rates should be ‘free to fluctuate’, but
# only when they are near equilibrium. In other words, while non-stationary dynamics
# seem acceptable close to equilibrium, overall, the real exchange rates should be ‘stationary’,
# ‘stable’ or ‘mean reverting’, and hence return to their equilibrium values in the
# long-run.
# This description of the behavior of real exchange rates suggests the existence of nonlinear
# dynamics. As an econometrician, you surely want to test the validity of this claim!
#   Does the data support these theories? Are exchange rates ‘free to fluctuate’ close to equilibrium
# but overall stationary and mean-reverting?


# Suppose that the parameters theta = (alpha, beta, delta, gamma, mu, sigma_e^2) of 
# the Gaussian Exponential SESTAR model are estimated by maximum likelihood (ML) 
# on a compact parameter. Assume that the ML estimator is asymptotically normal and that the estimates
# yielded the following results:

# The tasks can be found after the housekeeping part

### 0. HOUSEKEEPING ####
#***********************

## 0.1 Clear session ####
rm(list = ls())    # clear workspace
if(!is.null(dev.list())) dev.off() # Clear previous plots
cat("\014")        # clear command window

## 1. Do you reject the hypothesis that gamma = 0? What does that tell you about the dynamic
# behavior of this real exchange rate?

# Meaning: perform a t-test, for gamma Ho=0, Ha=/0

n = 196 # Number of observations
est_sestar <- data.frame(99.763,0.5914,0.4037,0.1113,99.7423) # model estimates for the paremeters
ho <- 0 # two sided test for the significance of the estimates
stderr_sestar <- data.frame(0.5231,0.2101,0.0751,0.0180,2.4971) #stderr
alpha<-0.05 #assume alpha significance level

t <- (est_sestar[1,3]-ho)/(stderr_sestar[1,3])
t.Ha <- qnorm(1 - alpha/2)
# upper limit
# Reject?
print(t > abs(t.Ha))
#lower limit
# Reject?
print(t < -abs(t.Ha))

# Conclusion: the value does fall in the rejection region --> we reject HO. 
# The results are significant at the 5% level.

# 2. Suppose that you also obtained parameters estimates for the linear
# Gaussian AR(1) model. Which model do you think is best?

# Meaning: check significance levels for the parameters 
cat("\014")                        # clear command window
rm(list = ls())                    # clear workspace
if(!is.null(dev.list())) dev.off() # Clear previous plots

n = 196                                     # Number of observations
est_AR1 <- data.frame(2.0750,0.9793,1.9279) # model estimates for the paremeters
hoAR1 <- c(0,0,0)                           # two sided test for the significance of the estimates
stderr_AR1 <- c(0.4131,0.9193,0.1779)       # stderr
alpha<-0.05                                 #assume alpha significance level

t <- (est_AR1-hoAR1)/(stderr_AR1)
t.Ha <- qnorm(1 - alpha/2)

# upper limit
# Reject?
print(t > abs(t.Ha))
# lower limit
# Reject?
print(t < -abs(t.Ha))
# The value does fall in the rejection region --> we reject HO.

# Calculations of the AIC
AIC_sestar = -2*(log(-216.15)) + 2*6
AIC_AR = -2*(log(-238.12)) + 2*3



# 3. Calculations related to the AR model for questions 4 and 5####
# AR (1)  simulation ####
rm(list = ls())    # clear workspace
if(!is.null(dev.list())) dev.off() # Clear previous plots
cat("\014")        # clear command window

h<-12                         #set steps ahead
M<-10000                      #set number of draws
x_t<-105.33                   #parameter values
omega<-2.0750
phi<-0.9793
sigma<-1.9279
errores<-rnorm(10000, 0, sqrt(sigma))

#Forward iterate AR(1) model to obtain forecast
xforecast  <- matrix(c(0:0), nrow = M, ncol = h+1)  # define length of forecast vector (including observed data)
xforecast[,1] = x_t;                                # set first T periods equal to observed data
for (m in 1:M){                                     # start forward iteration loop
  for (t in 1:h)   { 
  xforecast[m,(t+1)] = omega + phi*xforecast[m,t]+ errores[m]
  }
}

x_hat<-matrix(0, 1, ncol=1)
ub<-matrix(0, 1, ncol=h)
lb<-matrix(0, 1, ncol=h)

x_hat[,1]<-x_t
ub[,1]<-x_t
lb[,1]<-x_t
for (t in 1:h) {
  x_hat[t+1]=mean(xforecast[,t+1])
  ub[t+1]<-quantile(xforecast[,t+1],0.90)
  lb[t+1]<-quantile(xforecast[,t+1],0.10)
}

x_hatt<-data.frame(
  h_ax<-1:(h+1),
  x_hat<-x_hat
)

upper_bound<-data.frame(
  h_ax<-1:(h+1),
  upper_b<-ub
)

lower_bound<-data.frame(
  h_ax<-1:(h+1),
  lower_b<-lb
)

## Plot observed data and forecast
plot1 <- ggplot()  +
  geom_line(data = x_hatt, aes(x = h_ax, y = x_hat), color = "black") +
  geom_line(data = lower_bound, aes(x = h_ax, y = lower_b), color = "blue") +
  geom_line(data = upper_bound, aes(x = h_ax, y = upper_b), color = "blue")
  
plot1+labs(title="Forecast RER: AR(1) model") +xlab("Months ahead") + ylab("Forecast Real Exchange Rates")

# 4. FRMSE AR(1)#### 
# 4.1 Create the validation sample
val_sample <- data.frame(
  c(103.26, 101.73, 101.14, 100.79, 101.19, 101.24, 100.26, 100.72, 101.02, 100.08, 99.526, 98.956))
P<-12

# 4.2 Define function to add first 12 elements
FRMSE <- matrix(c(0:0), nrow = P, ncol = 1)  #define length of vector 
for (p in 1:P)   { 
  FRMSE[p,1] = ((val_sample[p,1] - x_hatt[(p+1),2])^2)
}

FRMSEAR1<-((1/P)*sum(FRMSE))^(1/2)
FRMSEAR1

# 4.2 FRMSE AR(1)#### 
# 4.2.1 Define function to add 12 elements to compute
FMAE <- matrix(c(0:0), nrow = P, ncol = 1)  #define length of vector 
for (p in 1:P)   { 
  FMAE[p,1] = (abs(val_sample[p,1] - x_hatt[(p+1),2]))
}
FMAEAR1<-((1/P)*sum(FMAE))
FMAEAR1


# 5. Calculations related to the SESTAR model for questions 4 and 5####
# SESTAR  simulation ####
rm(list = ls())                    # Clear workspace
if(!is.null(dev.list())) dev.off() # Clear previous plots
cat("\014")                        # Clear command window

h<-12                               # Set steps ahead
M<-10000                            # Set number of draws
x_t<-105.33                         # Parameter values
alpha<-99.763
omega<-2.0750
delta<-0.5914
gamma<-0.4037
beta<-0.1113
mu<-99.7423
sigma<-0.9670
errores<-rnorm(10000, 0, sqrt(sigma)) # Set the errors

# Forward iterate SESTAR model to obtain forecast
xforecast  <- matrix(c(0:0), nrow = M, ncol = h+1)  #define length of forecast vector (including observed data)
xforecast[,1] = x_t;     #set first T periods equal to observed data
for (m in 1:M){  # start forward iteration loop
  for (t in 1:h)   { 
    xforecast[m,(t+1)] = alpha + (delta+(gamma/(1+exp(beta*(xforecast[m,t]-(mu))^2))))*(xforecast[m,t]-mu) + errores[m]
  }
}

x_hat<-matrix(0, 1, ncol=1)
ub<-matrix(0, 1, ncol=h)
lb<-matrix(0, 1, ncol=h)

x_hat[,1]<-x_t
ub[,1]<-x_t
lb[,1]<-x_t
for (t in 1:h) {
  x_hat[t+1]=mean(xforecast[,t+1])
  ub[t+1]<-quantile(xforecast[,t+1],0.90)
  lb[t+1]<-quantile(xforecast[,t+1],0.10)
}

x_hatt<-data.frame(
  h_ax<-1:(h+1),
  x_hat<-x_hat
)

upper_bound<-data.frame(
  h_ax<-1:(h+1),
  upper_b<-ub
)

lower_bound<-data.frame(
  h_ax<-1:(h+1),
  lower_b<-lb
)

## Plot observed data and forecast
plot2 <- ggplot()  +
  geom_line(data = x_hatt, aes(x = h_ax, y = x_hat), color = "black") +
  geom_line(data = lower_bound, aes(x = h_ax, y = lower_b), color = "blue") +
  geom_line(data = upper_bound, aes(x = h_ax, y = upper_b), color = "blue")

plot2+labs(title="Forecast RER: SESTAR model") +xlab("Months ahead") + ylab("Forecast Real Exchange Rates")


# 6.1 FRMSE GAUSSIAN SESTAR#### 
# Create the validation sample
val_sample <- data.frame(
  c(103.26, 101.73, 101.14, 100.79, 101.19, 101.24, 100.26, 100.72, 101.02, 100.08, 99.526, 98.956))
P<-12
# Define function to add 12 elements to compute
FRMSE <- matrix(c(0:0), nrow = P, ncol = 1)  #define length of vector 
for (p in 1:P)   { 
  FRMSE[p,1] = ((val_sample[p,1] - x_hatt[(p+1),2])^2)
}
FRMSESESTAR<-((1/P)*sum(FRMSE))^(1/2)
FRMSESESTAR

# 6.2 FMAE GAUSSIAN SESTAR#### 
# Define function to add 12 elements to compute
FMAE <- matrix(c(0:0), nrow = P, ncol = 1)  #define length of vector 
for (p in 1:P)   { 
  FMAE[p,1] = (abs(val_sample[p,1] - x_hatt[(p+1),2]))
}
FMAESESTAR<-((1/P)*sum(FMAE))
FMAESESTAR














