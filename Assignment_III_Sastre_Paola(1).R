#*************************************************************
#### Advanced Econometrics - Assignment 3 #####
##   Paola Sastre Dordelly
#*************************************************************

# In economics and finance, it is well known that the conditional volatility of financial returns
# evolves slowly over time. Financial returns are thus characterized by the presence
# of ‘clusters of volatility’. Recent evidence suggests that robust volatility filters might be
# needed for correctly capturing the time-varying conditional volatility in stock returns.
# Furthermore, there are also reasons to believe that large negative returns produce more
# volatility than large positive returns. This characteristic, first noted by Black (1976), is
# known as the leverage effect. Some analysts claim that the leverage effect plays a very
# important role in financial markets. As an econometrician, you surely want to test these
# claims! Do robust filters really perform better? Does the data support the existence of a
# ‘leverage effect’?
# Consider the following Robust-GARCH-with-Leverage-Effect model for time-varying volatilities


### 0. HOUSEKEEPING ####
#***********************

## 0.1 Clear session ####
rm(list = ls())                     # Clear workspace
if(!is.null(dev.list())) dev.off()  # Clear previous plots
cat("\014")                         # Clear command window

setwd("~/Desktop/Econometría:Assignment" )
data<-read.csv("financial_returns_data.csv", header=FALSE)
x<-data$V2

# In the file financial_returns_data.mat you can find two series
# of inflation-adjusted daily returns on complex derivatives. The underlying value of
# each derivative is derived from the performance of a number of composite indices
# that reflect the market value of large companies in the US, Europe and Japan. For
# the first series only, make use of a GARCH model as well as the Robust GARCHwith-
# Leverage-Effect model above to filter the time-varying conditional volatility
# present in these stock market indices. Report the estimated parameters for both
# models.

# 1.1 Create a seq
seq<- seq(from = -25, to = 25, by = 0.01)
M <- length(seq)   

# 1.2 Estimated parameters
omega1 <- 0.2959
alpha1 <- 0.1155
beta1 <- 0.8667#garch
omega2    <- 0.0191
alpha2    <- 0.1262
beta2     <- 0.8896
delta2    <- -1.1786
lambda2   <- 8.4056
rho2   <- 0.1170#rgarch


# 1.3 Model the NIC
NIC1 = matrix(c(0:0), M, 1)    
NIC2 = matrix(c(0:0), M, 1)   
for (i in 1:M) {
  NIC1[i] <- alpha1*seq[i]^2 
  NIC2[i] <- alpha2*((seq[i]^2)+ delta2*(seq[i]))/(1+(rho2/lambda2)*(seq[i]^2))
}

# 1.4 Plot the News Impact Curves
NIC1_df <- data.frame(M, NIC1) 
NIC2_df <- data.frame(M, NIC2)

plot_NIC <- ggplot() + 
  geom_line(data = NIC1_df, aes(x = seq, y = NIC1), color = "black") +
  geom_line(data = NIC2_df, aes(x = seq, y = NIC2), color = "blue")+
  labs(title="News Impact Curve of GARCH and RGARCH") +xlab("evaluated period") + ylab("")
plot_NIC

# 2. Use the parameter estimates of the second in the data set return
# series provided above to demonstrate in one plot the estimated news-impactcurves
# of GARCH and Robust-GARCH-with-Leverage-Effect. Compare the estimated
# news-impact curves. Looking at both graphs, what does it tell you about the
# dynamic behavior of the estimated volatilities?

T <- length(x)

# 2.1 Sigmas for GARCH
sigma1 <- matrix(c(0:0), nrow = T,  ncol = 1)    
sigma1[1] <- var(x) 
for (t in 1:T) {
  sigma1[t+1] <- omega1 + alpha1*x[t]^2 + beta1*sigma1[t]
}

# 2.2 Sigmas for RGARCH
sigma2 <- matrix(c(0:0), nrow = T, ncol = 1)    
sigma2[1] <- var(x)                           
for (t in 1:T) {
  sigma2[t+1] <- omega2 + alpha2*((x[t]^2)+ delta2*(x[t]))/(1+(rho2/lambda2)*(x[t]^2)) + beta2*sigma2[t]
  }

# 3. With the data and given parameter estimates, obtain the filtered
# volatility of both GARCH and Robust-GARCH-with- Leverage-Effect for the second
# series only. Calculate the 5% value-at-risk (VaR) implied by the filtered volatilities,
# and plot both VaRs together with the returns.

# 3.1 Compute VaR
VAR1<- matrix(c(0:0), nrow = T,  ncol = 1)
VAR2<- matrix(c(0:0), nrow = T,  ncol = 1)
for(t in 1:T){
  VAR1[t] =  qnorm(0.05, mean = 0, sd = sqrt(sig_garch[t])) 
  VaR05_rob[t] = qnorm(0.05, mean = 0, sd = sqrt(sig_rob[t])) 
}

VaR05_df_garch <- data.frame(obs = 1:nrow_2, VaR05_garch = VaR05_garch) 
VaR05_df_rob <- data.frame(obs = 1:nrow_2, VaR05_rob = VaR05_rob)

graphVar05 <- ggplot() + 
  geom_line(data = VaR05_df_garch, aes(x = obs, y = VaR05_garch), color = "red") +
  geom_line(data = VaR05_df_rob, aes(x = obs, y = VaR05_rob), color = "green")

graphVar05-serie1 <- returns_data %>% select(V1) %>% mutate(V1 = demean(V1)) %>% transmute(serie = V1*100)

x <- serie1$serie
library(Jmisc)


# 3.2 Compute VaR 
VaR05_garch <- matrix(0, nrow = nrow_2, ncol = 1) 
VaR05_rob <- matrix(0, nrow = nrow_2, ncol = 1) 
for(t in 1:nrow_2){
  VaR05_garch[t] = qnorm(0.05, mean = 0, sd = sqrt(sig_garch[t])) 
  VaR05_rob[t] = qnorm(0.05, mean = 0, sd = sqrt(sig_rob[t])) 
}

VaR05_df_garch <- data.frame(obs = 1:nrow_2, VaR05_garch = VaR05_garch) 
VaR05_df_rob <- data.frame(obs = 1:nrow_2, VaR05_rob = VaR05_rob)

graphVar05 <- ggplot() + 
  geom_line(data = VaR05_df_garch, aes(x = obs, y = VaR05_garch), color = "red") +
  geom_line(data = VaR05_df_rob, aes(x = obs, y = VaR05_rob), color = "green")
graphVar05

# Considering the 5% VaRs that you calculated in the previous
# question what is the coverage obtained from the two models?

serie2 <- returns_data %>% select(V2)
x2 <- serie2$V2
T <- length(x)

##GARCH ##
# Parameters
omega1 = 0.2959
alpha1 = 0.1155
beta1 = 0.8667

# Generate fitted values 
sigma_garch = matrix(0, nrow = T, ncol = 1)    # Create an empty matrix 
sigma_garch[1] = var(x)                            # starting value is var()
for (i in 1:T) {
  sigma_garch[i+1] <- omega1+ alpha1*x[i]^2 + beta1*sig_garch[i]
}

## ROBUST GARCH##
# Parameters:
omega2 = 0.0191
alpha2 = 0.1262
beta2 = 0.8896
delta2= -1.1786
lambda2= 8.4056
rho2 = 0.1170

# Generate fitted values 
sigma_rgarch = matrix(0, nrow = T, ncol = 1)    # Create an empty matrix 
sig_rgarch[1] = var(x)                            # starting value is var(x)
for (i in 1:T) {
  sigma_rgarch[i+1] <- omega2 + alpha2*((x[i]^2)) + delta2*x[i]
}

# Compute VaR
VaR05_garch_model <- matrix(0, nrow = T, ncol = 1) 
VaR05_robust_model <- matrix(0, nrow = T, ncol = 1) 
for(t in 1:T){
  VaR05_garch[t] = qnorm(0.05, mean = 0, sd = sqrt(sig_garch_model[t])) 
  VaR05_rob[t] = qnorm(0.05, mean = 0, sd = sqrt(sig_robust_model[t])) 
}

VaR05_df_garch <- data.frame(obs = 1:T, VaR05_garch = VaR05_garch) 
VaR05_df_rob <- data.frame(obs = 1:T, VaR05_rob = VaR05_rob)

graphVar05 <- ggplot() + 
  geom_line(data = VaR05_df_garch, aes(x = obs, y = VaR05_garch), color = "red") +
  geom_line(data = VaR05_df_rob, aes(x = obs, y = VaR05_rob), color = "green")
graphVar05                                         



VAR1<- matrix(c(0:0), nrow = T,  ncol = 1)
VAR2<- matrix(c(0:0), nrow = T,  ncol = 1)
for(t in 1:T){
  VAR1[t] <-qnorm(0.05, 0, sqrt(sigma1[t]))
  VAR2[t] <- qt(0.05, 15)*sqrt(sigma2[t])
}

VAR1_df <- data.frame(1:T, VAR1) 
VAR2_df <- data.frame(1:T, VAR2)

plot2 <- ggplot() + 
  geom_line(data= data, aes(1:T, y=x), color="blue")+
  geom_line(data = VAR1_df, aes(1:T, y = VAR1), color = "black") +
  geom_line(data = VAR2_df, aes(1:T, y = VAR2), color = "orange")

plot2

# Coverage?
cov_1 <-(sum(x < VAR1))/T
cov_2 <-(sum(x < VAR2))/T