#*************************************************************
#### Advanced Econometrics - Assignment 1 #####
##   Paola Sastre Dordelly
#*************************************************************

### 0. HOUSEKEEPING ####
#***********************

## 0.1 Clear session ####

rm(list = ls())                       # Clear previous environment
if(!is.null(dev.list())) dev.off()    # Clear previous plots

## 0.2 Set working directory ####

setwd("/Users/paolas.dordelly/Documents/VU/Advanced Econometrics/Assignment Econometrics I")

## 0.3 Download needed packages iff they are not installed yet  ####
if (!require("readr")) install.packages("readr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(readr)
library(ggplot2)
library(dplyr)

# 0.4 Read the files 
wind_data<- read.csv("wind_data.csv", header = FALSE, sep = ",", dec = ".")   # read csv file 
names(wind_data)
colnames(wind_data)<- c("wind direction")

# 0.5 Parametrization (can be "fined tuned" just by changing the values)
alpha<-0.5
beta<-1
c<-0.1

# 1. Plot the wind direction data.
wind_plot<-plot.ts(wind_data, ylab="wind direction", xlab="seconds")
wind_plot

# 2. Calculate the efficiency obtained when the lagged raw wind direction
# data is used directly to set the rotor’s position; i.e. set
# r_t = w_{t-1} and calculate 1/t \sum_{t=1}^T 100 *(1 -((c^2y_t^2)/(1+c^2y_t^2)))
wind_i_1<-lag(wind_data, k = (-1):nrow(wind_data))

# Create var yt
yt<-(wind_data-wind_i_1)
yt<-na.omit(yt)

RE<-(1/nrow(yt))*((sum(1-(((c^2)*(yt^2))/(1+(c^2)*(yt^2)))))*100)
RE

# 3. Calculate the in-sample relative efficiency of the turbine using the
# company’s default filter parameters alpha and beta

r_t_1<-((alpha*(wind_data-wind_i_1))+((beta*wind_i_1)))
r_t_1<-lag(r_t_1, k=1L:nrow(r_t_1))
yt2<- (wind_data - r_t_1)
yt2<-na.omit(yt2)

RE_2<-(1/nrow(yt2))*((sum(1-(((c^2)*(yt2^2))/(1+(c^2)*(yt2^2)))))*100)
RE_2


# 4. Use the available data to fine-tune the filtering parameters theta = (alpha, beta)
# that determine the position of the rotor and optimize the expected relative
# efficiency of the wind turbine. 
# That is equal to = hat(theta_T) = argmax_theta 1/T \sum_{t=1}^T 100 * (1 -((c^2y_t^2)/(1+c^2y_t^2)))

rotorpos <- 0
for(i in 1:3599) {
  rotorpos[i] <- c(rotorpos, alpha*(wind_data-wind_i_1)+beta*(wind_i_1))
  print(rotorpos[i])
}

RE_2<-function(rotorpos){
  c2<-0.1^2
  y2<-(wind_data-wind_i_1)^2
  return((1/nrow(3600)*(sum(100*(1-(c2-y2/(1+c2*y2)))))))
}

RE_2(rotorpos)