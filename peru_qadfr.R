rm(list=ls())

library(quantreg)
library(openxlsx)
library(tseries)
library(dplyr)
library(seasonal)
library(mFilter)
library(tseries)
library(e1071)
library(openxlsx)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

#data <- read.xlsx()

##*************************
## Peru 1
##*************************


zz <- data$p1

n = length(zz)

yt = zz[7:n]

yt_lag = zz[6:(n-1)]

Dyt_L1 = zz[6:(n-1)] - zz[5:(n-2)]
Dyt_L2 = zz[5:(n-2)] - zz[4:(n-3)]
Dyt_L3 = zz[4:(n-3)] - zz[3:(n-4)]
Dyt_L4 = zz[3:(n-4)] - zz[2:(n-5)]
Dyt_L5 = zz[2:(n-5)] - zz[1:(n-6)]



fit1 <- rq( yt ~ yt_lag + Dyt_L1 + Dyt_L2 + Dyt_L3 + Dyt_L4 + Dyt_L5, tau=c(.1, .2, .3, .4, .5,.6,.7,.8,.9) )

w = summary(fit1, se="boot",  bsmethod= "xy", R=500)

df <- data.frame(
  alpha0 = numeric(),    
  alpha0_t = numeric(), 
  alpha0_pvalue = numeric(),    
  alpha1 = numeric(),   
  alpha1_t = numeric(),  
  alpha1_pvalue = numeric()   
)



for (i in 1:9) {
  
  
  alpha0 = w[[i]][["coefficients"]][1,1]
  alpha0_t = w[[i]][["coefficients"]][1,3]
  alpha0_pvalue = w[[i]][["coefficients"]][1,4]
  
  alpha1 = w[[i]][["coefficients"]][2,1]
  alpha1_t = w[[i]][["coefficients"]][2,3]
  alpha1_pvalue = w[[i]][["coefficients"]][2,4]
  
  
  df[i,1] = alpha0 
  df[i,2] = alpha0_t 
  df[i,3] = alpha0_pvalue 
  df[i,4] = alpha1 
  df[i,5] = alpha1_t 
  df[i,6] = alpha1_pvalue  
  
}


df1 = df



betas_hat <- data.frame(
  alpha0 = numeric(),    
  alpha0_lb = numeric(), 
  alpha0_ub = numeric(),
  alpha1 = numeric(),    
  alpha1_lb = numeric(), 
  alpha1_ub = numeric())

gg = plot(w)

for (i in 1:9) {
  


  zz = gg[[1]]
  
  z1 = zz[,,i][1,1]
  z2 = zz[,,i][1,2]
  z3 = zz[,,i][1,3]
  
  
  z4 = zz[,,i][2,1]
  z5 = zz[,,i][2,2]
  z6 = zz[,,i][2,3]
  
  betas_hat[i,1] = z1
  betas_hat[i,2] = z2
  betas_hat[i,3] = z3
  betas_hat[i,4] = z4
  betas_hat[i,5] = z5
  betas_hat[i,6] = z6
  

}

beta1 = betas_hat



##*************************
## Peru 2
##*************************


zz <- data$p2

n = length(zz)

yt = zz[7:n]

yt_lag = zz[6:(n-1)]

Dyt_L1 = zz[6:(n-1)] - zz[5:(n-2)]
Dyt_L2 = zz[5:(n-2)] - zz[4:(n-3)]
Dyt_L3 = zz[4:(n-3)] - zz[3:(n-4)]
Dyt_L4 = zz[3:(n-4)] - zz[2:(n-5)]
Dyt_L5 = zz[2:(n-5)] - zz[1:(n-6)]


fit1 <- rq( yt ~ yt_lag + Dyt_L1 + Dyt_L2 + Dyt_L3 + Dyt_L4 + Dyt_L5, tau=c(.1, .2, .3, .4, .5,.6,.7,.8,.9) )

w = summary(fit1, se="boot",  bsmethod= "xy", R=500)

y <- yt
X <- model.matrix(~ yt_lag + Dyt_L1 + Dyt_L2 + Dyt_L3 + Dyt_L4 + Dyt_L5)

set.seed(123)  # Para reproducibilidad
boot_results <- boot.rq(y = y, x = X, tau = 0.9, R = 500, bsmethod = "xy")



alpha0 = boot_results[["B"]][,2]
sd(alpha0)



df <- data.frame(
  alpha0 = numeric(),    
  alpha0_t = numeric(), 
  alpha0_pvalue = numeric(),    
  alpha1 = numeric(),   
  alpha1_t = numeric(),  
  alpha1_pvalue = numeric()   
)



for (i in 1:9) {
  
  
  alpha0 = w[[i]][["coefficients"]][1,1]
  alpha0_t = w[[i]][["coefficients"]][1,3]
  alpha0_pvalue = w[[i]][["coefficients"]][1,4]
  
  alpha1 = w[[i]][["coefficients"]][2,1]
  alpha1_t = w[[i]][["coefficients"]][2,3]
  alpha1_pvalue = w[[i]][["coefficients"]][2,4]
  
  
  df[i,1] = alpha0 
  df[i,2] = alpha0_t 
  df[i,3] = alpha0_pvalue 
  df[i,4] = alpha1 
  df[i,5] = alpha1_t 
  df[i,6] = alpha1_pvalue  
  
}


df2 = df


betas_hat <- data.frame(
  alpha0 = numeric(),    
  alpha0_lb = numeric(), 
  alpha0_ub = numeric(),
  alpha1 = numeric(),    
  alpha1_lb = numeric(), 
  alpha1_ub = numeric())

gg = plot(w)

for (i in 1:9) {
  
  
  
  zz = gg[[1]]
  
  z1 = zz[,,i][1,1]
  z2 = zz[,,i][1,2]
  z3 = zz[,,i][1,3]
  
  
  z4 = zz[,,i][2,1]
  z5 = zz[,,i][2,2]
  z6 = zz[,,i][2,3]
  
  betas_hat[i,1] = z1
  betas_hat[i,2] = z2
  betas_hat[i,3] = z3
  betas_hat[i,4] = z4
  betas_hat[i,5] = z5
  betas_hat[i,6] = z6
  
  
}

beta2 = betas_hat


##*************************
## Peru 3
##*************************


zz <- data$p3

n = length(zz)

yt = zz[7:n]

yt_lag = zz[6:(n-1)]

Dyt_L1 = zz[6:(n-1)] - zz[5:(n-2)]
Dyt_L2 = zz[5:(n-2)] - zz[4:(n-3)]
Dyt_L3 = zz[4:(n-3)] - zz[3:(n-4)]
Dyt_L4 = zz[3:(n-4)] - zz[2:(n-5)]
Dyt_L5 = zz[2:(n-5)] - zz[1:(n-6)]



fit1 <- rq( yt ~ yt_lag + Dyt_L1 + Dyt_L2 + Dyt_L3 + Dyt_L4 + Dyt_L5, tau=c(.1, .2, .3, .4, .5,.6,.7,.8,.9) )

w = summary(fit1, se="boot",  bsmethod= "xy", R=500)

df <- data.frame(
  alpha0 = numeric(),    
  alpha0_t = numeric(), 
  alpha0_pvalue = numeric(),    
  alpha1 = numeric(),   
  alpha1_t = numeric(),  
  alpha1_pvalue = numeric()   
)



for (i in 1:9) {
  
  
  alpha0 = w[[i]][["coefficients"]][1,1]
  alpha0_t = w[[i]][["coefficients"]][1,3]
  alpha0_pvalue = w[[i]][["coefficients"]][1,4]
  
  alpha1 = w[[i]][["coefficients"]][2,1]
  alpha1_t = w[[i]][["coefficients"]][2,3]
  alpha1_pvalue = w[[i]][["coefficients"]][2,4]
  
  
  df[i,1] = alpha0 
  df[i,2] = alpha0_t 
  df[i,3] = alpha0_pvalue 
  df[i,4] = alpha1 
  df[i,5] = alpha1_t 
  df[i,6] = alpha1_pvalue  
  
}


df3 = df


betas_hat <- data.frame(
  alpha0 = numeric(),    
  alpha0_lb = numeric(), 
  alpha0_ub = numeric(),
  alpha1 = numeric(),    
  alpha1_lb = numeric(), 
  alpha1_ub = numeric())

gg = plot(w)

for (i in 1:9) {
  
  
  
  zz = gg[[1]]
  
  z1 = zz[,,i][1,1]
  z2 = zz[,,i][1,2]
  z3 = zz[,,i][1,3]
  
  
  z4 = zz[,,i][2,1]
  z5 = zz[,,i][2,2]
  z6 = zz[,,i][2,3]
  
  betas_hat[i,1] = z1
  betas_hat[i,2] = z2
  betas_hat[i,3] = z3
  betas_hat[i,4] = z4
  betas_hat[i,5] = z5
  betas_hat[i,6] = z6
  
  
}

beta3 = betas_hat


##*************************
## Peru 4
##*************************


zz <- data$p4

n = length(zz)

yt = zz[6:n]

yt_lag = zz[5:(n-1)]

Dyt_L1 = zz[5:(n-1)] - zz[4:(n-2)]
Dyt_L2 = zz[4:(n-2)] - zz[3:(n-3)]
Dyt_L3 = zz[3:(n-3)] - zz[2:(n-4)]
Dyt_L4 = zz[2:(n-4)] - zz[1:(n-5)]



fit1 <- rq( yt ~ yt_lag + Dyt_L1 + Dyt_L2 + Dyt_L3 + Dyt_L4, tau=c(.1, .2, .3, .4, .5,.6,.7,.8,.9) )

w = summary(fit1, se="boot",  bsmethod= "xy", R=500)

df <- data.frame(
  alpha0 = numeric(),    
  alpha0_t = numeric(), 
  alpha0_pvalue = numeric(),    
  alpha1 = numeric(),   
  alpha1_t = numeric(),  
  alpha1_pvalue = numeric()   
)



for (i in 1:9) {
  
  
  alpha0 = w[[i]][["coefficients"]][1,1]
  alpha0_t = w[[i]][["coefficients"]][1,3]
  alpha0_pvalue = w[[i]][["coefficients"]][1,4]
  
  alpha1 = w[[i]][["coefficients"]][2,1]
  alpha1_t = w[[i]][["coefficients"]][2,3]
  alpha1_pvalue = w[[i]][["coefficients"]][2,4]
  
  
  df[i,1] = alpha0 
  df[i,2] = alpha0_t 
  df[i,3] = alpha0_pvalue 
  df[i,4] = alpha1 
  df[i,5] = alpha1_t 
  df[i,6] = alpha1_pvalue  
  
}


df4 = df


betas_hat <- data.frame(
  alpha0 = numeric(),    
  alpha0_lb = numeric(), 
  alpha0_ub = numeric(),
  alpha1 = numeric(),    
  alpha1_lb = numeric(), 
  alpha1_ub = numeric())

gg = plot(w)

for (i in 1:9) {
  
  
  
  zz = gg[[1]]
  
  z1 = zz[,,i][1,1]
  z2 = zz[,,i][1,2]
  z3 = zz[,,i][1,3]
  
  
  z4 = zz[,,i][2,1]
  z5 = zz[,,i][2,2]
  z6 = zz[,,i][2,3]
  
  betas_hat[i,1] = z1
  betas_hat[i,2] = z2
  betas_hat[i,3] = z3
  betas_hat[i,4] = z4
  betas_hat[i,5] = z5
  betas_hat[i,6] = z6
  
  
}

beta4 = betas_hat



##*************************
## Peru 5
##*************************


zz <- data$p5

n = length(zz)

yt = zz[4:n]

yt_lag = zz[3:(n-1)]


Dyt_L1 = zz[3:(n-1)] - zz[2:(n-2)]
Dyt_L2 = zz[2:(n-2)] - zz[1:(n-3)]



fit1 <- rq( yt ~ yt_lag + Dyt_L1 + Dyt_L2 , tau=c(.1, .2, .3, .4, .5,.6,.7,.8,.9) )

w = summary(fit1, se="boot",  bsmethod= "xy", R=500)

df <- data.frame(
  alpha0 = numeric(),    
  alpha0_t = numeric(), 
  alpha0_pvalue = numeric(),    
  alpha1 = numeric(),   
  alpha1_t = numeric(),  
  alpha1_pvalue = numeric()   
)




for (i in 1:9) {
  
  
  alpha0 = w[[i]][["coefficients"]][1,1]
  alpha0_t = w[[i]][["coefficients"]][1,3]
  alpha0_pvalue = w[[i]][["coefficients"]][1,4]
  
  alpha1 = w[[i]][["coefficients"]][2,1]
  alpha1_t = w[[i]][["coefficients"]][2,3]
  alpha1_pvalue = w[[i]][["coefficients"]][2,4]
  
  
  df[i,1] = alpha0 
  df[i,2] = alpha0_t 
  df[i,3] = alpha0_pvalue 
  df[i,4] = alpha1 
  df[i,5] = alpha1_t 
  df[i,6] = alpha1_pvalue  
  
}


df5 = df



betas_hat <- data.frame(
  alpha0 = numeric(),    
  alpha0_lb = numeric(), 
  alpha0_ub = numeric(),
  alpha1 = numeric(),    
  alpha1_lb = numeric(), 
  alpha1_ub = numeric())

gg = plot(w)

for (i in 1:9) {
  
  
  
  zz = gg[[1]]
  
  z1 = zz[,,i][1,1]
  z2 = zz[,,i][1,2]
  z3 = zz[,,i][1,3]
  
  
  z4 = zz[,,i][2,1]
  z5 = zz[,,i][2,2]
  z6 = zz[,,i][2,3]
  
  betas_hat[i,1] = z1
  betas_hat[i,2] = z2
  betas_hat[i,3] = z3
  betas_hat[i,4] = z4
  betas_hat[i,5] = z5
  betas_hat[i,6] = z6
  
  
}

beta5 = betas_hat


