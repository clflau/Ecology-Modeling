
library(deSolve)

#Simple Pred-Prey Lotka-Volterra
LV_func <- function(tt, init, params){
  R <- init[1]
  C <- init[2]
  with(as.list(params),{
    dR.dt <- r*R-a*R*C
    dC.dt <- a*R*C-d*C
    return(list(c(dR.dt, dC.dt)))
  })
}

init <- c(R=1000, C=10)
tseq <- seq(0, 2000, by=10)
params <- c(r=0.1, a=0.0003, d=0.08)

LV.out <- lsoda(init, tseq, LV_func, params)
head(LV.out); tail(LV.out)
plot(LV.out[,2], LV.out[,3], type = "l", xlab = "R", ylab = "C")
plot(LV.out[,1], LV.out[,2], type = "l", col = "blue", xlab = "time")
lines(LV.out[,1], LV.out[,3], col = "red", xlab = "time")

#######################

#Pred-Prey Lotka-Volterra with prey self-limitation
LV_rsl_func <- function(tt, init, params){
  R <- init[1]
  C <- init[2]
  with(as.list(params),{
    dR.dt <- r*R*(1-alph*R)-a*R*C
    dC.dt <- e*a*R*C-d*C
    return(list(c(dR.dt, dC.dt)))
  })
}

init <- c(R=100, C=100)
tseq <- seq(0, 2000, by=10)
params <- c(r=0.2, alph=0.0002, e=0.01, a=0.001, d=0.002) #play around with parameters to get spirals and limit cycles

LV.out <- lsoda(init, tseq, LV_rsl_func, params)
head(LV.out); tail(LV.out)
plot(LV.out[,2], LV.out[,3], type = "l", xlab = "R", ylab = "C", main = "Pred-Prey with Prey self-limit", sub = paste("r=", params[1], "; alpha=", params[2], "; e=", params[3], "; a=", params[4], "; d=", params[5], sep = " "))
plot(LV.out[,1], LV.out[,2], type = "l", col = "blue", xlab = "time", ylab = "pop size")
lines(LV.out[,1], LV.out[,3], col = "red", xlab = "time")
legend("topright", c("Resource", "Consumer"), col = c("blue", "red"), lty = 1)

#internal equilibrium
R_star <- with(as.list(params), d/(e*a)); R_star
C_star <- with(as.list(params), (e*a*r-d*r*alph)/(e*a^2)); C_star
with(as.list(params), e*a/d > alph) # stable equilibrium if TRUE



######################

#Pred-Prey Lotka-Volterra with Type II functional response
LV_T2_func <- function(tt, init, params){
  R <- init[1]
  C <- init[2]
  with(as.list(params),{
    dR.dt <- r*R-(a*R*C/(1+a*h*R))
    dC.dt <- (e*a*R*C/(1+a*h*R))-d*C
    return(list(c(dR.dt, dC.dt)))
  })
}

init <- c(R=10, C=10)
tseq <- seq(0, 10)
params <- c(r=20, h=0.001, e=0.005, a=0.01, d=0.01) 

#internal equilibrium
R_star <- with(as.list(params), d/(e*a-a*h*d)); R_star
C_star <- with(as.list(params), (r*h*d/(a*e-a*h*d))+1); C_star
with(as.list(params), e > h*d) # stable equilibrium if TRUE

LV.out <- lsoda(init, tseq, LV_T2_func, params)
head(LV.out); tail(LV.out)
plot(LV.out[,2], LV.out[,3], type = "l", xlab = "R", ylab = "C", main = "Pred-Prey with Pred TypeII Functional Response", sub = paste("r=", params[1], "; h=", params[2], "; e=", params[3], "; a=", params[4], "; d=", params[5], sep = "  "))
abline(v = R_star, h = C_star, col = 2)
plot(LV.out[,1], LV.out[,2], type = "l", col = "blue", xlab = "time", ylab = "pop size")
lines(LV.out[,1], LV.out[,3], col = "red", xlab = "time")
legend("topright", c("Resource", "Consumer"), col = c("blue", "red"), lty = 1)


