##Ecology problem set 1

#Logistic Density-dependence model

#Exercise 1c

A <- 40
K <- 100
r <- -100

curve(r*N*(1-(N/K))*((N/A)-1), 0, 100, xname = "N")
abline(h = 0, v = 0)



#Exercise 3c - Beverton-Holt model

K <- 100
curve(1*N, 0, 120, xname = "N", col = "red")
x <- c(.25, .5, 2, 5, 10)
for(R in x){
  curve((R*N)/(1+((R-1)/K)*N), 0, 120, xname = "N", add = TRUE)
}

# Discrete logistic model
curve(1*N, 0, 200, xname = "N", col = "red")
x <- c(-2, -.5, .25, .5, 2, 5, 10)
for(R in x){
  curve(N*(1+R*(1-(N/K))), 0, 200, xname = "N", add = TRUE)
}

#Exercise 3d - Beverton-Holt model

BevHoltFunc <- function(N0, RR, KK, ttmax=10){
  #initialize variable to a vector of NA values
  NN <- matrix(NA, nrow = 1, ncol = ttmax+1)
  NN[1] <- N0
  
  #use a loop to iterate the model the desired number of times
  for(tt in 1:ttmax){
    NN[tt+1] <- (RR*NN[tt])/(1+((RR-1)/KK)*NN[tt])
  }
  print(NN)
  plot(1:(ttmax+1),NN, type = "l", xlab="time", ylab="N", col="blue")
}

N0 <- 20
RR <- 2
#RR <- c(.25, .5, 2, 5, 10)
KK <- 100

#par(mfrow=c(length(RR),1))
#for(ii in 1:length(RR)){
  BevHoltFunc(N0, RR, KK)

#}
  
par(mfrow=c(1,1))



#test
plot(qnorm) # default range c(0, 1) is appropriate here,
# but end values are -/+Inf and so are omitted.
plot(qlogis, main = "The Inverse Logit : qlogis()")
abline(h = 0, v = 0:2/2, lty = 3, col = "gray")

curve(sin, -2*pi, 2*pi, xname = "t")
curve(tan, xname = "t", add = NA,
      main = "curve(tan)  --> same x-scale as previous plot")

op <- par(mfrow = c(2, 2))
curve(x^3 - 3*x, -2, 2)
curve(x^2 - 2, add = TRUE, col = "violet")

## simple and advanced versions, quite similar:
plot(cos, -pi,  3*pi)
curve(cos, xlim = c(-pi, 3*pi), n = 1001, col = "blue", add = TRUE)

chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n = 2001)
plot (chippy, -8, -5)

for(ll in c("", "x", "y", "xy"))
  curve(log(1+x), 1, 100, log = ll, sub = paste0("log = '", ll, "'"))
par(op)

