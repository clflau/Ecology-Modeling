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
curve(1*N, 0, 120, xname = "N", xlab = "N(t)", ylab = "N(t+1)", col = "red")
x <- c(.25, .5, 2, 5, 10)
for(R in x){
  curve((R*N)/(1+((R-1)/K)*N), 0, 120, xname = "N", add = TRUE)
}
text(c(10, 20, 40, 70, 60), c(70, 50, 50, 50, 20), labels = c("R=10", "R=5", "R=2", "R=0.5", "R=0.25"))

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
  out.matrix <- matrix(NA, nrow = length(RR), ncol = ttmax +1)
  
  #use a loop to iterate the model the desired number of times
  for(ii in 1:length(RR)){
    for(tt in 1:ttmax){
      NN[tt+1] <- (RR[ii]*NN[tt])/(1+((RR[ii]-1)/KK)*NN[tt])
    }
    out.matrix[ii, ] <- NN
  }
  return(out.matrix)
}

N0 <- 50
RR <- c(.25, .75, 1.1, 2, 10)
KK <- 100

output <- BevHoltFunc(N0, RR, KK)

plot(1:11, output[1, ], type = "l", main = "Beverton-Holt simulation at different R values; N0=50, K=100", xlab="time", ylab="N", ylim = c(0, 100))
for(ii in 1:length(RR)){
  lines(1:11, output[ii, ])
}
text(c(1.5, 8, 10, 3, 1.2), c(10, 25, 60, 70, 85), labels = c("R=0.25", "R=0.75", "R=1.1", "R=2", "R=10"))


out.matrix
NN
output <- BevHoltFunc(N0, RR, KK)
output



#3c - Ricker model
K <- 100
curve(1*N, 0, 150, xname = "N", xlab = "N(t)", ylab = "N(t+1)", col = "red")
x <- c(-4, -1, .75, 1.5, 2, 4)
for(R in x){
  curve(N*exp(R*(1-N/K)), 0, 150, xname = "N", add = TRUE)
}
text(c(10, 20, 30, 40, 65, 85), c(140, 100, 80, 55, 30, 30), labels = c("R=4", "R=2", "R=1.5", "R=0.75", "R=-1", "R=-4"))

#3d - Ricker model

RickerFunc <- function(N0, RR, KK, ttmax=100){
  #initialize variable to a vector of NA values
  NN <- matrix(NA, nrow = 1, ncol = ttmax+1)
  NN[1] <- N0
  out.matrix <- matrix(NA, nrow = length(RR), ncol = ttmax +1)
  
  #use a loop to iterate the model the desired number of times
  for(ii in 1:length(RR)){
    for(tt in 1:ttmax){
      NN[tt+1] <- NN[tt]*exp(RR[ii]*(1-NN[tt]/KK))
      }
    out.matrix[ii, ] <- NN
  }
  return(out.matrix)
}

N0 <- 10
RR <- c(-4, -.5, .75, 1.5, 2, 3)
KK <- 100

output <- RickerFunc(N0, RR, KK)

plot(1:101, output[1, ], type = "l", main = "Ricker simulation at different R values; N0=10, K=100", xlab="time", ylab="N", ylim = c(0, 250))
for(ii in 1:length(RR)){
  lines(1:101, output[ii, ])
}
text(c(95), c(220), labels = c("R=3"))

#R=3 chaotic? test sensitivity to N0
N0 <- 10
RR <- 3
KK <- 100

output <- RickerFunc(N0, RR, KK)

plot(1:101, output[1, ], type = "l", main = "Ricker simulation N0 sensitivity test; N0=10 and 10.1, K=100", xlab="time", ylab="N", ylim = c(0, 250))

N0 <- 10.1
RR <- 3
KK <- 100

output <- RickerFunc(N0, RR, KK)

lines(1:101, output[1, ],lty = 5)

#compare sensitivity to stable regime

N0 <- 10
RR <- 1.5
KK <- 100

output <- RickerFunc(N0, RR, KK)

plot(1:101, output[1, ], type = "l", main = "Ricker simulation N0 sensitivity test; N0=10 and 10.1, K=100", xlab="time", ylab="N", ylim = c(0, 120))

N0 <- 10.1
RR <- 1.5
KK <- 100

output <- RickerFunc(N0, RR, KK)

lines(1:101, output[1, ],lty = 5)
text(c(20), c(90), labels = c("R=1.5"))
