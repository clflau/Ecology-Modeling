##Ecology problem set 2


#Eigenvalues and Eigenvectors

wombat_LM <- rbind(c(0.6, 1.5), c(0.3, 0)) #Leslie matrix
wombat_eigen <- eigen(wombat_LM)
wombat_eigen
#dominant eigen value
dom_ei_val <- wombat_eigen$values[1]
dom_ei_val
#stable age distribution
stable_age <- wombat_eigen$vectors[,1]/sum(wombat_eigen$vectors[,1]) #normalized
stable_age


###
n_2015 <- matrix(c(2000, 1000), nrow = 2, byrow = TRUE)
n_2016 <- wombat_LM %*% n_2015
n_2016


###
#proportion of pop growth 2067-2068
AA <- wombat_LM
tt <- 2067-2015
for (ii in 1:tt){
  AA <- wombat_LM %*% AA
}
n_2067 <- AA %*% n_2015
n_2067

tt <- 2068-2015
for (ii in 1:tt){
  AA <- wombat_LM %*% AA
}
n_2068 <- AA %*% n_2015
n_2068
sum(n_2068)/sum(n_2067)

#proportion of pop growth 2068-2069
tt <- 2069-2015
for (ii in 1:tt){
  AA <- wombat_LM %*% AA
}
n_2069 <- AA %*% n_2015
sum(n_2069)/sum(n_2068)



###
nn_0 <- n_2015
pop <- nn_0
nn <- nn_0
for (ii in 1:50){
  pop <- wombat_LM%*%pop
  nn <- cbind(nn, pop)
}
nn
par(mfrow = c(1, 2))
plot(0:50, nn[1, ], type = "l", lty = 1 , ylim = c(1000, 13000), yaxt = "n", xlab = "Time", ylab = "Absolute Population size", main = "50 time step")
lines(0:50, nn[2, ], lty = 2)
axis(side = 2, at = seq(1000, 13000, by=2000))
legend("topleft", lty = c(1, 2) ,legend = c("age 0","age 1"), cex = 0.75)

plot(0:50, nn[1, ]/(nn[1, ]+nn[2, ]), type = "l", yaxt = "n", ylim = c(0, 1), lty = 1, xlab = "Time", ylab = "Proportion of age class")
axis(side = 2, at = seq(0, 1, by=0.1))
lines(0:50, nn[2, ]/(nn[1, ]+nn[2, ]), lty = 2)
legend("topleft", lty = c(1, 2),legend = c("age 0","age 1"), cex = 0.75)


###
#absolute increase of 0.01 in b0
wombat_LM1 <- rbind(c(0.61, 1.5), c(0.3, 0))
wombat_eigen1 <- eigen(wombat_LM1)
wombat_eigen1$values[1]
#absolute increase of 0.01 in s0
wombat_LM2 <- rbind(c(0.61, 1.5), c(0.31, 0))
wombat_eigen2 <- eigen(wombat_LM2)
wombat_eigen2$values[1]
#Does 0.01 increase in b0 have greater impact on lamba than 0.01 increase in s0?
wombat_eigen1$values[1] - wombat_eigen$values[1] > wombat_eigen2$values[1] - wombat_eigen$values[1]

# 1% increase in b0
wombat_LM3 <- rbind(c(0.6*1.01, 1.5), c(0.3, 0))
wombat_eigen3 <- eigen(wombat_LM3)
wombat_eigen3$values[1]
# 1% increase in s0
wombat_LM4 <- rbind(c(0.6, 1.5), c(0.3*1.01, 0))
wombat_eigen4 <- eigen(wombat_LM4)
wombat_eigen4$values[1]
#Does 1% increase in b0 have greater impact on lamba than 1% increase in s0?
wombat_eigen3$values[1] - wombat_eigen$values[1] > wombat_eigen4$values[1] - wombat_eigen$values[1]





###testbed###
no <- matrix(c(4,1),nrow=2)
N <- NULL
N <- cbind(N,no)
pop <- no
A <- matrix(c(1.5, 2, 0.5, 0), nrow = 2, byrow = TRUE)
for (i in 1:10){
  pop <- A%*%pop
  N <- cbind(N,pop)
}
plot(0:10,log(apply(N,2,sum)),type="l", col="blue", xlab="Time",
        ylab="Population Size")
N[1,]/N[2,]




