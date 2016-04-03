library(boot)

ical <- function(X,i){
  X <- X[i,]
#  r <- sum(X[,4] * X[,2])/sum(X[,5] * X[,2])
  q <- sum(X[,4] * X[,2])/sum(X[,4] * X[,5] * X[,2])
  ga <- sum(X[,4] * X[,5] * X[,2])/sum(X[,5] * X[,2])
# g <- 1 - ga
  iout <- c(q,ga)
}

bsoutput <- data.frame(0,0,0,0,0,0,0,0)
names(bsoutput) <- c("state", "abuse_type","q", "95% CI", "95% CI", "gamma", "95% CI", "95% CI")
dbs <- dpanu[,c(1,72,75:81)]

# names(dpanu)
bsk <- 0

bsstates <- levels(dbs$vstate)
abuses <- c("PA", "VA", "EA", "SD", "NG")

for(i in 1: length(bsstates)){
  
  bsd <- subset(dbs, dbs$vstate == bsstates[i])
  for(j in 1: length(abuses)){
    bsk <- bsk + 1
    bsi <- bsd[,c(1:3,3+j,9)]
    bsi <- na.omit(bsi)
    bsr <- boot(bsi, ical, R = 500, stype = "i")
    
# writing the result
    bsoutput[bsk, 1] <- bsstates[i]
    bsoutput[bsk, 2] <- abuses[j]
    
    bias1 <- mean(bsr$t[,1]-bsr$t0[1])
    bsoutput[bsk, 3] <- round(bsr$t0[1] - bias1, 2) # corrected for bias
    r1 <- boot.ci(bsr, conf = 0.95, type = "basic", index = 1)
    bsoutput[bsk, 4] <- round(r1$basic[4]- bias1, 2)
    bsoutput[bsk, 5] <- round(r1$basic[5]- bias1, 2)
    
    bias2 <- mean(bsr$t[,2]-bsr$t0[2])
    bsoutput[bsk, 6] <- round(bsr$t0[2] - bias2, 2) # corrected for bias
    r2 <- boot.ci(bsr, conf = 0.95, type = "basic", index = 2)
    bsoutput[bsk, 7] <- round(r2$basic[4]- bias2, 2)
    bsoutput[bsk, 8] <- round(r2$basic[5]- bias2, 2)
    
    
  }
  
}

