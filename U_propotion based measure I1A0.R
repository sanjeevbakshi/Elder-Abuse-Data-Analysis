library(boot)

# Ideal and Antiideal are pre-defined

# function to calculate u
ufun01 <- function(X, i, bI, bAI){
  X <- X[i,]
  
  # mcov <- as.matrix(cov(X[,3:7], use = "pairwise.complete.obs")) # estimated covariance matrix
  
  mcov <- matrix(rep(0,25), nrow <- ncol <- 5)
  for(ic in 1:5){
    for(jc in 1:5){
      bv <- na.omit(X[,c(2,2+ic,2+jc)])
      
      mcov[ic,jc] <- cov.wt(cbind(bv[,2], bv[,3]), wt = bv[,1])[[1]][1,2]
      
      # mcov[ic,jc] <- mcov[jc,ic] <- sum((bv[,1])*(bv[,2]-weighted.mean(bv[,2],bv[,1] ,na.rm = TRUE))*(bv[,3]-weighted.mean(bv[,3],bv[,1] ,na.rm = TRUE)))/(sqrt(sum((bv[,1])*(bv[,2]-weighted.mean(bv[,2],bv[,1] ,na.rm = TRUE))^2) * sum((bv[,1])*(bv[,2]-weighted.mean(bv[,2],bv[,1] ,na.rm = TRUE))^2)))
    }
  }
  
  
  
  
  
  
  mu1 <- vector(mode = "numeric", length = 5)
  muI <- bI
  muAI <- bAI
  for(j in 1:5){
    mu1[j] <- weighted.mean(X[,j+2], X[,2], na.rm = TRUE)
  }
  
  if(det(mcov) != 0)
    {
    di <- t(as.matrix(mu1 - muI)) %*% solve(mcov) %*% as.matrix(mu1 - muI)
    da <- t(as.matrix(mu1 - muAI)) %*% solve(mcov) %*% as.matrix(mu1 - muAI)
    u <- log(da/di)
  }
  else
    {
    di <- NA
    da <- NA
    u <- NA
  }
  
  
  
  uout <- c(u, di, da)
}



# the output file of results
uoutput01 <- data.frame(0,0,0,0,0,0,0,0,0,0)
names(uoutput01) <- c("state", "u", "CI", "CI", "di","CI", "CI", "da", "CI", "CI")


AI <- c(1,1,1,1,1) # The anti-ideal state
I <-c(0,0,0,0,0)   # The ideal state

# Data file for analysis
dubs <- dbs[,c(1,2,4:8)]
states <- levels(dubs$vstate)
abuses

# bootstrapping for u length(states)
for(i in 1:length(states)){
  pd <- subset(dubs, dubs$vstate == states[i])
  ubs01 <- boot(pd, ufun01, R = 1000, stype = "i", bI = I, bAI = AI)
  
  uoutput01[i,1] <- states[i]
  
  bias1 <- mean(ubs01$t[,1] - ubs01$t0[1], na.rm = TRUE)
  uoutput01[i,2] <- round(ubs01$t0[1] - bias1,4) 
  uci01 <- boot.ci(ubs01, conf = 0.95, type = "perc", index = 1)
  uoutput01[i,3] <- round(uci01$percent[4]-bias1,4) # note perc and percent
  uoutput01[i,4] <- round(uci01$percent[5]-bias1,4) # note perc and percent
  
  bias2 <- mean(ubs01$t[,2] - ubs01$t0[2], na.rm = TRUE)
  uoutput01[i,5] <- round(ubs01$t0[2]-bias2,4)
  ucii01 <- boot.ci(ubs01, conf = 0.95, type = "perc", index = 2)
  uoutput01[i,6] <- round(ucii01$percent[4]-bias2,4) # note perc and percent
  uoutput01[i,7] <- round(ucii01$percent[5]-bias2,4) # note perc and percent
  
  
  bias3 <- mean(ubs01$t[,3] - ubs01$t0[3], na.rm = TRUE)
  uoutput01[i,8] <- round(ubs01$t0[3]-bias3,4)
  ucia01 <- boot.ci(ubs01, conf = 0.95, type = "perc", index = 3)
  uoutput01[i,9] <- round(ucia01$percent[4]-bias3,4) # note perc and percent
  uoutput01[i,10] <- round(ucia01$percent[5]-bias3,4) # note perc and percent
}



uoutput101 <- data.frame(uoutput01[,1],round(uoutput01[,2:10],2))

