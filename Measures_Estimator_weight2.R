# computing the measures for each state

# weights are incarporates into maximum likelihood estimation
library(maxLik)

mlevel <- levels(dpanu$vstate)

# output for measures - output with weighted  observations in MML
woutput <- data.frame(0,0,0,0,0,0,0,0)
names(woutput) <- c("state","rate", "aspect", "R", "Q", "Gamma", "Proportion", "G")

# output for global measures
wgoutput <- data.frame(0,0,0,0,0,0)

aspects <- c("PA", "VA", "EA","SD", "N")

names(wgoutput) <- c("state","combined rate", "gR", "gQ", "gGamma", "gG")


# number of aspects of elder abuse is denited by nasp
nasp <- 5
count <- 0
for(i in 1:length(mlevel)){
  # vector for global measures for a state
  gm <- c(0,0,0)
  d2 <- subset(dpanu, dpanu$vstate == mlevel[i])
  sumrates <- 0
  for(j in 1: nasp){
    d2a <- d2[, c(75+j, 81, 72)] ;# the indicator, time post 60 and weight
    d2a <- na.omit(d2a)
    m <- frqr(d2a[,1], d2a[,2], d2a[,3]) 
    sr <- m[1]/m[2]
    sq <- m[1]/m[3]
    sit <- m[3]/m[2]
    sp <- m[1]/m[4]
    gm <- gm + m[1:3]
    
    # the mle of lambda, the rate
    t <- d2a[,2]
    # table(t);
    # replacing the zero time post 60 by 0.5
    # 0.5 added to get rid of problems of zero time , loglikf1 not defined there
    for(k in 1: dim(d2a)[1]){
      if(t[k] == 0) t[k] <- 0.5
    }
    x <- d2a[,1]
    w <- d2a[,3]
    loglikf1  <- function(lambda) {w* x*log(1 - exp(-lambda*t)) - w*(1-x)*lambda*t}
    estimate1 <- maxLik(loglikf1 , start= 0.0002)
    erate <- round(coef(estimate1),3)
    sumrates <- sumrates + erate
    
    count <- count + 1
    woutput[count,] <- c(mlevel[i], erate, aspects[j], round(sr,3), round(sq,3), round(sit,3), round(sp,2), round(100*(1-sit),2))
  }

  gsr <- gm[1]/ gm[2]
  gsq <- gm[1]/gm[3]
  gsit <- gm[3]/gm[2]
  wgoutput[i,] <- c(mlevel[i], sumrates ,round(gsr,3), round(gsq,3), round(gsit,3),round(100* (1-gsit),2))
}

