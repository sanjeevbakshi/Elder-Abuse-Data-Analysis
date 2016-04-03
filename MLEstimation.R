library(maxLik)

#dlf1 <- na.omit(data.frame(dpanu$tp60,dpanu$VA))

#for(i in 1: dim(dlf1)[1]){
#  if(dlf1[i,1] == 0) dlf1[i,1] <- 0.5
#}
#t <- dlf1[,1] ;# 0.005 added to get rid of problems of zero time , loglikf1 not defined there
# table(t);
#x <- dlf1[,2]

loglikf1  <- function(lambda) {x*log(1 - exp(-lambda*t)) - (1-x)*lambda*t}

#estimate1 <- maxLik(loglikf1 , start= 0.01)
#summary(estimate1)
# be careful about start value else some of the iterations may produce NAN

#coef(estimate1)
#stdEr(estimate1, eigentol=1e-12)
