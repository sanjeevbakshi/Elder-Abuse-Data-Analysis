# computing the measures for each state

mlevel <- levels(dpanu$vstate)

# output for measures
output <- data.frame(0,0,0,0,0,0)
names(output) <- c("state", "aspect", "R", "Q", "G", "Proportion")

# output for global measures
goutput <- data.frame(0,0,0,0)

aspects <- c("PA", "VA", "EA","SD", "N")

names(goutput) <- c("state", "gR", "gQ", "gG")


# number of aspects of elder abuse is denited by nasp
nasp <- 5
count <- 0
for(i in 1:length(mlevel)){
  # vector for global measures for a state
  gm <- c(0,0,0)
  d2 <- subset(dpanu, dpanu$vstate == mlevel[i])
  
  for(j in 1: nasp){
    d2a <- d2[, c(75+j, 81, 72)]
    d2a <- na.omit(d2a)
    m <- frqr(d2a[,1], d2a[,2], d2a[,3])
    sr <- m[1]/m[2]
    sq <- m[1]/m[3]
    sit <- m[3]/m[2]
    sp <- m[1]/m[4]
    gm <- gm + m[1:3]
    count <- count + 1
    output[count,] <- c(mlevel[i], aspects[j], round(sr,3), round(sq,3), round(sit,3), round(sp,2))
  }

  gsr <- gm[1]/ gm[2]
  gsq <- gm[1]/gm[3]
  gsit <- gm[3]/gm[2]
  goutput[i,] <- c(mlevel[i], round(gsr,3), round(gsq,3), round(gsit,3))
}
