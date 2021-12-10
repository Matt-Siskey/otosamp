###############################################
### Bootstrap Estimator
### Seeds
### Matt Siskey
### August 2021
###############################################

Seed <- function(n){
  seed <- round(runif(n,1,10000000),0)
  return(seed)
}

i=10000
seeds <- matrix(0,i,2)

seeds[,1]  <- Seed(i)
seeds[,2]  <- Seed(i)


#seeds[1,2] = 679877 #random number
colnames(seeds)=c("bootest.seed1","bootest.seed2")
seed.list=list(seeds)
save(seed.list,file="C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp/seed.list")
