#1(a)

rm(list=ls()) 
library(mvtnorm)
set.seed(100)

n=1000
meanVector = c(11,8,12,19,10)
coVarianceMatrix = matrix(
                          c(6,4,3,2,1,
                            4,5,4,3,2,
                            3,4,7,4,3,
                            2,3,4,9,4,
                            1,2,3,4,4),
                          ncol=5)

x = rmvnorm(n, meanVector, coVarianceMatrix)
head(x)

#1(b)

colnames(x) = c("X1","X2","X3","X4","X5")

head(x)
boxplot(x)




#2(a)

library(mvtnorm) 

n <- c(500,1500,6000)
meanVector = c(4,5)
coVarianceMatrix = matrix(
                          c(8,-3,
                            -3,5),
                          ncol=2)

set.seed(1000)
sample = list(rmvnorm(n[1],mu,covmat),
              rmvnorm(n[2],mu,covmat),
              rmvnorm(n[3],mu,covmat))

for(j in 1:length(n)){
  mleXbar = apply(sample[[j]], 2, mean)
  cat("For sample-",j, "Mean Vector\n", mleXbar, "\n")
  
  # Covariance matrix
  mleCoVarianceMatrix1 = var(sample[[j]]) 
  
  x = sample[[j]]
  mleCoVarianceMatrix = 0
  for(i in 1:n[j]){
    xihTerm = (x[i,] - mleXbar) %*% t(x[i,] - mleXbar)  
    mleCoVarianceMatrix  = mleCoVarianceMatrix + xihTerm    
  }
  mleCoVarianceMatrix = mleCoVarianceMatrix / n[j]
  cat("Mean Vector \n")
  print(mleCoVarianceMatrix) 
  #print(mleCoVarianceMatrix1)
  cat("----------------------------\n")
}




#3

T2 = function(x, mu0){
  n = nrow(x); p = ncol(x)
  meanVector = apply(x, 2, mean)
  sum = 0
  for(i in 1:n){
    ith = (x[i,] - meanVector) %*% t(x[i,] - meanVector)
    sum = sum + ith
  }
  result = n * (t(meanVector - mu0) %*% solve(sum/p) %*% (meanVector - mu0))
  return(result)
}

# Example of book
y1 = c(6, 10, 8)
y2 = c(9, 6, 3)
mu0  = c(9, 5)
x = cbind(y1, y2)

T2(x, mu0)


y1 <-c(580,473,664,739,143,127,703,108,185,111,815,770,759,928,849)
y2 <- c(516,319,369,193,853,632,551,578,074,544,365,522,205,360,137)
y3 <- c(613,514,782,293,927,512,936,856,244,618,500,542,443,402,396)
y4 <- c(750,963,107,530,121,837,118,113,663,816,930,570,789,611,700)
y5 <- c(185,183,211,189,216,195,215,223,163,190,208,170,197,156,190)
mu0 = c(108, 500, 600, 700, 180)
x = cbind(y1, y2, y3, y4, y5)

T2(x, mu0)









