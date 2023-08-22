set.seed(1000)

n = 1000
meanVector = c(11,8,12,19,10)
coVarianceMatrix = matrix(
  c(6,4,3,2,1,
    4,5,4,3,2,
    3,4,7,4,3,
    2,3,4,9,4,
    1,2,3,4,4),
  ncol=5)
# Random Multivariate Normal with size = n
x = rmvnorm(n = n, mean = meanVector, sigma = coVarianceMatrix)
# Means of column wise values of x matrix
mleXbar = apply(x, 2, mean)  
# MLE of mean
mleXbar                                                         

mleCoVarianceMatrix = 0
for(i in 1:n){
  # (xi - miu(hat))* transpose(xi - miu(hat))
  xihTerm = (x[i,] - mleXbar) %*% t(x[i,] - mleXbar)  
  # making sum of xi th terms
  mleCoVarianceMatrix  = mleCoVarianceMatrix + xihTerm    
}
# Covariance matrix = sum(xi th terms) / n
mleCoVarianceMatrix = mleCoVarianceMatrix / n
# MLE of covariance matrix
mleCoVarianceMatrix                                       
