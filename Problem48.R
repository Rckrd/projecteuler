#Problem 48
#The series, 1^1 + 2^2 3^3 + ... + 1010 = 10405071317.

#Find the last ten digits of the series, 11 + 22 + 33 + ... + 1000^1000.


#brute with gmp
library(gmp)

nums <- list()
nums[[1]] <- as.bigz(1)
nums[[2]] <- 2*nums[[1]]
res <- as.bigz(0)
for (i in 1:999)
  res <- res + as.bigz(i)^i
res %% 10^10



#smart force, keeping only 10 digits
fexp <- function(n){
  res <- 1
  for (i in 1:n)
    res <- (res %% 10^10) * n
  res
}

res <- as.bigz(0)
for (j in 1:999)
  res <- res + fexp(j) 
res %% 10^10


