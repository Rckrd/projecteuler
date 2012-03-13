#Problem 16



## 16
# 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.


for (i in 1:20) {
  print(i + "tjo")
}

digsum <- function(n){
  res <- as.character(n)
  res <- strsplit(res,"")
  res <- res[[1]]
  res <- as.numeric(res)
  res <- sum(res)
  res
}
res <-  NULL
x <- 1:60
for (i in x){
  res <- append(res,digsum(2^i))
}
qplot(x,res,geom="Line")


#start with first, double, if > 10, carry digit

N      <- 1000
carry  <- rep(0,N)
res    <- rep(0,N)
res[1] <- 2^0
n      <- 1
j      <- 1

for (i in 1:N){ #outer loop, double N times
  j <- 1
  while (j <= min(n,N)) { #inner loop, over all digits in vector
    if (res[j]*2 > 9){
      carry[j+1] <- 1 
      n <- n + 1 
    }
    res[j] <- (2*res[j]) %% 10 + carry[j]
    carry[j] <- 0
    j <- j + 1
  }
  
}
res[length(res):1]
sum(res)