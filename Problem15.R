#problem 15
#triangle sum - closed form
# 
TN <- function(n){(n+1)/2*n}

x <- 1
N <- 10^6

res <- rep(0,3*N)
dim(res) <- c(3,N)

for (i in 1:N){
  res[1,i] <- i
  res[2,i] <- TN(i)
  res[3,i] <- 0
}
x
TN(x)



GetDivisor <- function(n){
  res <- NULL
  for (i in 1:ceiling(n/2,0))
    if (n %% i == 0) res <- append(res,i)
  res
}

DivisorFunction <- function(n,x){
  sum(GetDivisor(n)^x)
}

n <- TN(10^4)
divisors <- ((n %% 1:n == 0) * 1:n)
divisors <- divisors[divisors>0]
length(divisors)

