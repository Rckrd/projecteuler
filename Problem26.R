#PRoblem 26
#Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
#Find the value of d 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.




library(ggplot2)
library(plyr)
library(reshape2)
library(pracma)


# create function to calculate vector of decimals, division algoritm
dmax <- 1000
len <- 2*dmax

div <- function(d){
  # returns decimals in vectorform
  x <- 1 # x / d, we are interested in 1/d
  ret <- vector(length = len)
  if(d == 0) return(NA)
  for(i in 1:len) {
    ret[i] <- x %/% d
    x <- (x %% d) * 10
  }
#remove the zeros before first non-zero
rt <- floor(log(d,10)) + 1
ret[-(1:rt)]
}

# create matrix with rows corresponding to 1 / row
# brute force, we could have removed even and known 1 cycles
x <- sapply(1:dmax, div)

# the objective function 
# takes a vector x, and a diff, n and calculates a scalar
objfun <- function(n, x){
  sum(abs(diff(x, lag = n)))
}

fcyc <- function(x) {
  val <- sapply(1:length(x), objfun, x)
  ret <- min(which(min(val) == val)) # return the first min value
  ret
}


# brute force apply
res <- sapply(x, fcyc)

#select the largest that is not equal ~2000



#----- other solutions
library("gmp")
D=2:999
delete.2.5 <- function(n){
  if(n<2) return(n)
  f=as.data.frame(table(as.integer(factorize(n))))
  primes=as.integer(levels(f[,1]))
  frequency=f[,2]
  if(primes[1]==5) n=(n/(5^frequency[1]))
  if(length(primes)>1 && primes[2]==5) n=(n/(5^frequency[2]))
  if(length(primes)>2 && primes[3]==5) n=(n/(5^frequency[3]))
  if(primes[1]==2) n=(n/(2^frequency[1]))
  return(n)
}
D=sapply(D,delete.2.5)
D=sort(unique(D))
D=D[D>1]
max.len=0
d.max.len=1
for(d in D){
  len=1
  digits=floor(log10(d))+1
  while(mod.bigz(mul.bigz((pow.bigz(10,len)-1),10^(digits-1)),d)!=0){
    len=len+1
  } 
  if(max.len<len){
    max.len=len
    d.max.len=d
  }
}
c(d.max.len,max.len)

#zzzzz

ans = 0
for (i in 1000:1) {
  if (i - 1 <= ans) break
  if (i %% 2 == 0 || i %% 5 == 0) next
  x = c(1, 10)
  while (x[2] != 1) x = c(x[1] + 1, (x[2] * 10) %% i)
  ans = max(ans, x[1])
}
ans + 1


