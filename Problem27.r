#problem 27
#n² + n + 41

#n² + an + b, where |a| < 1000 and |b| < 1000


# 
# Euler published the remarkable quadratic formula:
#   
#   n² + n + 41
# 
# It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. 
# However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, 
# and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
# 
# Using computers, the incredible formula  n² − 79n + 1601 was discovered, 
# which produces 80 primes for the consecutive values n = 0 to 79. 
# The product of the coefficients, −79 and 1601, is −126479.
# 
# Considering quadratics of the form:
#   
#   n² + an + b, where |a| < 1000 and |b| < 1000
# 
# where |n| is the modulus/absolute value of n
# e.g. |11| = 11 and |−4| = 4
# 
# Find the product of the coefficients, a and b, for the quadratic expression that produces 
# the maximum number of primes for consecutive values of n, starting with n = 0.


#check for primality
library(divisors)
is.prime <- function(n) divisors(n)$num==2

isPrime <- function(x){
  div <- 2:floor(sqrt(x))
  !any(x %% div == 0)
}


#populate a list of primes below 10^6, slow and bad
primes <- NULL
for (i in seq(1,10^6,2))
  if(is.prime(i)) primes <- append(primes,i)


# better, but not mine
primest <- function(n){
  p <- 2:n
  i <- 1
  while (p[i] <= sqrt(n)) {
    p <-  p[p %% p[i] != 0 | p==p[i]]
    i <- i+1
  }
  p
}

sieve <- function(n)
{
  n <- as.integer(n)
  if(n > 1e8) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}
# ------
primes <- sieve(10^6)


  
# |a| < 100, |b| < 100

a <- 1
b <- 41

qf <- function(n, a,b){
  n^2+a*n+b
}

N <- 10
A <- array(0,c(N*2,N*2))

for (a in primes[1:10])
  for (b in primes[1:10]){
  x <- qf(n, a, b) %in% primes
  A[N+a,N+b] <- min(which(x == F))
}
  


test <- function(a,b)
  min(which(qf(0:100) %in% primes

            
            
            
            
  
