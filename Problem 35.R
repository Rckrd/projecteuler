#The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
#There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
#How many circular primes are there below one million?

library(gmp)


#check for primality
library(divisors)
is.prime <- function(n) divisors(n)$num==2

#populate a list of primes below 10^6
primes <- NULL
for (i in seq(1,10^6,2))
  if(is.prime(i)) primes <- append(primes,i)



circ <- function(n){
  #returns the next rotation as string
  x <- unlist(strsplit(as.character(n),""))
  paste(c(x[-1],x[1]),collapse="")
}

#checks primality of all rotations
is.circprime <- function(n){
  res <- T
  for (i in 1:nchar(n)){
    res <- res*is.prime(as.integer(n))
    n <- circ(as.character(n))
  }
  res
}

#creates a list of all circular primes
circprimes <- NULL
for (n in primes)
  if (is.circprime(n)) circprimes <- append(circprimes,n)


#####################################################
#Paolo
R  

library(gmp)
primegenerator <- function(n) (1:n)[gmp::isprime(1:n) != 0]
sf <- function(x) strsplit(as.character(x),"")[[1]]
is_circular <- function(n, verbose=FALSE) {
  s <- sf(n)
  j <- sf(paste(rep(n,length(s)),collapse=""))
  a <- 0
  for ( i in 1:length(s) ){
    if(isprime(paste(j[i:(length(s)+a)],collapse=""))) {
      if(verbose) print(paste(j[i:(length(s)+a)],collapse=""))
      a = a + 1
    } else return(FALSE)
  }
  return(TRUE)
}
is_circular_v <- Vectorize(is_circular)
p <- primegenerator(10^6)
print(length(p[is_circular_v(p)]))
######################################################

