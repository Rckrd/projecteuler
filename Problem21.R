#problem 21


#Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
#If d(a) = b and d(b) = a, where a b, then a and b are an amicable pair and each of a and b are called amicable numbers.

#For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; 
#therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

#Evaluate the sum of all the amicable numbers under 10000.


library(gmp)
library(divisors)

d <- function(n){
  divs <- divisors(n)$divs
  divs <- divs[-length(divs)]
  sum(divs)
}

isAmicable <- function(a,b){
  d(a) == b && d(b) == a && a != b
}

isAm <- NULL
N <- 10000
for (i in 1:N){
  isAm[i] <- isAmicable(i,d(i))
}
AmNums <- isAm*1:N #find which numbers
AmNums <- AmNums[AmNums>0]


