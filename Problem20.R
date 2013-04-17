#Problem 20
#n! means n (n 1) ... 3 2 1

#For example, 10! = 10 9 ... 3 2 1 = 3628800,
#and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

#Find the sum of the digits in the number 100!
  


#easy way out -- gmp

library(gmp)

factorialZ(300)
as.bigz(factorial(30))

n0 <- as.bigz()
n <- as.character(n0)
n <- strsplit(n,"")
n <- unlist(n)
n <- as.integer(n)
sum(n)


#this is correct
sum(as.integer(unlist(strsplit(as.character(factorialZ(100)),""))))