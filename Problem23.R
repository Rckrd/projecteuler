#problem 23

#A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
# For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
#which means that 28 is a perfect number.
#A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant 
# if this sum exceeds n.
#As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, 

#the smallest number that can be written as the sum of two abundant numbers is 24. 
#By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the 
#sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis 
#even though it is known that the greatest number that cannot be expressed as the sum of two 
#abundant numbers is less than this limit.

#Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.


#load good libraries
library(gmp)
library(divisors)


#function - is perfect
isperfect <- function(n){
  d <- divisors(n)$divs
  sum(d[-length(d)]) == n
}

isabundant <- function(n){
  d <- divisors(n)$divs
  sum(d[-length(d)]) > n
}


#create a vector of abundant numbers from 1 to 28123
N <- 28123 #bad
#N <- 20161 #better

abnums <- NULL
for (i in 1:N){
  if (isabundant(i)) abnums <- append(abnums,i)
}


#########################
# not the best solution
sabnums <- abnums[1:100]
#create a list of all non-abundant number that does not have 
res <- NULL
for (i in 1:max(sabnums)*2)
  if (!(i %in% sabnums && sum((sabnums - i %in% sabnums))>0)) res <- append(res,i)

nums <- expand.grid(sabnums,sabnums)
library(plyr)  
nums <- mdply(nums,"+")
##################


fpart <- function(n){
answer <- 0
for (i in floor(n/2):1)
  if (i %in% abnums && (n-i) %in% abnums){
      answer <- answer + 1 
      break
  }
  answer
}
  
res <- NULL
for (i in 1:1000)
    res <- append(res,fpart(i))
sum((1:1000)[!res])
    

abnums[abnums %% 2 != 0]

s2abnums[1:100] #all even numbers over 48 are present in the sum of two abnums


#we need an odd number - can only be constructed from an odd + an even. 

oddabnums <-  abnums[abnums %% 2 != 0]
evenabnums <- abnums[abnums %% 2 == 0]


numbersnotsumofabnums <- setdiff(1:47,s2abnums[s2abnums<48])


  
#construct all odd sums of abnums

oddsumsofabnums <- NULL
  for (i in oddabnums)
    for (j in evenabnums)
      oddsumsofabnums <- append(oddsumsofabnums,j+i)


oddnumsnotsums <- setdiff(seq(49,max(oddsumsofabnums),2),oddsumsofabnums)

sum(c(numbersnotsumofabnums,oddnumsnotsums))


sum(numbersnotsumofabnums)
sum(c(numbersnotsumofabnums,oddnumsnotsums[1:1417]))
