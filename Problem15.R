#problem 15
#routes in a 20x20 grid

#count all binary digits with a digit sum of 20



#load library
library(sfsmisc)





n <- 20
cnt <- 0
i <- 1
while (i < 2^(2*n)){
  b <- digitsBase(i,base=2, 2*n)
  if (sum(b) == n) cnt <- cnt + 1
  i <- i + 1
}
cnt


f <- function(n){
  factorial(2*n)/factorial(n)^2
}
  
