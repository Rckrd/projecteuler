


#145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
#Find the sum of all numbers which are equal to the sum of the factorial of their digits.
#Note: as 1! = 1 and 2! = 2 are not sums they are not included.


is.curious <- function(n){
  sum(factorial(as.integer(unlist(strsplit(as.character(n),"")))))==n
}

res <- NULL
for (i in 3:1000000)
  if(is.curious(i)) res <- append(res,i)