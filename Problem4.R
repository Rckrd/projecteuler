#Problem 4
#A palindromic number reads the same both ways. 
#The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
#Find the largest palindrome made from the product of two 3-digit numbers.



xvector[length(xvector):1]

isPalim <- function(n) {
  xlist <- strsplit(as.character(n),"")
  xvector <- xlist[[1]]
  sum(xvector[length(xvector):1] == xvector) == length(xvector)
}


x <- NULL


for (i in 999:100)
  for (j in 999:100)
    if(isPalim(i*j)) 
    {
      x <- append(x,i*j)
    }
max(x)