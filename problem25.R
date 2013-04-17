#Problem 25
#The 12th term, F12, is the first term to contain three digits.
#What is the first term in the Fibonacci sequence to contain 1000 digits?

fib <- function(n) { #closed form
  a <- (1+sqrt(5))/2
  b <- (1-sqrt(5))/2
  (a^n-b^n)/sqrt(5)
}

NoNums <- function(n){
  n <- as.character(n)
  n <- strsplit(n,"")
  n <- unlist(n)
  n <- as.integer(n)
  n <- length(n)
  n
}




x <- as.bigz(1)
x <- append(x,as.bigz(1))
for (i in 2:10000){
  x <- append(x,x[i-1]+x[i])
}

xnums <- NULL

for (j in 1:length(x)){
  xnums <- append(xnums,NoNums(x[j]))
}

qplot(1:length(xnums),xnums)+geom_smooth(method=lm)

xnums.inc <- (xnums-c(0,xnums[-length(xnums)]))*1:length(xnums)

xnums.inc.2 <- xnums.inc[xnums.inc>0]




if <- function(n){
  n*0.5488-0.2090
}
  

#better solution
fibon1 = list()
fibon1[[1]] = fibon1[[2]] = as.bigz(1)
digits = 10000
i=3 
n=0

while(n < digits){
  fibon1[[i]] = fibon1[[i-1]]+fibon1[[i-2]]
  n = nchar(as.character(fibon1[[i]]))
  i=i+1
}
i-1


res <- NULL
for (i = 1 to 1000)
  res <- res + nums[[i]]
res


# snyggare lösning från the internetz


library(gmp)
first <- as.bigz(1)
second <- as.bigz(1)
third <- first + second
n <- 2

while(nchar(as.character(third)) < 1000) {
  n <- n + 1
  third <- first + second
  first <- second
  second <- third
}

cat("The result is:", n, "\n")

  
  
  