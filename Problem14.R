#problem 14
# The following iterative sequence is defined for the set of positive integers:
#   
#   n n/2 (n is even)
# n 3n + 1 (n is odd)
# 
# Using the rule above and starting with 13, we generate the following sequence:
#   
#   13 40 20 10 5 16 8 4 2 1
# 
# It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
# 
# Which starting number, under one million, produces the longest chain?
# 
# NOTE: Once the chain starts the terms are allowed to go above one million.


#loops through all - returns vector
f <- function(N){
res <- N
  n <- N
  while (n > 1){
    if (n %% 2 == 0) n <- n/2
    else n <- 3*n+1
  res <- append(res,n)
  }
  res
}


N <- 5*10^5
y <- NULL
for (i in N:10^6){
  y <- append(y,length(f(i)))
}

qplot(1:N,x)m
  

#better to build a memory



qplot(x=1:length(f(1000)),f(1000),geom="line")


#loop build
x <- matrix(c(1,0),nrow=2)
for (i in 2:10^5){
  x <- cbind(x,c(i,fNV(i)))
}

#vector build

even <- seq(2,10^2,2)
even <- rbind(even,2*even)

odd <- seq(1,10^2-1,2)
odd <- rbind(odd,2*odd)

nv <- cbind(even,odd)

for (i in 1:100){
  cat(nv[2,i],"->",nv[1,i],"\n")
}


df <- data.frame(nv[1,],nv[2,])
names(df) <- c("n","stop_time")

qplot(data=df, n, stop_time)










#first the next value function
fNV <- function(n){
  if (n == 1) 1
  else if (n %% 2 == 0) n <- n/2
  else n <- 3*n + 1
  n
}




#returns the stop time
fST <- function(N){
  res <- N
  n <- N
  while (n > 1){
    n <- fNV(n)
    res <- append(res,fNV(n))
  }
 length(res)
}



# we need a function that calculates the stop time, using a memory
#start with seed (1)
#N is start number, n is variable




fSTM <- function(N) {
#initialize memory
biggest <- 0
st <- 1
M <- matrix(c(1,1),nrow=2)  

N <-1000
  for (i in 1:N){
  n <- i
  
  while (n > 1){
        q <- n
        n <- fNV(n)
        #check if member, if true return the stop time - done
        if (is.element(n, M[1,])) 
          st <- M[2,which(n == M[1, ])] + 1
        else{ #calculate stoptime and add to matrix
          st <- fST(n) + 1
          M <- cbind(M, c(q, st))
        }
        if ( st > biggest ) biggest <- st
      }
      #calculate next value

    }
    #return the starting number for the longest chain
   # M[1,which(M[2,] == max(M[2,]))]
   biggest
}




## Problem 14
# Collatz conjecture
problem14 <- function(N) {
  maxChain <- 0
  chains <- rep(0,N)
  x <- 1
  for (i in 1:N) {
    n <- i
    chain <- 0
    while(n > 1) {
      n <- ifelse(n %% 2 == 0, n/2, 3*n+1)
      chain <- chain + 1
      if (n < N && chains[n] > 0) {
        chain <- chain + chains[n]
        break
      }
      
    }
    chains[i] <- chain
    if (chain > maxChain) {
      maxChain <- chain
      x <- i
    }
  }
  x
}



