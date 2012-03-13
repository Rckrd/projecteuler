#Problem 7


#By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
#What is the 10 001st prime number?


f <- function(n) {sum(n %% 1:20)}
ElapsedTime <- system.time({
  ##########################
  n <- 2*10^6
  cb <- rep(T,n)
  for (i in 2:(n/2))
    if(cb[i])
    {
      for (j in seq(2*i,n,i))
        cb[j] <- F
    }
  primes <- cb*1:n 
  primes <- primes[primes>0] #remove zeros
  primes[10002]  
  ##########################
})[3]
ElapsedMins<-floor(ElapsedTime/60)
ElapsedSecs<-(ElapsedTime-ElapsedMins*60)
cat(sprintf("\n The answer is:  %d \n Total elapsed time:  %d minutes and %f seconds\n", answer, ElapsedMins, ElapsedSecs))


#Now all i such that A[i] is true are prime.