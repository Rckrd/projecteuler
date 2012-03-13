#If we list all the natural numbers below 10 
#that are multiples of 3 or 5, 
#we get 3, 5, 6 and 9. The sum of these multiples is 23.

#Find the sum of all the multiples of 3 or 5 below 1000.




ElapsedTime <- system.time({
  ##########################
sum(union(1:199*5,1:333*3))
  ##########################
})[3]
ElapsedMins<-floor(ElapsedTime/60)
ElapsedSecs<-(ElapsedTime-ElapsedMins*60)
cat(sprintf("\n The answer is:  %d \n Total elapsed time:  %d minutes and %f seconds\n", answer, ElapsedMins, ElapsedSecs))





