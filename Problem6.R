#Problem 6
#um of cubes equals square of sums for 1:10


#2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
#What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


f <- function(n) {sum(n %% 1:20)}

ElapsedTime <- system.time({
  ##########################
  for (i in seq(2520,10^9,2520)) if( f(i)==0 ) print(i)
  ##########################
})[3]
ElapsedMins<-floor(ElapsedTime/60)
ElapsedSecs<-(ElapsedTime-ElapsedMins*60)
cat(sprintf("\n The answer is:  %d \n Total elapsed time:  %d minutes and %f seconds\n", answer, ElapsedMins, ElapsedSecs))





