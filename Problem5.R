#5 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?



#Stupid brute force
N <- 20
while (sum(N %% 1:20)>0){
  N <- N +1
}
N


#
f <- function(n) {sum(n %% 1:20)}
for (i in seq(2520,10^9,2520)) if( f(i)==0 ) print(i)