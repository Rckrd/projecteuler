
# 9
#A Pythagorean triplet is a set of three natural numbers, a b c, for which,
#a2 + b2 = c2
#For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
#There exists exactly one Pythagorean triplet for which a + b + c = 1000.
#Find the product abc.

a^2+b^2==c^2

a <- 3
b <- 4
c <- 5


#brute and stupid, too stupid
for (a in 1:1000)
  for (b in 1:1000)
    for (c in 1:1000)
      if (a^2+b^2==c^2) res <- c(a,b,c)


#brute and stupid (uses a+b+c=1000)
res <- NULL
N <- 1000
for (b in 1:N)
  for (c in 1:N)
    if ( ( (N-(b+c))^2 + b^2 == c^2 ) && c(N-(b+c),b,c) >0 ) res <- c(N-(b+c),b,c)
res
