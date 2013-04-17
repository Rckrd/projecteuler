
#problem 28 - spriral

N <- 5

x <- 1:N^2


i1 <- 1
i2 <- seq(max(i1)+2,max(i1)+3^2,2)
i3 <- seq(max(i2)+4,max(i2)+4^2,4)
i4 <- seq(max(i3)+6,max(i3)+6^2,6)

k <- 1
N <- 5

for (i in 2:ceiling(N/2))  
  k <- append(k,seq(max(k)+2*i,max(k)+i^2,2*i))
  
k <- c(i1,i2,i3,i4)

sum(x[i])


d1 <- seq(2,2000,2)
d2 <- rep(seq(4,2000,4),each=2)
res1  <- 1
res2 <- 1
for (i in seq_len(length(d1))){
  res1 <- append(res1,res1[i]+d1[i])
  res2 <- append(res2,res2[i]+d2[i])
}
sum(res1)+sum(res2)-1

  