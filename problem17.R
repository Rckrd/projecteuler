#Problem 17
#If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
#If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used? 
#NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
#The use of "and" when writing out numbers is in compliance with British usage.


d <- c("","one","two","three","four","five","six","seven","eight","nine")
t <- c("ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen")
M <- c("twenty", "thirty","forty","fifty","sixty","seventy","eighty","ninety")

res1_to_99 <- NULL
for (cDT in union(d[2:10],t))
  res1_to_99 <- append(res1_to_99,cDT)

for (cm in M)
  for (cd in d)
    res1_to_99 <- append(res1_to_99,paste(cm,cd,sep=""))

H <- paste(d[2:10],"hundred",sep="")
td <- c("",paste("and",res1_to_99,sep=""))

res <- NULL
for (e in H)
  for (f in td)
    res <- append(res,paste(e,f,sep=""))

sum(nchar(c(res1_to_99,res,"onethousand")))




