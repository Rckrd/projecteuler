#problem 22

#Using names.txt (right click and 'Save Link/Target As...'), 
#a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. 
#Then working out the alphabetical value for each name, 
#multiply this value by its alphabetical position in the list to obtain a name score.

#For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, 
#is the 938th name in the list. So, COLIN would obtain a score of 938 53 = 49714.
#What is the total of all the name scores in the file?

folder <- "C:\\Users\\rickard.edholm\\Documents\\ProjectEuler\\names.txt"

xd <- read.table(folder, sep=",", na.strings="")
xd <- as.vector(t(xd))
names <- sort(xd)

pos <- 1:length(names)

falfa <- function(str){
  alfa <- c("a",  "b",	"c",	"d",	"e",	"f",	"g",	"h",	"i",	"j",	"k",	"l",	"m",	"n",	"o",	"p",	"q",	"r",	"s",	"t",	"u",	"v",	"w",	"x",	"y",	"z")
  X <- unlist(strsplit(str,""))
  res <- 0
  for (x in X)
    res <- res + which(alfa == tolower(x))
 res   
}


res <- 0
for (i in pos)
  res <- res + falfa(names[i])*i

2
y <- rep(c("abc","def","a","b","cdefgh","tjosanprosanhosan"),10)
aaply(y, 1, falfa) -> res



nlist <- scan(folder, what= "", sep=",", na.strings="")

dict = 1:26
names(dict) = unlist(strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ", ""))
score <- function (name) {
  return (sum(dict[unlist(strsplit(name, ""))]))
}

weighted.score = (1:length(nlist))*(sapply(nlist, score)) 
sum(weighted.score)





