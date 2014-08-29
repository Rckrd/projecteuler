words < readline(url("http://projecteuler.net/project/resources/p042_words.txt"))
words <- names(words)
words[1645] <- "TRUE"
triangleNum <- function(n) n * (n + 1) / 2

triangleNums <- triangleNum(1:1000)

isTriangleNum <- function(n) n %in% triangleNums

alphaPos <- function(a) (1:26)[letters == tolower(a)]

sum(sapply())

wordsum <- function(word) sum(sapply(strsplit(word,'')[[1]], alphaPos))

i <- 0
for (word in words) {
    i <- i+1
    print(wordsum(word))
}

sum(sapply(words, function(w) isTriangleNum(wordsum(w))))
