#The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
#Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
#(Please note that the palindromic number, in either base, may not include leading zeros.)




dectobin <- function(n){
t <- paste(rev(as.integer(intToBits(as.bigz(n)))), collapse="") 
t <- as.integer(substr(t,regexpr("[^0]",t),nchar(t)))
t
}


bintodec <- function(b){
  x <- as.integer(unlist(strsplit(as.character(b),"")))
  #x %*% rev(2^(0:(length(x)-1)))
  sum(2 ^ (which(as.logical(rev(x))) - 1)) 
}

is.palin <- function(s){
  prod(rev(unlist(strsplit(s,""))) == unlist(strsplit(s,"")))
}


is.palindromicindecandbin <- function(n){
  ispalin(as.character(n)) && ispalin(as.character(dectobin(n)))
}

res <- NULL
for (i in 1:10^6)
  if(is.palin(as.character(i))) res <- append(res,i)

res <- NULL
for (n in decpalin)
  if (is.palin(as.character(binary(n)))) res <- append(res,n)




    
binary<-function(p_number) {  
  bsum<-as.bigz(0)  
  bexp<-as.bigz(1)  
  while (p_number > 0) {
    digit<-p_number %% 2    
    p_number<-floor(p_number / 2)     
    bsum<-bsum + digit * bexp     
    bexp<-bexp * 10  
    }
return(bsum)
}

p_number<-readline("Decimal number?: ")
p_number<-as.numeric(p_number)
bsum<-binary(p_number)cat("Binary: ", bsum)    
    
    
    