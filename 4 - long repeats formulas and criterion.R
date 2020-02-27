
N <- 1e5
nucl <- c('a', 't', 'g', 'c')
p <- 1/4 #probability of 'a'
probs <- c(p, 1/4, 1/4, 1/4)
maxsize <- c()
for (k in 1:100){
  DNA <- paste(sample(nucl, N, replace = T, prob = probs), collapse = '')
  longrepeats <- gregexpr('a+', DNA)[[1]]
  repsizes <- attr(longrepeats, 'match.length')
  maxsize <- c(maxsize, max(repsizes))
}
n <- (1-p)*N # estimation for number of long repeats
gamma <- 0.5772156649
pi <- 3.14159265359

# checking formulas
mean(maxsize)
-(gamma+log(n))/log(p)-1/2
var(maxsize)
pi^2/(6*log(p)^2)+1/12

# testing of uniform appearance of nucleotide
criterion <- function(DNA, nucleotide = 'a', p = 1/4, alpha = 0.05){
  N <- nchar(DNA)
  longrepeats <- gregexpr(paste(c(nucleotide, '+'), collapse = ''), DNA)[[1]]
  repsizes <- attr(longrepeats, 'match.length')
  maxsize <- max(repsizes)
  pval <- 1-(1-p^maxsize)^(N*(1-p))
  if (pval>=alpha) print("nucleotide is iid uniform") 
  else print("nucleotide tends to long")
  return (pval)
}
