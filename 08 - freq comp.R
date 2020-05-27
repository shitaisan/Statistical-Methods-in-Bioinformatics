N <- 10000
p <- c('a' = 1/4, 'c' = 1/4, 'g' = 1/4, 't' = 1/4)
nucls <- c('a', 'c', 'g', 't')
a <- 50
b <- N-a

test <- function(k){
  DNA <- sample(nucls, N, replace=T, prob = p)
  statistic <- function(n){
    freqs <- rbind(table(DNA[1:n]), table(DNA[(n+1):N]))
    freqs <- rbind(freqs, apply(freqs, 2, sum))
    return (2*sum(freqs[1,]*log(freqs[1,]/n))
            +2*sum(freqs[2,]*log(freqs[2,]/(N-n)))
            -2*(sum(freqs[3,]*log(freqs[3,]/N))))
  }
  return (max(sapply(a:b), statistic)))
}

res <- sapply(1:100, test)
