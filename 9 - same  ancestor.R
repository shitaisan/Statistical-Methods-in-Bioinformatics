N <- 100
p <- c('a' = 1/4, 'c' = 1/4, 'g' = 1/4, 't' = 1/4)
nucls <- c('a', 'c', 'g', 't')
dna1 <- sample(nucls, N, prob = p, replace = T)
dna2 <- sample(nucls, N, prob = p, replace = T)
k <- 5
equal <- dna1==dna2
