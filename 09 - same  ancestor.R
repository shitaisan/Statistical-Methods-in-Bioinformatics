N <- 100000
prob <- c('a' = 1/4, 'c' = 1/4, 'g' = 1/4, 't' = 1/4)
nucls <- c('a', 'c', 'g', 't')
dna1 <- sample(nucls, N, prob = prob, replace = T)
dna2 <- sample(nucls, N, prob = prob, replace = T)

k <- 5
equal <- dna1==dna2 
names(equal) <- 1:N

begin <- 1:(N-k)
end <- sapply(begin, function(begin) 
                      as.numeric(names(which.max(cumsum(!equal[begin:N])>k))))
end[is.na(end)] <- N
success <- end-begin-k

ymax <- max(success)
p <- sum(prob^2)
1-pnbinom(ymax, k+1, 1-p)

n <- N*(1-p)
r1 <- 3.45*1e-4
r2 <- 2.64*1e-2
gamma <- 0.5772156649
pi <- 3.14159265359
lambda <- -log(p)
mu <- (log(n)+gamma+k*log(log(n)/lambda)+k*log(1/p-1)-log(prod(1:k)))/lambda - 1/2 +r1
sqsigma <- pi^2/(6*lambda^2) + 1/12 +r2

1-exp(-exp(-(pi*(ymax-mu)/(sqsigma-6^(1/2))+gamma)))
