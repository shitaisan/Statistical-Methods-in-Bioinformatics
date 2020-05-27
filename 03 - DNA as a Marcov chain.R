nucl <- c('a', 't', 'g', 'c')
P <- matrix(c(0.1, 0.6, 0.5, 0.2, 0.4, 0.1, 0.05, 0.3, 
              0.35, 0.1, 0.35, 0.25, 0.15, 0.2, 0.1, 0.25), nrow = 4, ncol = 4,
            dimnames = list(nucl, nucl))

DNA <- vector(length = G)
DNA[1] <- 'a'
for (i in 2:G)
  DNA[i] <- sample(nucl, 1, prob = P[DNA[i-1],])
