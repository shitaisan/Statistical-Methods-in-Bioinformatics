N <- 1000000
p <- c('a' = 1/4, 'c' = 1/4, 'g' = 1/4, 't' = 1/4)
nucls <- c('a', 'c', 'g', 't')
M <- c('gatggtgg', 'gctggtgg', 'ggtggtgg', 'gttggtgg')
k <- nchar(M[1])
m <- length(M)

motifnumb <- c()
mean_dist_between_occur <- c()
for (k in 1:100){
  DNA <- sample(nucls, N, replace=T, prob = p)
  textDNA <- paste(DNA, collapse = '')
  
  all <- c()
  for (w in M){
    pattern <- paste(c('(?=', w, ')'), collapse = '')
    one <- gregexpr(pattern, textDNA, perl = T)[[1]]
    all <- union(all, one)
  }
  l <- length(all)
  motifnumb <- c(motifnumb, l)
  mean_dist_between_occur <- c(mean_dist_between_occur, 
                         mean(all[2:l]-all[1:(l-1)]))
}
#=========
mean(motifnumb)
probM <- sum(sapply(M, function(w) 
                    prod(p[strsplit(w, split='')[[1]]])))
(N-k+1)*probM

mean(mean_dist_between_occur)
1/probM
