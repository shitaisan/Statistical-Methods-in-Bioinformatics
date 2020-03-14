N <- 100000
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

#check means of motif number and distance between occurences
#=========
mean(motifnumb)
probM <- sum(sapply(M, function(w) 
  prod(p[strsplit(w, split='')[[1]]])))
(N-k+1)*probM

mean(mean_dist_between_occur)
1/probM

# check var of motif number
#==========
eps <- array(dim = c(m, m, k), dimnames = list(M, M, 1:k))
for (u in M){
  for (v in M){
    begin <- substring(v, 1, 1:nchar(v))
    end <- substring(u, nchar(u):1, nchar(u))
    eps[u, v, ] <- begin==end
  }
}

pi <- array(dim = c(m, m, k), dimnames = list(M, M, 1:k))
for (u in M){
  u1 <- strsplit(u, split='')[[1]]
  for (v in M){
    v1 <- strsplit(v, split='')[[1]]
    for (j in 1:k)
        pi[u, v, j] <- prod(p[u1], p[v1])/prod(p[v1[1:j]])
  }
}

var(motifnumb)
(N-k+1)*probM-((2*k-1)*N-3*k^2+4*k-1)*probM^2+2*sum((N-2*k+1:(k-1)+1)*apply(pi*eps, 3, sum)[-k])
  
  
