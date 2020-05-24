G <- 100000
L <- 500

N <- 2000

props <- c()
consnum <- c()
sizes <- c()
for (k in 1:300){
  fragments <- sort(sample(1:(G-L), N, replace = T))
  # number of contigs
  consnum <- c(consnum, sum(fragments[2:N]-fragments[1:(N-1)]>500)+1)
  # genom proportion covered by contigs
  DNA <- rep(F, length = G)
  for (i in fragments)
    DNA[i:(i+L)] <- T
  props <- c(props, sum(DNA)/G)
  # mean contig size
  sizes <- c(sizes, sum(DNA)/consnum)
}

mean(props)
mean(consnum)
mean(sizes)
