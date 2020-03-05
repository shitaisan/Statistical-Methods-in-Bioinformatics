N <- 316000
w <- "agaga"
DNA <- sample(c('a', 'c', 'g', 't', w), N, replace=T, 
              prob = c(0.24, 0.24, 0.24, 0.24, 0.04))
textDNA <- paste(DNA, collapse = '')
N <- nchar(textDNA)
DNA <- strsplit(textDNA, split = NULL)[[1]]
pattern <- paste(c('(?=', w, ')'), collapse = '')

x <- gregexpr(pattern, textDNA, perl = T)[[1]]

count <- c(1)
for (i in 1:(length(x)-1)){
  if (x[i+1]-x[i]<10)
    count[length(count)] <- count[length(count)]+1
  else count <- c(count, 1)
}
plot(ecdf(count))
curve(pgeom())

sum(x[2:length(x)]-x[1:(length(x)-1)]<10)
y <- gregexpr("gatgcacgatgcacgat", DNA)[[1]]
z <- gregexpr("gatgcacgatgcacgatgcacgat", DNA)[[1]]
a <- gregexpr("gatgcacgatgcacgatgcacgatgcacgat", DNA)[[1]]
