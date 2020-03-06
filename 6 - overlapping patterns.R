N <- 1e8
w <- "gggggggggg"
prob <- c('a' = 1/6, 'c' = 1/6, 'g' = 1/2, 't' = 1/6)
DNA <- sample(c('a', 'c', 'g', 't'), N, replace=T, prob = prob)

textDNA <- paste(DNA, collapse = '')
N <- nchar(textDNA)
DNA <- strsplit(textDNA, split = NULL)[[1]]
pattern <- paste(c('(?=', w, ')'), collapse = '')

x <- gregexpr(pattern, textDNA, perl = T)[[1]]

count <- c(1)
for (i in 1:(length(x)-1)){
  if (x[i+1]-x[i]<nchar(w))
    count[length(count)] <- count[length(count)]+1
  else count <- c(count, 1)
}

plot(ecdf(count))
p <- 1-1/mean(count)
curve(pgeom(x, prob = p), add = T)

# ============ number of letters before first appearance
# install.packages("stringi")
w <- 'gaga'
N <- 100000
begin <- substring(w, 1, 1:nchar(w))
end <- substring(w, nchar(w):1, nchar(w))
eps <- begin==end
test <- function(x){
  DNA <- sample(c('a', 'c', 'g', 't'), N, replace=T, prob = prob)
  textDNA <- paste(DNA, collapse = '')
  return (stringi::stri_locate_first_fixed(textDNA, w)[,1])
}

res  <- sapply(1:1000, test)

mean(res, na.rm = T)
# exact mean  
temp <- eps/sapply(begin, function(x) prod(prob[strsplit(x, split = NULL)[[1]]]))
print(m <- sum(temp))
var(res, na.rm = T)
m^2+m-2*sum(1:nchar(w)*temp)
