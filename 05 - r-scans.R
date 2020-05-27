library(Rmpfr)

criterion <- function(x){
  n <- length(x)
  x <- c(0, sort(x), 1)
  u <- x[2:(n+2)]-x[1:(n+1)]
  umax <- max(u)
  h <- ceiling(1/umax-1)
  g <- 1:h
  pval <- sum((-1)^(g+1)*chooseMpfr(n+1, g)*(mpfr(1-g*umax, precBits = 100))^n)
  return (list(u=umax, 
               pval=as.numeric(pval)))
}

experiment <- function(n){
  x <- runif(n)
  exact <- criterion(x)
  u <- exact$u
  approx1 <- 1-exp(-(n+1)*exp(-(n+1)*u))
  approx2 <- (n+1)*exp(-(n+1)*u)
  return (list(approx1=approx1/exact$pval, approx2=approx2/exact$pval))
}


n <- seq(100, 100000, length.out = 10)
res <- sapply(n, experiment)
