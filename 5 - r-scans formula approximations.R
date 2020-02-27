N <- c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9)
u <- 0.00001
h <- ceiling(1/u-1)
g <- 1:h

exact <- c()
approx1 <- c()
approx2 <- c()
for (n in N){
  #probability of Umax>u
  exact <- c(exact, sum((-1)^(g+1)*choose(n+1, g)*(1-g*u)^n))
  approx1 <- c(approx1, 1-exp(-(n+1)*exp(-(n+1)*u)))
  approx2 <- c(approx2, (n+1)*exp(-(n+1)*u))
}

plot(approx1/exact)
