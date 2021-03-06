rm(list=ls())
word1 <- strsplit('-gaatct', split = '')[[1]]
word2 <- strsplit('-catt', split = '')[[1]]
m <- length(word1)-1
n <- length(word2)-1
dp <- matrix(nrow = m + 1, ncol = n + 1, dimnames = list(word1, word2))

d <- 2
dp['-',] <- -(0:n)*d
dp[,'-'] <- -(0:m)*d

s <- sapply(word2, function(x) 2*(word1 %in% x)-1)
arrays <- matrix(nrow = m + 1, ncol = n + 1, dimnames = list(word1, word2))
arrays[1,-1] <- 'left'
arrays[-1,1] <- 'up'

for (i in 1:m)
  for (j in 1:n){
    one <- max(dp[i, j]+s[i+1, j+1])
    two <- dp[i, j+1]-d
    three <- dp[i+1, j]-d
    if (one>=two & one>=three) {dp[i+1, j+1] <- one; arrays[i+1, j+1] <- 'diag'}
    else if (two>=one & two>=three) {dp[i+1, j+1] <- two; arrays[i+1, j+1] <- 'up'}
    else {dp[i+1, j+1] <- three; arrays[i+1, j+1] <- 'left'}
  }
