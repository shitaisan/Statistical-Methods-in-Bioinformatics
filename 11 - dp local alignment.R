rm(list=ls())
word1 <- strsplit('-gaatct', split = '')[[1]]
word2 <- strsplit('-catt', split = '')[[1]]
m <- length(word1)-1
n <- length(word2)-1
dp <- matrix(nrow = m + 1, ncol = n + 1, dimnames = list(word1, word2))

d <- 2
dp[,1] <- 0
dp[1,] <- 0

s <- sapply(word2, function(x) 2*(word1 %in% x)-1)
arrays <- matrix(nrow = m + 1, ncol = n + 1, dimnames = list(word1, word2))

for (i in 1:m)
  for (j in 1:n){
    one <- max(dp[i, j]+s[i+1, j+1])
    two <- dp[i, j+1]-d
    three <- dp[i+1, j]-d
    if (all(one<=0, two<=0, three<=0)) dp[i+1, j+1] <- 0
    else if (one>=two & one>=three) {dp[i+1, j+1] <- one; arrays[i+1, j+1] <- 'diag'}
    else if (two>=one & two>=three) {dp[i+1, j+1] <- two; arrays[i+1, j+1] <- 'up'}
    else {dp[i+1, j+1] <- three; arrays[i+1, j+1] <- 'left'}
  }
i <- which(dp==max(dp), arr.ind = T)[,1]
j <- which(dp==max(dp), arr.ind = T)[,2]
while (!is.na(arrays[i, j])){
  if (arrays[i, j]=='diag') {i <- i-1; j <- j-1;}
  else if (arrays[i, j]=='up') i <- i-1
  else if (arrays[i, j]=='left') j <- j-1
}
