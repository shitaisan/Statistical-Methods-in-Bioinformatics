G <- 100000
L <- 500

N <- 2000
M <- 200

a <- N*L/G
b <- M*L/G

anchconsnum <- c()
anchprops <- c()

for (i in 1:300){
  fragments <- sort(sample(1:(G-L), N, replace = T))
  anchpts <- sort(sample(1:G, M, replace = T))
  anchleng <- c()
  prev <- c()
  for (k in 1:M){
    anchcont <- fragments[fragments <= anchpts[k] & fragments > anchpts[k]-500]
    if (length(intersect(anchcont, prev))==0)
      anchleng <- c(anchleng, anchcont[length(anchcont)]+L-anchcont[1])
    else {
      anchcont <- union(prev, anchcont)
      anchleng[length(anchleng)] <- anchcont[length(anchcont)]+L-anchcont[1]
    }
    prev <- anchcont
  }
  anchconsnum <- c(anchconsnum, length(anchleng))
  anchprops <- c(anchprops, sum(anchleng)/G)
}

mean(anchconsnum)
N*b*(exp(-a)-exp(-b))/(b-a)
mean(anchprops)
1-(exp(-a)+a*exp(-a-b)+a*a*exp(-2*b)/((a-b)^2)+a*a*(b-a-1)*exp(-a-b)/((a-b)^2))
