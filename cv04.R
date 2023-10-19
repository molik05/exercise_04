#TASK 1
ReturnCoins <- function(M){
  r <- M
  ppa <- floor(r/50)
  r <- r-50*ppa
  pdv <- floor(r/20)
  r <- r-20*pdv
  pde <- floor(r/10)
  r <- r-10*pde
  pp <- floor(r/5)
  r <- r-5*pp
  pd <- floor(r/2)
  r <- r-2*pd
  return(c(ppa,pdv,pde,pp,pd,r))
}
ReturnCoins(13)

#Task 2 universal
UniversalReturnCoins <- function(M,types){
  r <- M
  finals <- c()
  for (t in 1:length(types)){
    print(types[t])
    p <- floor(r/types[t])
    r <- r-(types[t]*p)
    finals <- append(finals,p)
  }
  return(finals)
}
UniversalReturnCoins(13,c(50,20,10,5,2,1))

#Task 3
Chocolate <- function(M,r,c){
  if (r==nrow(M)){
    return(M[r,c])}
  else {
    bars <- M[r,c]
    if (M[r+1,c+1] > M[r+1,c]){
      diagonal <- Chocolate(M,r+1,c+1)
      return(diagonal+bars)}
    else {
      down <- Chocolate(M,r+1,c)
      return(down + bars)}}
}

Chocolate(matrix(c(1,2,1,1,2,1,1,2,1), nrow=3,ncol=3,byrow = TRUE),1,1)

#Task 4
HanoiTowers <- function(n, fromPeg, toPeg){
  if (n==1){
    print(paste("Move disc from peg",fromPeg,"to Peg",toPeg,sep=" "))
    return()}
  unusedPeg <- 6 - fromPeg - toPeg
  HanoiTowers(n-1, fromPeg, unusedPeg)
  print(paste("Move disc from peg",fromPeg,"to Peg",toPeg,sep=" "))
  HanoiTowers(n-1,unusedPeg,toPeg)
  return()
}
HanoiTowers(3,1,3)
