##name of script: func_adj_6corcoo.R
#version_number: 1.2
#

adj_6corcoo <- function(n,X,Y) {
  X_adj <- rep(0,n)
  Y_adj <- rep(0,n)
  XY_adj <- matrix(nrow=n,ncol=3)
  BT <- matrix(nrow=n-1,ncol=2*n)
  w <- rep(0,n-1)
  v <- rep(0,2*n)
  #
  
  m=1 #(1.row)
  i=1
  j=2
  k=3
  BT[m,1] <- X[k] - X[j]
  BT[m,2] <- Y[k] - Y[j]
  BT[m,3] <- 2*X[j] - X[i] - X[k]
  BT[m,4] <- 2*Y[j] - Y[i] - Y[k]
  BT[m,5] <- X[i] - X[j]
  BT[m,6] <- Y[i] - Y[j]
  BT[m,7] <- 0
  BT[m,8] <- 0
  BT[m,9] <- 0
  BT[m,10] <- 0
  BT[m,11] <- 0
  BT[m,12] <- 0
  w[m] <- (X[i] - X[j]) * (X[k] - X[j]) + (Y[i] - Y[j]) * (Y[k] - Y[j])
  #
  m=2
  i=2
  j=3
  k=4
  BT[m,1] <- 0
  BT[m,2] <- 0
  BT[m,3] <- X[k] - X[j]
  BT[m,4] <- Y[k] - Y[j]
  BT[m,5] <- 2*X[j] - X[i] - X[k]
  BT[m,6] <- 2*Y[j] - Y[i] - Y[k]
  BT[m,7] <- X[i] - X[j]
  BT[m,8] <- Y[i] - Y[j]
  BT[m,9] <- 0
  BT[m,10] <- 0
  BT[m,11] <- 0
  BT[m,12] <- 0
  w[m] <- (X[i] - X[j]) * (X[k] - X[j]) + (Y[i] - Y[j]) * (Y[k] - Y[j])
  #
  m=3
  i=3
  j=4
  k=5
  BT[m,1] <- X[i] - X[j]
  BT[m,2] <- Y[i] - Y[j]
  BT[m,3] <- 0
  BT[m,4] <- 0
  BT[m,5] <- X[k] - X[j]
  BT[m,6] <- Y[k] - Y[j]
  BT[m,7] <- 2*X[j] - X[i] - X[k]
  BT[m,8] <- 2*Y[j] - Y[i] - Y[k]
  BT[m,9] <- 0
  BT[m,10] <- 0
  BT[m,11] <- 0
  BT[m,12] <- 0
  w[m] <- (X[i] - X[j])*(X[k]-X[j]) + (Y[i]-Y[j])*(Y[k]-Y[j])
  #
  m=4
  i=4
  j=5
  k=6
  BT[m,1] <- 0               
  BT[m,2] <- 0               
  BT[m,3] <- 0
  BT[m,4] <- 0
  BT[m,5] <- 0              
  BT[m,6] <- 0              
  BT[m,7] <- X[k] - X[j]              
  BT[m,8] <- Y[k] - Y[j]              
  BT[m,9] <- 2*X[j] - X[i] - X[k]
  BT[m,10] <- 2*Y[j] - Y[i] - Y[k]
  BT[m,11] <- X[i] - X[j]
  BT[m,12] <- Y[i] - Y[j]
  w[m] <- (X[i] - X[j])*(X[k]-X[j]) + (Y[i]-Y[j])*(Y[k]-Y[j])
  #
  m=5
  i=5
  j=6
  k=1
  BT[m,1] <- X[i] - X[j]
  BT[m,2] <- Y[i] - Y[j]
  BT[m,3] <- 0
  BT[m,4] <- 0
  BT[m,5] <- 0        
  BT[m,6] <- 0        
  BT[m,7] <- 0        
  BT[m,8] <- 0        
  BT[m,9] <- X[k] - X[j]
  BT[m,10] <- Y[k] - Y[j]
  BT[m,11] <- 2*X[j] - X[i] - X[k]
  BT[m,12] <- 2*Y[j] - Y[i] - Y[k]
  w[m] <- (X[i] - X[j])*(X[k]-X[j]) + (Y[i]-Y[j])*(Y[k]-Y[j])
  #
  w
  BT
  B <- t(BT)
  B
  BTB <- BT%*%B
  BTB
  BTBinv <- solve(BTB)
  BTBinv
  
  korr <- (BTBinv)%*%(-w)
  korr
  v <- B%*%korr
  
  vec <- 1 : n
  
  for (m1 in vec) { 
    X_adj[m1] <- X[m1] + v[(2*m1-1),1]
    Y_adj[m1] <- Y[m1] + v[2*m1,1]
  }
  
  X_adj
  Y_adj
  
  for (m1 in vec) { 
    XY_adj[m1,] <- c(m1,X_adj[m1],Y_adj[m1])
  }
  
  XY_adj
  v
  stdev <- sd(v)
  res4 <- list(XY_adj,stdev)
  return(res4)
} #end of function 'adj_6corcoo'

#end of script 'func_adj_6corcoo.R'
##########################################################################


