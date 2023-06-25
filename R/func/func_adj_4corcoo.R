##name of script: func_adj_4corcoo.R
#cat("version_number= ",v_nr,"\n")
#function for adjustment of 4-corner rectangular buildings
#author: Joachim Höhle
#GNU General Public License (GPL)

adj_4corcoo <- function(X,Y) {
  X_adj <- rep(0,4)
  Y_adj <- rep(0,4)
  BT <- matrix(nrow=3,ncol=8)
  w <- rep(0,3)
  v <- rep(0,8)
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
  w[m] <- (X[i] - X[j]) * (X[k] - X[j]) + (Y[i] - Y[j]) * (Y[k] - Y[j])
  #
  
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
  w[m] <- (X[i] - X[j]) * (X[k] - X[j]) + (Y[i] - Y[j]) * (Y[k] - Y[j])
  #
  
  m=3
  i=3
  j=4
  k=1
  BT[m,1] <- X[i] - X[j]
  BT[m,2] <- Y[i] - Y[j]
  BT[m,3] <- 0
  BT[m,4] <- 0
  BT[m,5] <- X[k] - X[j]
  BT[m,6] <- Y[k] - Y[j]
  BT[m,7] <- 2*X[j] - X[i] - X[k]
  BT[m,8] <- 2*Y[j] - Y[i] - Y[k]
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
  
  vec <- 1 : 4
  
  for (m1 in vec) { 
    X_adj[m1] <- X[m1] + v[(2*m1-1),1]
    Y_adj[m1] <- Y[m1] + v[2*m1,1]
  }
  
  X_adj
  Y_adj
  
  XY_adj <- matrix(nrow=4,ncol=3)
  
  for (m1 in vec) { 
    XY_adj[m1,] <- c(m1,X_adj[m1],Y_adj[m1])
  }
  
  XY_adj
  v
  cat("residuals=", v, "\n")
  stdev <- sd(v)
  res3 <- list(XY_adj,stdev)
  return(res3)
} #end of function 'adj_4corcoo'

#end of script 'func_adj_4corcoo.R'
##########################################################################


