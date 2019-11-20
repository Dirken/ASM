MSPEkfold <- function(X, Y, K, n.lambdas, lambda.v, n) {
  PMSE.CV <-matrix(nrow = K, ncol=n.lambdas)
  folds <- sample(rep(1:K, length=n), n, replace=FALSE) 
  MPSE <- data.frame()
  # Iterate through K Folds
  for (k in 1:K){
    Xk <- as.matrix(X[folds != k,])
    Yk <- as.matrix(Y[folds != k])
    Xv <- as.matrix(X[folds == k,])
    Yv <- as.matrix(Y[folds == k])
    r <- dim(Xv)[1]
    mspe.k.val = MSPEvalidation(Xk, Yk, Xv, Yv, lambda.v, n.lambdas)
    MPSE <- rbind(MPSE, mspe.k.val) 
  }
  MPSE2 <- c()
  for (i in 1:n.lambdas){
    MPSE2 <- append(MPSE2, mean(MPSE[,i]))
  }
  return(MPSE2)
}

# leave-one-out
MSPEcv <- function(X, Y, n.lambdas, lambda.v, n) {
  PMSE.CV <- numeric(n.lambdas)
  for (l in 1:n.lambdas){
    lambda <- lambda.v[l]
    PMSE.CV[l] <- 0
    for (i in 1:n){
      #   m.Y.i <- mean(Y[-i])
      m.Y.i <- 0
      X.i <- X[-i,]
      Y.i <- Y[-i]-m.Y.i
      Xi <- X[i,]
      Yi <- Y[i]
      beta.i <- solve(t(X.i)%*%X.i + lambda*diag(1,p)) %*% t(X.i) %*% Y.i
      hat.Yi <- Xi %*% beta.i + m.Y.i
      PMSE.CV[l] <- PMSE.CV[l] + (hat.Yi-Yi)^2
    }
    PMSE.CV[l] <- PMSE.CV[l]/n
  }
  return(PMSE.CV)
}

MSPEvalidation <- function(X, Y, Xval, Yval, lambda.v, n.lambdas) {
  PMSE.VAL <- n.lambdas
  # Iterate through candidate values
  for (l in 1:n.lambdas){
    lambda <- lambda.v[l]
    r <- dim(Xval)[1]
    PMSE.VAL[l] <- 0
    # Compute beta with the traning dataset
    beta.i <- solve(t(X)%*%X + lambda*diag(1,p)) %*% t(X) %*% Y
    for (i in 1:r){
      # Compute the errors with the validation dataset
      Xi <- Xval[i,]; Yi <- Yval[i]
      hat.Yi <- Xi %*% beta.i
      PMSE.VAL[l] <-PMSE.VAL[l] + (hat.Yi - Yi)^2
    }
    PMSE.VAL[l] <- PMSE.VAL[l]/r
  }
  return(PMSE.VAL)
}