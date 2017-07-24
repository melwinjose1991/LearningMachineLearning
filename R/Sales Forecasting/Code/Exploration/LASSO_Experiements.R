library(glmnet)
library(psych)

createVarWithCorr = function(x1, rho=0.6){
  n     <- length(x1)            # length of vector
  theta <- acos(rho)             # corresponding angle
  x2    <- rnorm(n, 2, 0.5)      # new random data
  X     <- cbind(x1, x2)         # matrix
  Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
  
  Id   <- diag(n)                               # identity matrix
  Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
  P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
  x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
  Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
  Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
  
  x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
  cor(x1, x)
  x
}

## 2 perfectly correlated variables
X1 = 101:200
X2 = createVarWithCorr(X1, 0.99)
cor(X1,X2)

Y = X1 + rnorm(length(X1))

x = as.matrix(data.frame(x1=X1, x2=X2))

grid = 2.71828 ^ seq(-100, 1, length = 1000)
cv_fit = cv.glmnet(x, Y, nfolds = 100, alpha=1, lambda=grid)
plot(cv_fit)
coef(cv_fit, s="lambda.min")


fit = glmnet(x, Y, alpha=1, lambda=grid)
plot(fit)


## HClust
X1 = rnorm(100, sd=10)
X2 = createVarWithCorr(X1, 0.99)

X3 = createVarWithCorr(X1, 0.30)
X4 = createVarWithCorr(X1, -0.15)
cor(X4, X3)

X5 = rnorm(length(X1), mean=10, sd=100)
X6 = rnorm(length(X1), mean=5, sd=10)
X7 = X5 + X6
cor(X7, X5)
cor(X7, X6)

cor(X4, X6)

x = as.matrix(data.frame(x1=X1, x2=X2, x3=X3, x4=X4, x5=X5,
                         x6=X6, x7=X7))

c = cor(scale(x))
c

a = abs(c)
a

d = dist(a)
d

h = hclust(d, method = "single")
h

plot(h)

cutree(h, h=0.5)