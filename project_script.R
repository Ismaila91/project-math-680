## Loading the cookie data set
load("C:/Users/User/Documents/Ismaila/Cours Computation Intensive Statistics/cookie.RData")

train.set = cookie.app
test.set = cookie.val

## Lasso regression using the glmnet package
library(glmnet)
m.lasso = cv.glmnet(x=as.matrix(train.set[,-1]), y=train.set$sucres)
plot(m.lasso)
coef.lasso = coef(m.lasso, s="lambda.min")
predict.sucres.lasso = predict(m.lasso, newx=as.matrix(test.set[,-1]), s="lambda.min")
## prediction error in case of lasso regression
pred.error.lasso = mean((test.set$sucres-predict.sucres.lasso)^2)

## Adaptative lasso regression 
m.ridge = cv.glmnet(x=as.matrix(train.set[,-1]), y=train.set$sucres, alpha=0)
v.weights = 1/abs(as.matrix(coef(m.ridge, s="lambda.min"))[,1][2:ncol(train.set)])^.5
m.adalasso = cv.glmnet(x=as.matrix(train.set[,-1]), y=train.set$sucres, penalty.factor=v.weights)
plot(m.adalasso)
coef.adalasso = coef(m.adalasso, s="lambda.min")
predict.sucres.adalasso = predict(m.adalasso, newx=as.matrix(test.set[,-1]), s="lambda.min")
## prediction error in case of adaptive lasso regression
pred.error.adalasso = mean((test.set$sucres-predict.sucres.adalasso)^2) 

## Fused lasso regression using admm algorithm

## Soft-threshold operator of a vector x and a threshold tresh
soft.op = function(x, thresh){
  soft.thresh= numeric(length(x))
  soft.thresh[which(x>thresh)] = x[which(x>thresh)] - thresh
  soft.thresh[which(x < -thresh)] = x[which(x < -thresh)] + thresh
  return(soft.thresh)
}

## ADMM algorithm for fused lasso
m = 50 ## the number of iterations
r = .01*max(eigen(crossprod(X))$values) ## rho 
l=m.lasso$lambda.min ## lambda 
X = as.matrix(train.set[,-1]) ## the design matrix
y = train.set$sucres ## the response variable

## The difference operator D. I only encourage sparsity in the difference
## of the coefficients, i.e i set lambda1=0
D = matrix(0, ncol(X)-1, ncol(X))
d = nrow(D)           
for (i in 1:d) {
  ifelse(i==1, D[i,c(1,2)]<-c(-1,1), ifelse(i==d, D[i,c(d-1,d)]<-c(-1,1),
                                           D[i,c(i-1,i+1)]<-c(-1,1)))
}

beta = as.matrix(rep(1,ncol(X))) ## Initialization of beta
w = as.matrix(numeric(length = d)) ## Initialization of w
alpha = as.matrix(rep(1,d)) ## initialization of alpha
for (k in 1:m) {
  beta = qr.solve(crossprod(X)+r*crossprod(D))%*%(t(X)%*%y+r*t(D)%*%(alpha-w))
  alpha = as.matrix(soft.op(drop(D%*%beta+w),l/r))
  w = w + D%*%beta - alpha
}
## Prediction and prediction error
pred.fused.lasso = as.matrix(test.set[,-1])%*%beta
pred.error.fusedlasso = mean((test.set$sucres-pred.fused.lasso)^2)
pred.error.fusedlasso
