#####################
#library("MRCE")

MTGS.mrce<-function(X, Y, r){
  requireNamespace("MRCE")
  n<-nrow(X)
  p<-ncol(X)
  q<-ncol(Y)
  m<-round(n*r)


  for (k in 1:25){
    tst<-sample(1:n,size=m,replace=FALSE)

    XTRN<-X[-tst,] ; YTRN<-Y[-tst,]
    XTST<-X[tst,] ; YTST<-Y[tst,]

    fit=mrce(Y=YTRN, X=XTRN, lam1=0.25, lam2=1e-5, method="single")
    lam2.mat=1000*(fit$Bhat==0)
    refit=mrce(Y=YTRN, X=XTRN, lam2=lam2.mat, method="fixed.omega", omega=fit$omega, tol.in=1e-12)
    summary(refit)
    Bhat<-refit$Bhat
    XTST<-as.matrix(XTST)
    X<-as.matrix(X)
    Pred<-X%*%Bhat
    Pred1<-XTST%*%Bhat
  }
  return(list("Bhat"=fit$Bhat, "muhat"=fit$muhat, "Pred"=Pred))
}

