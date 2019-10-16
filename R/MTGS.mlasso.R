##########################
#library("glmnet")

MTGS.mlasso<-function(X, Y, r){


  requireNamespace("glmnet")
  n<-nrow(X)
  p<-ncol(X)
  q<-ncol(Y)
  m<-round(n*r)


  for (k in 1:25){
    tst<-sample(1:n,size=m,replace=FALSE)

    XTRN<-X[-tst,] ; YTRN<-Y[-tst,]
    XTST<-X[tst,] ; YTST<-Y[tst,]

    XTRN<-as.matrix(XTRN)
    YTRN<-as.matrix(YTRN)
    XTST<-as.matrix(XTST)
    YTST<-as.matrix(YTST)
    X<-as.matrix(X)

    #library("glmnet")
    fit=glmnet(XTRN,YTRN,family="mgaussian")
    Pred<-predict(fit,newx=X,s=0.01)
    Pred<-data.frame(Pred)
    Pred<-as.matrix(Pred)

  }
  my_list=list("fit"=fit,"Pred"=Pred)
  return(my_list)
}
