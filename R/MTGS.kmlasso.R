################################
#library(glmnet)
#library(kernlab)

MTGS.kmlasso<-function(X,Y){
  requireNamespace("glmnet","kernlab")
  X<-as.matrix(X)
  Y<-as.matrix(Y)
  yin<-t(Y)
  kernelGausian=function(x,c,sigma){
    x=as.matrix(x)
    c=as.matrix(c)
    d=nrow(x)
    nx=ncol(x)
    nc=ncol(c)
    x2=colSums(x^2,1)
    x2=as.matrix(x2)
    c2=colSums(c^2,1)
    c2=as.matrix(c2)
    repmat = function(y,m,n){
      my = dim(y)[1]
      ny = dim(y)[2]
      matrix(t(matrix(y,my,ny*n)),my*m,ny*n,byrow=T)
    }
    distance2=repmat(t(c2),nx,1)+repmat(x2,1,nc)-2*t(x)%*%c
    X1=exp(-distance2/(2*sigma^2))
  }
  n<-nrow(X)
  d<-ncol(X)
  #Normalization
  x=X/((as.matrix(apply(X,1,sd)))%*%(rep(1,ncol(X))) +.Machine$double.eps)
  #Centering matrix
  H = diag(n) - 1/n*rep(1,n);
  #Transformation of input
  KH = matrix(0,n^2,d);
  requireNamespace("kernlab")
  rbf <- rbfdot(sigma = 0.05)
  for (ii in 1:d){
    Kx = kernelMatrix(rbf,x[,ii,drop=FALSE]);
    tmp = H%*%Kx%*%H;
    KH[,ii] = c(tmp);
  }
  KH
  #Transformation of output
  d1<-nrow(yin)
  n1<-ncol(yin)
  y=yin/((as.matrix(apply(yin,1,sd)))%*%(rep(1,ncol(yin))) +.Machine$double.eps)
  H1= diag(n1) - 1/n1*rep(1,n1);
  LH = matrix(0,n1^2,d1);
  for (ii in 1:d1){
    Ly = kernelGausian(y[ii,,drop=FALSE],y[ii,,drop=FALSE],1.0);
    tmp = H1%*%Ly%*%H1;
    LH[,ii] = c(tmp);
  }
  LH
  requireNamespace("glmnet")
  fit=glmnet(KH,LH,family="mgaussian")
  Pred<-predict(fit,as.matrix(X),s=0.03)
  my_list=list("fit"=fit,"Pred"=Pred)
  return(my_list)
}
