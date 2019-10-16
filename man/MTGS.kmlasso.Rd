\name{MTGS.kmlasso}
\alias{MTGS.kmlasso}
\title{Genomic Selection using Kernelized Multivariate LASSO (KMLASSO)
}
\description{
Calculates the Genomic Estimated Breeding Value based on KMLASSO method.
}
\usage{MTGS.kmlasso(X, Y)
}

\arguments{
  \item{X}{
X is a design matrix of marker genotype of size n×p, where n are no of            Individuals under study (i.e. genotype, lines) and p are no of markers.
}
\item{Y}{Y is matrix of individuals (n) with their phenotypic traits (q) of size n×q.}
}

\value{


$fit    Lists various coeffecient assocaited to LASSO model fitting.

$Pred   GEBV's for genotype/individuals under study.


}
\references{

Tibshirani, R. 1996. Regression shrinkage and selection via the lasso. J. R. Stat. Soc. Series B (Methodological). 267–288.

Searle, S.R., G. Casella and C.E. McCulloch. 1992. Variance Components. John Wiley, Hoboken.

Friedman, J., Hastie, T. and Tibshirani, R. (2008) Regularization Paths for Generalized Linear Models via Coordinate Descent, https://web.stanford.edu/~hastie/Papers/glmnet.pdf Journal of Statistical Software, Vol. 33(1), 1-22 Feb 2010 http://www.jstatsoft.org/v33/i01/.

Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. URL http://www.jstatsoft.org/v33/i01/.

}


\examples{

library(MTGS)
data(brassica_data)
X<-brassica_data[,1:100]
Y<-brassica_data[,101:103]
#r<-0.25
MTGS.kmlasso(X,Y)

}
