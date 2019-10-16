\name{MTGS.mrce}
\alias{MTGS.mrce}
\title{Genomic Selection using Multivariate Regression with Covariance
Estimation (MRCE)
}
\description{
Calculates the Genomic Estimated Breeding Value based on MRCE method.
}
\usage{MTGS.mrce(X, Y, r)
}

\arguments{
  \item{X}{
X is a design matrix of marker genotype of size n×p, where n are no of            Individuals under study (i.e. genotype, lines) and p are no of markers.
}
\item{Y}{Y is matrix of individuals (n) with their phenotypic traits (q) of size n×q.}
\item{r}{r is fraction of testing data (ranges from (0-1)) used during model fitting(suppose if one want to use 75\% of data for model training and
         remaining 25\% for model testing so one has to define r=0.25).}
}

\value{


$Bhat 	      Estimate of marker effects for different phenotypic traits(q).

$muhat 	      Estimate of intercept for different phenotypic traits(q).

$Pred 	      GEBVs for genotype under study.


}

\references{


Rothman, A.J., Levina, E., and Zhu, J. 2010. Sparse multivariate regression with covariance estimation. J. Comput. Graph. Stat. 19, 947-962.

Adam J. Rothman (2017). MRCE: Multivariate Regression with Covariance Estimation. R package version 2.1. https://CRAN.R-project.org/package=MRCE.

}

\examples{

library(MTGS)
data(brassica_data)
X<-brassica_data[,1:100]
Y<-brassica_data[,101:103]
r<-0.25
MTGS.mrce(X,Y,r)

}
