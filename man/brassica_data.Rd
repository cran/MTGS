\docType{data}
    \name{brassica_data}
    \alias{brassica_data}
    \title{Genotypic and phenotypic dataset for brassica napus}
    \format{A data frame with 50 rows as genotypes with 103 columns (i.e. contains information of genotyped markers and phenotypic traits).}
    \details{
    In brassica_data, it has total 50 rows which represents particular genotypes and a total of 103 of columns, in which first 100 columns contain information of genotyped markers and remaining three columns represent value of phenotypic traits associated with genotype under study.

    }
    \source{
      Kole C, Thorman C, Karlsson B, Palta J, Gaffney P, Yandell B, Osborn T (2002) Comparative mapping of loci controlling winter survival and related traits in oilseed brassica rapa and B. napus. Molecular Breeding. 1: 201–210.
    }
    \description{
     Dataset used in this study is related to Brassica napus (Kole et al, 2002). In our sample dataset it has 3 response for 50 lines (individuals) which are genotyped for 100 markers. Lines are derived from two cultivars namely Stellar and Major. Marker genotypes are represented in 0/1, where 0 represents a Stellar allele and 1 represents Major allele. Response are related to days of flowering at different weeks of vernlization (flower0, flower4, flower8).

    }

    \usage{data("brassica_data")}

\examples{
library(MTGS)
data(brassica_data)
X<-brassica_data[,1:100]   #### Extracting Genotype
Y<-brassica_data[,101:103] #### Extracting Phenotype

}
