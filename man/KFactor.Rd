\name{KFactor}

\alias{KFactor}

\title{Estimates soil erodibility.}

\description{The function estimates the soil erodibility factor.}

\usage{KFactor(df_kfactor)}

\arguments{
 \item{df_kfactor}{Data to be analyzed.}
}

\value{
  \item{kfactor}{Tabulated results of the k factor.}
}

\keyword{Nomograph; Particle-size; Organic matter; Soil-structure and permeability.}

\references{
Godoi, R. F.; Rodrigues, D. B.; Borrelli, P.; Oliveira, P. T. S. (2021). “High-resolution soil erodibility map of Brazil”. Science of The Total Environment, v. 781, p. 146673. <doi:10.1016/j.scitotenv.2021.146673>.

Wischmeier, W. and Smith, D. (1978). Predicting rainfall erosion losses: a guide to conservation planning. Agricultural Handbook No. 537. U.S. Department of Agriculture, Washington DC, USA. <https:...>
}

\author{
Dione Pereira Cardoso\cr

Paulo Cesar Ossani\cr

Junior Cesar Avanzi
}

\examples{
data(Data_Erodibility)
kfactor <- KFactor(Data_Erodibility)
round(kfactor,6) # result K factor
}
