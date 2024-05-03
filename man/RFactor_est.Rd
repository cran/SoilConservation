\name{RFactor_est}

\alias{RFactor_est}

\title{Estimation the rainfall erosivity.}

\description{The function estimates the rainfall erosivity factor.}

\usage{RFactor_est(data, latitude, longitude)}

\arguments{
 \item{data}{Data to be analyzed.}
 \item{latitude}{Latitude}
 \item{longitude}{Longitude}
}

\value{
  \item{RFactor}{Estimated rainfall erosivity.}
  \item{equation}{Equation used for estimations.}
}

\keyword{Erosivity index; R-Factor; Universal Soil Loss Equation-USLE}

\references{
Arnoldus H. M. J. (1980). An approximation of the rainfall factor in the universal soil loss equation. In: De Boodt M, Gabriels D (eds) Assessment of erosion. JohnWiley & Sons, Chichister, pp 127–132. <https:...>.

Oliveira, P. T. S. and Wendland, E.; Nearing, M. A. (2013). “Rainfall erosivity in Brazil: A review”. Catena, v. 100, p. 139-147, 2013. <doi:10.1016/j.catena.2012.08.006>.
}

\author{
Dione Pereira Cardoso\cr

Paulo Cesar Ossani\cr

Junior Cesar Avanzi
}

\examples{
data(Data_Rainfall_month)
rfactor <- RFactor_est(Data_Rainfall_month[,2:13], 
                       latitude = -12.01527777, 
                       longitude = -48.544444440)
rfactor$RFactor
rfactor$equation
}
