\name{RFactor_calc}

\alias{RFactor_calc}

\title{Determination the rainfall erosivity .}

\description{The function calculates the rainfall erosivity factor.}

\usage{RFactor_calc(data, erosive.precip = 10, equation = "WS")}

\arguments{
 \item{data}{Data to be analyzed.}
 \item{erosive.precip}{Precipitation considered erosive (default = 10).}
 \item{equation}{"WS" - Wischmeier and Smith (defaul),\cr
                 "BF" - Brown and Foster,\cr
                 "RUSLE2" - USDA-Agriculture Research Service.}
}

\value{
  \item{result}{Tabulated results.}
  \item{record}{Record of rainfall relative to 5, 10, 15, 30 or 60 minutes.}
}

\keyword{Erosivity index; R-Factor; Universal Soil Loss Equation-USLE}

\references{
Brown, L.C. and Foster, G.R. (1987). Storm erosivity using idealized intensity distributions. Trans. ASAE 30, 2, 379–386. <https:...>.

USDA-Agricultural Research Service. (2013). Science Documentation Revised Universal Soil Loss Equation Version 2. <https://www.ars.usda.gov/ARSUserFiles/60600505/RUSLE/RUSLE2_Science_Doc.pdf>.

Wischmeier, W. and Smith, D. (1978). Predicting rainfall erosion losses: a guide to conservation planning. Agricultural Handbook No. 537. U.S. Department of Agriculture, Washington DC, USA. <https:...>.
}

\author{
Dione Pereira Cardoso\cr

Paulo Cesar Ossani\cr

Junior Cesar Avanzi
}

\examples{
 data(Data_Rainfall_minutes)
 res <- RFactor_calc(Data_Rainfall_minutes, erosive.precip = 10, equation = "WS")
 res$result
}
