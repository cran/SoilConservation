\name{SoilLoss}

\alias{SoilLoss}

\title{Estimation soil losses by water erosion.}

\description{The function estimates soil losses according to USLE and its revised versions (RUSLE).}

\usage{SoilLoss(df_SoilLoss)}

\arguments{
 \item{df_SoilLoss}{Data to be analyzed.}
}

\value{
  \item{result.A}{Tabulated results of the A (Soil loss).}
}

\keyword{Soil Loss; Water Erosion; R Factor; K Factor; LS Factor; C Factor and P Factor}

\references{
Wischmeier, W. and Smith, D. (1978). Predicting rainfall erosion losses: a guide to conservation planning. Agricultural Handbook No. 537. U.S. Department of Agriculture, Washington DC, USA. <https:...>.
}

\author{
Dione Pereira Cardoso\cr

Paulo Cesar Ossani\cr

Junior Cesar Avanzi
}

\examples{
data(Data_SoilLoss)
SoilLoss <- SoilLoss(Data_SoilLoss[,2:6])
round(SoilLoss,2) # result Soil loss
}
