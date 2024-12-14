RFactor_est <- function(data, latitude = NA, longitude = NA) {
  # Esta funcao estima a erosivida da chuva,
  # criada em 27/04/2024 por Paulo Cesar Ossani
  # com acessoria de Dione Pereira Cardoso
  
  # Entrada:
  # date   - dados para a analise;
  # latitude - latitude em grau decimal
  # longitude - longitude em grau decimal
  
  # Saida:
  # RFactor - erosividade da chuva estimada
    
  if (!is.data.frame(data)) 
     stop("'date' input is incorrect, it should be of type date frame or matrix. Verify!")
  
  if (!is.numeric(latitude)) 
     stop("'latitude' input is incorrect, it should be of type numeric. Verify!")
 
  if (!is.numeric(longitude)) 
     stop("'longitude' input is incorrect, it should be of type numeric. Verify!") 
  
  earth_dist <- function(latitude1, longitude1, latitude2, longitude2) {
    # formula Haversine - usada para encontrar a distancia entre dois pontos 
    # na terra com base nas latitudes e longitudes
    # varios codigos http://www.codecodex.com/wiki/Calculate_Distance_Between_Two_Points_on_a_Globe#Excel
    # https://en.wikipedia.org/wiki/Haversine_formula
    # http://ptcomputador.com/Software/microsoft-access/135119.html
    # https://qastack.com.br/gis/88484/performing-distance-calculation-using-excel
    
    r <- 6371  # raio aproximado da terra
    
    deg_rad <- pi / 180  
    
    rad_Lat <- deg_rad * (latitude2  - latitude1)   # transforma em radianos
    rad_Lon <- deg_rad * (longitude2 - longitude1)  # transforma em radianos
    
    a <- sin(rad_Lat / 2) * sin(rad_Lat / 2) + cos(deg_rad * latitude1) * cos(deg_rad * latitude2) * sin(rad_Lon / 2) * sin(rad_Lon / 2)  
    
    distance <- 2 * r * asin(sqrt(a))  
    
    return(distance)
    
  }
  
  ### Incio - encontra a equacao mais proxima usando a latitude/longetude ###
  # latitude e longitude das equacoes
  lati.equ <- c(-1.24,-4.09,-19.07,-19.22,-19.80,-18.67,-19.96,-18.87,-18.46,-20.27,-18.30,-22.12,-22.12,-16.05,-15.65,-16.03,-15.55,-13.55,-15.62,-15.84,-15.56,-16.45,-13.44,-12.29,-7.57,-8.29,-8.40,-8.00,-7.98,-8.32,-8.32,-28.55,-27.85,-28.65,-27.40,-22.52,-23.20,-21.28,-23.22)
  long.equ <- c(48.46,-63.14,-42.55,-42.49,-42.15,-43.08,-43.42,-42.97,-43.30,-54.32,-54.45,-54.56,-54.56,-57.68,-57.48,-57.27,-55.17,-52.26,-56.11,-54.39,-54.29,-54.57,-56.71,-55.29,-40.50,-35.98,-35.43,-35.18,-35.15,-37.72,-37.72,-53.90,-54.48,-56.00,-51.20,-47.04,-46.16,-47.01,-49.23)
  
  mdist <- NULL
  for(k in 1:length(lati.equ)) {
    mdist <- rbind(mdist, earth_dist(latitude, longitude, lati.equ[k], long.equ[k])) # distancia da cidade do acidente a estacoes
  }
  mo <- sort(mdist) # ordena as distancias em ordem crescente
  num.equation <- which(mdist %in% mo[1])[1] # encontra a posicao dos k vizinhos mais proximos
  ### Fim - encontra a equacao mais proxima usando a latitude/longetude ###

  col.mean <- colMeans(data)
  sum.mean <- sum(col.mean)
  MFI <- col.mean^2 / sum.mean
  
  if (num.equation == 1) {
    RFactor  <- 321.5 + 36.2 * MFI
    equation <- "321.5 + 36.2 * MFI"
  } else if (num.equation == 2) {
    RFactor  <- 67.355 * MFI^0.85
    equation <- "67.355 * MFI^0.85"
  } else if (num.equation == 3) {
    RFactor  <- 158.35 * MFI^0.85
    equation	<- "158.35 * MFI^0.85"
  } else if (num.equation == 4) {
    RFactor  <- 215.4 * MFI^0.65
    equation <- "215.4 * MFI^0.65"
  } else if (num.equation == 5) {
    RFactor  <- 321.63 * MFI^0.48
    equation <- "321.63 * MFI^0.48"
  } else if (num.equation == 6) {
    RFactor  <- 123.33 * MFI^0.74
    equation <- "123.33 * MFI^0.74"
  } else if (num.equation == 7) {
    RFactor  <- 170.59 * MFI^0.64
    equation <- "170.59 * MFI^0.64"
  } else if (num.equation == 8) {
    RFactor  <- 114.42 * MFI^0.81
    equation <- "114.42 * MFI^0.81"
  } else if (num.equation == 9) {
    RFactor  <- 179.33 * MFI^0.77
    equation <- "179.33 * MFI^0.77"
  } else if (num.equation == 10) {
    RFactor  <- 139.44 * MFI^0.6784
    equation <- "139.44 * MFI^0.6784"
  } else if (num.equation == 11) {
    RFactor  <- 138.33 * MFI^0.7431
    equation <- "138.33 * MFI^0.7431"
  } else if (num.equation == 12) {
    RFactor  <- 80.305 * MFI^0.8966
    equation <- "80.305 * MFI^0.8966"
  } else if (num.equation == 13) {
    RFactor  <- 67.355 * MFI^0.85
    equation <- "67.355 * MFI^0.85"
  } else if (num.equation == 14) {
    RFactor  <- 172.6326451 * MFI^0.5245258
    equation <- "172.6326451 * MFI^0.5245258"
  } else if (num.equation == 15) {
    RFactor  <- 56.115 * MFI^0.9504
    equation <- "56.115 * MFI^0.9504"
  } else if (num.equation == 16) {
    RFactor  <- 36.849 * MFI^1.0852
    equation <- "36.849 * MFI^1.0852"
  } else if (num.equation == 17) {
    RFactor  <- 0.6157 * MFI^1.7411
    equation <- "0.6157 * MFI^1.7411"
  } else if (num.equation == 18) {
    RFactor  <- 317.397829 * MFI^0.484654
    equation <- "317.397829 * MFI^0.484654"
  } else if (num.equation == 19) {
    RFactor  <- 109.412 * MFI^0.744
    equation <- "109.412 * MFI^0.744"
  } else if (num.equation == 20) {
    RFactor  <- 272.865645 * MFI^0.419164
    equation <- "272.865645 * MFI^0.419164"
  } else if (num.equation == 21) {
    RFactor  <- 0.3228 * MFI^1.9266
    equation <- "0.3228 * MFI^1.9266"
  } else if (num.equation == 22) {
    RFactor  <- 133.2004291 * MFI^0.5372499
    equation <- "133.2004291 * MFI^0.5372499"
  } else if (num.equation == 23) {
    RFactor  <- 147.262400 * MFI^0.533025
    equation <- "147.262400 * MFI^0.533025"
  } else if (num.equation == 24) {
    RFactor  <- 399.538719 * MFI^0.458718
    equation <- "399.538719 * MFI^0.458718"
  } else if (num.equation == 25) {
    RFactor  <- 95.48 * MFI^0.56
    equation <- "95.48 * MFI^0.56"
  } else if (num.equation == 26) {
    RFactor  <- 61.81 * MFI^0.58
    equation <- "61.81 * MFI^0.58"
  } else if (num.equation == 27) {
    RFactor  <- 57.32 * MFI^0.618
    equation <- "57.32 * MFI^0.618"
  } else if (num.equation == 28) {
    RFactor  <- 50.75 * MFI^0.724
    equation <- "50.75 * MFI^0.724"
  } else if (num.equation == 29) {
    RFactor  <- 69.24 * MFI^0.75
    equation <- "69.24 * MFI^0.75"
  } else if (num.equation == 30) {
    RFactor  <- 95.48 * MFI^0.56
    equation <- "95.48 * MFI^0.56"
  } else if (num.equation == 31) {
    RFactor  <- 182.86 + 56.21 * MFI
    equation <- "182.86 + 56.21 * MFI"
  } else if (num.equation == 32) {
    RFactor  <- 109.65 * MFI^0.76
    equation <- "109.65 * MFI^0.76"
  } else if (num.equation == 33) {
    RFactor  <- 118.52 * MFI^0.8034
    equation <- "118.52 * MFI^0.8034"
  } else if (num.equation == 34) {
    RFactor  <- 55.564 * MFI^1.1054
    equation <- "55.564 * MFI^1.1054"
  } else if (num.equation == 35) {
    RFactor  <- 59.265 * MFI^1.087
    equation <- "59.265 * MFI^1.087"
  } else if (num.equation == 36) {
    RFactor  <- 68.730 * MFI^0.841
    equation <- "68.730 * MFI^0.841"
  } else if (num.equation == 37) {
    RFactor  <- 67.355 * MFI^0.85
    equation <- "67.355 * MFI^0.85"
  } else if (num.equation == 38) {
    RFactor  <- 111.173 * MFI^0.691
    equation <- "111.173 * MFI^0.691"
  } else if (num.equation == 39) {
    RFactor  <- 72.5488 * MFI^0.8488
    equation <- "72.5488 * MFI^0.8488"
  }
  
  RFactor <- sum(RFactor)
  
  lista <- list(RFactor = RFactor, equation = equation)
  
  return(lista)
}


# lati1 <- - 1.24
# long1 <- 48.46
# equation1 <- 321.5 + 36.2 * MFI
# 
# lati2 <- - 4.09	
# long2 <- - 63.14	
# equation2 <- 67.355 * MFI^0.85
# 
# lati3 <- - 19.07	
# long3 <- - 42.55	
# equation3 <- 158.35 * MFI^0.85
# 
# lati4 <- - 19.22	
# long4 <- - 42.49	
# equation4 <- 215.4 * MFI^0.65
# 
# lati5 <- - 19.80	
# long5 <- - 42.15	
# equation5 <- 321.63 * MFI^0.48
# 
# lati6 <- - 18.67	
# long6 <- - 43.08	
# equation6 <- 123.33 * MFI^0.74
# 
# lati7 <- - 19.96	
# long7 <- - 43.42	
# equation7 <- 170.59 * MFI^0.64
# 
# lati8 <- - 18.87	
# long8 <- - 42.97	
# equation8 <- 114.42 * MFI^0.81
# 
# lati9 <- - 18.46	
# long9 <- - 43.30	
# equation9 <- 179.33 * MFI^0.77
# 
# lati10 <- - 20.27	
# long10 <- - 54.32	
# equation10 <- 139.44 * MFI^0.6784
# 
# lati11 <- - 18.30	
# long11 <- - 54.45	
# equation11 <- 138.33 * MFI^0.7431
# 
# lati12 <- - 22.12	
# long12 <- - 54.56	
# equation12 <- 80.305 * MFI^0.8966
# 
# lati13 <- - 22.12	
# long13 <- - 54.56	
# equation13 <- 67.355 * MFI^0.85
# 
# lati14 <- - 16.05	
# long14 <- - 57.68	
# equation14 <- 172.6326451 * MFI^0.5245258
# 
# lati15 <- - 15.65	
# long15 <- - 57.48	
# equation15 <- 56.115 * MFI^0.9504
# 
# lati16 <- - 16.03	
# long16 <- - 57.27	
# equation16 <- 36.849 * MFI^1.0852
# 
# lati17 <- - 15.55	
# long17 <- - 55.17	
# equation17 <- 0.6157 * MFI^1.7411
# 
# lati18 <- - 13.55	
# long18 <- - 52.26	
# equation18 <- 317.397829 * MFI^0.484654
# 
# lati19 <- - 15.62	
# long19 <- - 56.11	
# equation19 <- 109.412 * MFI^0.744
# 
# lati20 <- - 15.84	
# long20 <- - 54.39	
# equation20 <- 272.865645 * MFI^0.419164
# 
# lati21 <- - 15.56	
# long21 <- - 54.29	
# equation21 <- 0.3228 * MFI^1.9266
# 
# lati22 <- - 16.45	
# long22 <- - 54.57	
# equation22 <- 133.2004291 * MFI^0.5372499
# 
# lati23 <- - 13.44	
# long23 <- - 56.71	
# equation23 <- 147.262400 * MFI^0.533025
# 
# lati24 <- - 11.85	#############
# long24 <- - 55.65	
# equation24 <- 399.538719 * MFI^0.458718
# 
# lati25 <- - 12.29	
# long25 <- - 55.29	
# equation25 <- 399.538719 * MFI^0.458718
# 
# lati26 <- - 7.57	
# long26 <- - 40.50	
# equation26 <- 95.48 * MFI^0.56
# 
# lati27 <- - 8.29	
# long27 <- - 35.98	
# equation27 <- 61.81 * MFI^0.58
# 
# lati28 <- - 8.40	
# long28 <- - 35.43	
# equation28 <- 57.32 * MFI^0.618
# 
# lati29 <- - 8.00	
# long29 <- - 35.18	
# equation29 <- 50.75 * MFI^0.724
# 
# lati30 <- - 7.98	
# long30 <- - 35.15	
# equation30 <- 69.24 * MFI^0.75
# 
# lati31 <- - 8.32	
# long31 <- - 37.72	
# equation31 <- 95.48 * MFI^0.56
# 
# lati32 <- - 8.32	
# long32 <- - 37.72	
# equation32 <- 182.86 + 56.21 * MFI
# 
# lati33 <- - 28.55	
# long33 <- - 53.90	
# equation33 <- 109.65 * MFI^0.76
# 
# lati34 <- - 27.85	
# long34 <- - 54.48	
# equation34 <- 118.52 * MFI^0.8034
# 
# lati35 <- - 28.65	
# long35 <- - 56.00	
# equation35 <- 55.564 * MFI^1.1054
# 
# lati36 <- - 27.40	
# long36 <- - 51.20	
# equation36 <- 59.265 * MFI^1.087
# 
# lati37 <- - 22.52	
# long37 <- - 47.04	
# equation37 <- 68.730 * MFI^0.841
# 
# lati38 <- - 23.20	
# long38 <- - 46.16	
# equation38 <- 67.355 * MFI^0.85
# 
# lati39 <- - 21.28	
# long39 <- - 47.01	
# equation39 <- 111.173 * MFI^0.691
# 
# lati40 <- - 23.22	
# long40 <- - 49.23	
# equation40 <- 72.5488 * MFI^0.8488

