RFactor_calc <- function(data, erosive.precip = 10, equation = "WS") {
  # Esta funcao calcula a erosividade das chuvas,
  # criada em 25/04/2024 por Paulo Cesar Ossani
  # com acessoria de Dione Pereira Cardoso
  
  # https://www.quora.com/What-is-the-difference-between-rain-and-rainfall
  
  # Entrada:
  # date   - dados para a analise;
  # erosive.precip - precipitacao considerada erosiva (default = 10);
  # equation - BF (Brown and Foster, 1987), RUSLE2 (USDA-Agriculture Research Service, 2013),
  #            WS (Wischmeier and Smith, 1978) (default)

  # Retorna:
  # result - resultados tabelados
  # record - registro de precipitação relativa: 5, 10, 15, 30 or 60  minutos
  
  if (!is.data.frame(data))
     stop("'date' input is incorrect, it should be of type date frame or matrix. Verify!")

  if (class(data)[1] %in% c("tbl_df","tbl"))
     data <- as.data.frame(data)
  
  # if (ncol(data) != 3 || data# if (ncol(data) != 3 || !(!is.na(as.Date(as.character(data[1,1]),"%d/%m/%Y")) && !is.na(strptime(data[1,2],"%H:%M")) && is.numeric(data[1,3]))) 
  if (ncol(data) != 3 || !(!is.na(as.Date(as.character(format(data[1,1], format = "%d/%m/%Y")),"%d/%m/%Y")) && 
                           !is.na(strptime(format(data[1,2], format = "%H:%M"),"%H:%M")) && is.numeric(data[1,3]))) 
     stop("'date' input is incorrect, it should be have three columns in order: date (dd/mm/yyy), times (hh:mm) and precipitation. Verify!")
  
  equation <- toupper(equation) # transforma em maiusculo
  if (!(equation %in% c("BF", "RUSLE2", "WS")))
     stop("'equation' input is incorrect, it should be: 'BF', 'RUSLE2' or 'WS'. Verify!")
  
  time <- strptime(format(data[1:2,2], format = "%H:%M"),"%H:%M") 
  record <- as.numeric(time[2]-time[1]) # registro de precipitacao relativa a 5, 10, 15 ou 30 minutos
  if(!(record %in% c(5, 10, 15, 30, 60)))
    stop("The precipitation record should be 5, 10, 15, 30 or 60 minutes. Check the database!")
  
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end!")
  
  data[,1] <- format(data[,1], format = "%d/%m/%Y")

  num.events <- 360 / record # numero de eventos a serem processados por periodo de chuva
  ncol       <- ncol(data)
  
  result  <- matrix(NA, nrow = 0, ncol = 9) # resultados finais
  rfactor <- 0 # fator R

  names.year <- unique(format(as.Date(data[,1],"%d/%m/%Y"),"%Y")) # numero de anos
  med.EI30   <- matrix(NA, nrow = 12, ncol = length(names.year))  # valores de EI30 para calculo da media mensal
  for(i in 1:length(names.year)) { # calculo por anos
    # i=1
    dat1  <- data[format(as.Date(data[,1],"%d/%m/%Y"),"%Y") == names.year[i],] # filta os dados por ano
    
    month <- format(as.Date(dat1[,1]),"%m")
    rain  <- tapply(dat1[,ncol(dat1)], INDEX = month, FUN = sum) # soma das precipitacoes dentro de cada mes

    meses     <- rownames(rain) # nomes dos meses processados
    num.meses <- length(meses)
    num.rain  <- rep(0,num.meses) # numero de chuvas no mes
    sum.EI30  <- rep(0,num.meses) # soma dos produtos da energia cinetica x I30 nos meses
    num.rain.erosive <- rep(0,num.meses) # numero de chuvas erosivas nos meses
    sum.erosive.rain <- rep(0,num.meses) # soma das chuvas erosivas (mm) nos meses
    for(m in 1:num.meses) { # calulos por meses
      # m=1
      dat2 <- dat1[format(as.Date(dat1[,1]),"%m") == meses[m],] # filta os dados por mes
      nlin <- nrow(dat2)
      
      precip.mes <- dat2[, ncol] # preciptacao no mes
      unique(precip.mes)
      ## inicio - encontra as chuvas no mes ##
      vector.rain <- rep(1,nlin) # vetor que localiza as ocorrencias de chuvas
      k <- 0 # contador de intervalos de tempos
      n <- 1 # numero de chuvas
      h <- ifelse(sum(precip.mes[1:num.events]) != 0, 1, 0)
      for(j in 1:nlin) {
        if (precip.mes[j] == 0) {
           k <- k + 1
        } else {
          if (k >= num.events && precip.mes[j] != 0) {
             vector.rain[(j-k):j] <- 0
             if (h == 1) n <- n + 1
             vector.rain[j:nlin] <- n
             h <- 1
          }
          k <- 0
        }
      }
      if (k >= num.events) vector.rain[(j-k+1):nlin] <- 0

      max.rain    <- max(vector.rain) # numero maximo de chuvas no mes
      num.rain[m] <- max.rain # numero de chuvas no mes
      ## fim - encontra as chuvas no mes ##
      
      sum.prec <- rep(0,max.rain) # soma das precipitcao nos meses
      sum.Ech  <- rep(0,max.rain) # soma das energia cinetica em mega jaule (MJ/ha) nos meses
      I30      <- rep(0,max.rain) # intensidades (mm/h) maximas em 30 minutos nos meses
      EI30     <- rep(0,max.rain) # produto da energia cinetica x I30 nos meses
      
      if(rain[m] > 0) {
        
        ## inicio - calculo auxiliares ##
        Int <- precip.mes * 6  # intensidade da chuva I (mm/h)
        if (equation == "WS") Ec <- 0.119 + 0.0873 * log10(Int) # energia cinetica unitaria (MJ/ha mm)
        if (equation == "BF") Ec <- 0.29 * (1 - 0.72 * exp(-0.05*Int)) # energia cinetica unitaria (MJ/ha mm)
        if (equation == "RUSLE2")  Ec <- 0.29 * (1 - 0.72 * exp(-0.082*Int)) # energia cinetica unitaria (MJ/ha mm)
        Ech <- precip.mes * Ec # Energia cinetica em mega jaule (MJ/ha)
        ## fim - calculo auxiliares ##
        
        for(t in 1:max.rain) { 
           # t=1
           precip <- precip.mes[vector.rain == t]
 
           sum.prec[t] <- sum(precip) # soma das precipitacoes nos meses
           sum.Ech[t]  <- sum(na.omit(Ech[vector.rain == t]))   # soma das energia cinetica em mega jaule (MJ/ha) nos meses
           
           nevents <- length(precip)
           num.event.use <- 30 / record
           num.event.use <- ifelse(nevents <= num.event.use, nevents, num.event.use)
           P30 <- rep(0, nevents - num.event.use) # precipitacao (mm) em 30 minutos nos meses
           for(w in (nevents - num.event.use + 1):1) 
              P30[w] <- sum(precip[w:(w+num.event.use-1)])
           
           I30[t] <- max(P30 * 2) # intensidades (mm/h) maximas em 30 minutos nos meses

        }
        EI30 <- sum.Ech * I30 # produto da energia cinetica x I30
      }
      
      sum.EI30[m] <- sum(EI30[sum.prec >= erosive.precip | sum.Ech >= 3.6 | I30 >= 24]) # soma dos produtos da energia cinetica x I30 nos meses
      num.rain.erosive[m] <- length(EI30[sum.prec >= 10 | sum.Ech >= 3.6 | I30 >= 24]) # numero de chuvas erosivas nos meses
      sum.erosive.rain[m] <- sum(sum.prec[sum.prec >= 10 | sum.Ech >= 3.6 | I30 >= 24]) # soma das chuvas erosivas (mm) nos meses

    }
   
    med.EI30[as.numeric(meses),i] <- sum.EI30 # valores de EI30 para calculo da media mensal

    tab <- cbind(as.data.frame(rep(names.year[i],num.meses)), meses, 
                 round(rain,2), round(sum.EI30,2), num.rain, num.rain.erosive, 
                 num.rain - num.rain.erosive, round(sum.erosive.rain,2), 
                 round(rain - sum.erosive.rain,2))
    
    result <- rbind(result, tab)
    
    result <- rbind(result, c("Total","of year", colSums(tab[,3:ncol(tab)])))

    rfactor <- rfactor + sum(sum.EI30)
  } 

  res.rfactor <- rfactor/length(names.year)
  
  result <- rbind(result, c("R Factor",round(res.rfactor,2), "MJ mm/ha h year", rep("",6)))
  
  if(ncol(med.EI30) > 1) {
    mean.cols <- sum(rowMeans(med.EI30, na.rm = T))
  } else mean.cols <- res.rfactor 
    
  result <- rbind(result, c("Mean R Factor",round(mean.cols,2), "MJ mm/ha h year", rep("",6)))
  colnames(result) <- c("Year","Month","Rainfall(mm)","EI30","N. rainfall","N. rainfall erosive",
                        "N. rainfall non erosive","Erosive rainfall(mm)","Non erosive rainfall(mm)")
  
  rownames(result) <- NULL
  
  message("\n \n End!")
  
  lista <- list(result = result, record = record)
  
  return(lista)
}