SoilLoss <- function(df_SoilLoss) {
  # esta funcao executa os calculos da perda de solo,
  # criada em 25/04/2024 por Paulo Cesar Ossani 
  # com acessoria de Dione Pereira Cardoso
  # Equação resolvida: A <- R * K * LS * C * P
  
  # Entrada:
  # USLE Factors:
  # A = Soil loss (Mg ha^-1 year^-1);
  # R = rainfall erosivity (MJ mm ha^-1 h^-1 year^-1);
  # K = soil erodibility (Mg ha h ha^-1 MJ^-1 mm^-1);
  # L and S = slope length and steepness
  # C = land-use and management; and
  # P = conservation practices.
  
  # Saida:
  # result - resultados dos calculos

  if (ncol(df_SoilLoss) != 5)
    stop("'df_SoilLoss' input is incorrect, it should be have five columns: 
           R (rainfall erosivity), K (soil erodibility), L and S (slope length and steepness),
           C (land-use and management) and P (conservation practices).")
  
  result.A <- 1
  for(i in 1:ncol(df_SoilLoss))
    result.A <- result.A * df_SoilLoss[,i]
  
  result.A <- cbind(df_SoilLoss, result.A)
  colnames(result.A) <- c(colnames(df_SoilLoss), "Soil loss")
  
  return(result.A)
}

