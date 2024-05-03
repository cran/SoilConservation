KFactor <- function(df_kfactor) {
  # esta funcao executa os calculos para estimar erodibilidade  
  # do solo, criada em 25/04/2024 por Paulo Cesar Ossani 
  # com acessoria de Dione Pereira Cardoso
  # Equação resolvida: K <- 0.1317 * (2.1 * M^1.14 * 10^-4 * (12 - SOM) + 3.25 * (s - 2) + 2.5 * (p - 3))/100
                            
  # KFactor = soil erodibility (Mg ha h ha^-1 MJ^-1 mm^-1);
  # M = particle-size parameter, with M = (silt + vfs) × (100 − clay);
  # silt = silt fraction content (0.002–0.05 mm) (%);
  # vfs = very fine sand fraction content (0.05–0.1 mm) (%);
  # clay = clay fraction content (< 0.002 mm) (%);
  # SOM = soil organic matter content (%);
  # s = soil structure; and
  # p = soil permeability;
  
  # Saida:
  # kfactor - resultados dos calculos
  
  if (ncol(df_kfactor) != 6)
     stop("'df_kfactor' input is incorrect, it should be have four columns in order: 
           silt (silt fraction content), 
           vfs (very fine sand fraction content),
           clay (clay fraction content),
           SOM (soil organic matter content (%)), 
           s (soil structure) and 
           p (soil permeability)")
  
  M <- (df_kfactor[,1] + df_kfactor[,2]) * (100 - df_kfactor[,3])
  SOM <- df_kfactor[,4]
  s <- df_kfactor[,5]
  p <- df_kfactor[,6]
  K <- 0.1317 * (2.1 * M^1.14 * 10^-4 * (12 - SOM) + 3.25 * (s - 2) + 2.5 * (p - 3))/100 
  
  kfactor <- cbind(df_kfactor[,1:3],M, df_kfactor[,4:6], K)
  
  col.names <- colnames(df_kfactor)
  
  colnames(kfactor) <- c(col.names[1:3],"M",col.names[4:6], "K factor (Mg ha h/ha MJ mm)")
  
  return(kfactor)
}

