
# ------------
# RESULTADOS
# ------------

# ---------------------------------- ESCENARIO D ------------------------------
set.seed(123)

# Se obtiene la primera generacion (generación 0):
## Se va a crear un banco de datos con 6 crías, 3 machos y 3 hembras y 3 líneas.
generacion_0 <- data.frame(id = rep(1:2, times = 3), 
                           linea = rep(c("linea1", "linea2", "linea3"), c(2,2,2)),
                           alelos = c("AA", "AA", "AA", "AA", "AA", "AA"), 
                           sexo = rep(c("Hembra", "Macho"), c(3))) 

# Agrupar el banco de datos en función de machos y hembras
machos_gen0 <- subset(generacion_0, sexo == "Macho")
hembras_gen0 <- subset(generacion_0, sexo == "Hembra")

descendencia <- list()
for (i in 1:3){
  descendencia[[paste0("descendencia_",i)]] <- 
    camada(machos_gen0[i,], hembras_gen0[i,], prob_mut = 5.7*10^(-9)) 
}

# do-call: ejecuta una función a la lista de argumentos especificada.
# rbind: combina secuencias de vectores/matrices por filas.
descendencia_total <- data.frame() 
descendencia_total <- do.call(rbind, descendencia) 

# Mientras el número total de hembras o machos sea menor de 3, vuelve a cruzar  
# los progenitores.
while(min(sum(descendencia_total$sexo == "Macho"), 
          sum(descendencia_total$sexo == "Hembra")) < 3){
  descendencia <- list()
  for (i in 1:3){
    descendencia[[paste0("descendencia_",i)]] <- 
      camada(machos_gen0[i,], hembras_gen0[i,], prob_mut = 5.7*10^(-9)) 
  }
  
  descendencia_total <- data.frame() 
  descendencia_total <- do.call(rbind, descendencia) 
}

parentales <- apareamiento_D("Escenario D", descendencia_total)

# Simulación: repetir el proceso 5000 veces y quesarse con el valor medio 
num_AA <- NULL; num_AA_mean <- NULL 
num_Aa <- NULL; num_Aa_mean <- NULL
num_aa <- NULL; num_aa_mean <- NULL

n <- 50

for (z in 1:50){
  for (j in 1:n){
    descendencia <- list()
    
    for (i in 1:3){
      descendencia[[paste0("descendencia_",i)]] <- 
        camada(parentales[["progenitor_macho"]][i,], 
               parentales[["progenitor_hembra"]][i,], prob_mut = 5.7*10^(-9))
    }
    
    descendencia_total <- data.frame() 
    descendencia_total <- do.call(rbind, descendencia)
    
    # Mientras el número total de hembras o machos sea menor de 3, vuelve a   
    # cruzar los progenitores.
    while(min(sum(descendencia_total$sexo == "Macho"), 
              sum(descendencia_total$sexo == "Hembra")) < 3){
      descendencia <- list()
      for (i in 1:3){
        descendencia[[paste0("descendencia_",i)]] <- 
          camada(machos_gen0[i,], hembras_gen0[i,], prob_mut = 5.7*10^(-9)) 
      }
      
      
      descendencia_total <- data.frame() 
      descendencia_total <- do.call(rbind, descendencia) 
    }
    
    # rbind: combina secuencias de vectores/matrices por filas.
    num_AA[j] <- sum(descendencia_total$alelos == "AA")
    num_Aa[j] <- sum(descendencia_total$alelos == "Aa")
    num_aa[j] <- sum(descendencia_total$alelos == "aa")
    
    parentales <- apareamiento_D("Escenario D", descendencia_total)
  }
  
  # Aplica a cada una de las columnas de la matriz la media
  num_AA_mean <- rbind(num_AA_mean, num_AA)
  num_Aa_mean <- rbind(num_Aa_mean, num_Aa)
  num_aa_mean <- rbind(num_aa_mean, num_aa)
}

num_AA_mean_n50_esD <- round(apply(num_AA_mean, 2, mean))
num_Aa_mean_n50_esD <- round(apply(num_Aa_mean, 2, mean))
num_aa_mean_n50_esD <- round(apply(num_aa_mean, 2, mean))

