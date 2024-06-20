
# ESCENARIO A

# Cargar libreria para guardar los resultados en Google Drive
library(googledrive)
drive_auth()

# Simular para obtener P(aaxaa, j)
# Aunque en este caso se sabe que es un estado absorbente, por lo que, P_ij = 0
# y P_ii = 1
set.seed(123)

num_AAxAA_escA_cruaaxaa <- 0
num_AAxAa_escA_cruaaxaa <- 0
num_AAxaa_escA_cruaaxaa <- 0
num_AaxAa_escA_cruaaxaa <- 0
num_Aaxaa_escA_cruaaxaa <- 0
num_aaxaa_escA_cruaaxaa <- 0


# 1º PASO: función camada

generacion_0 <- data.frame(id = rep(1:2, times = 3), 
                           linea = rep(c("linea1", "linea2", "linea3"), c(2,2,2)),
                           alelos = c("aa", "aa", "aa", "aa", "aa", "aa"), 
                           sexo = rep(c("Hembra", "Macho"), c(3))) 

# Separar en machos y hembras.
machos_gen0 <- subset(generacion_0, sexo == "Macho")
hembras_gen0 <- subset(generacion_0, sexo == "Hembra")

# Simulación: 1*10^7
for (z in 1:(1*10^7)){
  descendencia <- list()
  for (i in 1:3){
    # Obtener la descedencia a partir de los progenitores proporcionados
    descendencia[[paste0("descendencia_",i)]] <- camada(machos_gen0[i,], 
                                                        hembras_gen0[i,], 
                                                        prob_mut = 5.7*10^(-9)) 
    # Si el número de machos/hembras es 0, vuelve a cruzar a los progenitores
    while (sum(descendencia[[i]]$sexo == "Macho") == 0 | 
           sum(descendencia[[i]]$sexo == "Hembra") == 0){
      descendencia[[paste0("descendencia_",i)]] <- camada(machos_gen0[i,], 
                                                          hembras_gen0[i,], 
                                                          prob_mut = 5.7*10^(-9))
    }
  }
  
  descendencia_total <- data.frame() 
  descendencia_total <- do.call(rbind, descendencia) 
  
  # 2º PASO: función apareamiento
  ## A partir de la descendencia obtenida (función camada), determina los progenitores 
  ## progenitores de la siguiente generación.
  parentales <- NULL
  parentales <- apareamiento("Escenario A", descendencia_total)
  
  # 3º PASO: 
  ## En función de cómo sea el cruce, se le suma uno para contar el número de 
  ## cruces que se han producido tras la simulación.
  for (j in 1:3){
    if (parentales[["progenitor_macho"]][j,]$alelos == "AA" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "AA"){ 
      num_AAxAA_escA_cruaaxaa <- num_AAxAA_escA_cruaaxaa + 1 }
    
    # se pueden tener dos opciones: cruce AA x Aa o cruce Aa x AA
    if (parentales[["progenitor_macho"]][j,]$alelos == "AA" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "Aa" | 
        parentales[["progenitor_macho"]][j,]$alelos == "Aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "AA"){ 
      num_AAxAa_escA_cruaaxaa <- num_AAxAa_escA_cruaaxaa + 1 }
    
    # se pueden tener dos opciones: cruce AA x aa o cruce aa x AA
    if (parentales[["progenitor_macho"]][j,]$alelos == "AA" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "aa" | 
        parentales[["progenitor_macho"]][j,]$alelos == "aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "AA"){ 
      num_AAxaa_escA_cruaaxaa <- num_AAxaa_escA_cruaaxaa + 1 }
    
    if (parentales[["progenitor_macho"]][j,]$alelos == "Aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "Aa"){ 
      num_AaxAa_escA_cruaaxaa <- num_AaxAa_escA_cruaaxaa + 1 }
    
    # se pueden tener dos opciones: cruce Aa x aa o cruce aa x Aa
    if (parentales[["progenitor_macho"]][j,]$alelos == "Aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "aa" | 
        parentales[["progenitor_macho"]][j,]$alelos == "aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "Aa"){ 
      num_Aaxaa_escA_cruaaxaa <- num_Aaxaa_escA_cruaaxaa + 1 }
    
    if (parentales[["progenitor_macho"]][j,]$alelos == "aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "aa"){ 
      num_aaxaa_escA_cruaaxaa <- num_aaxaa_escA_cruaaxaa + 1 }
  }
  print(z)
}

# Crear banco de datos con la información resultante para guardarlo en forma de
# archivo .csv en Google Drive.
escA_cruaaxaa <- data.frame(num_AAxAA = num_AAxAA_escA_cruaaxaa, 
                            num_AAxAa = num_AAxAa_escA_cruaaxaa, 
                            num_AAxaa = num_AAxaa_escA_cruaaxaa,
                            num_AaxAa = num_AaxAa_escA_cruaaxaa, 
                            num_Aaxaa = num_Aaxaa_escA_cruaaxaa, 
                            num_aaxaa = num_aaxaa_escA_cruaaxaa, 
                            total = sum(num_AAxAA_escA_cruaaxaa, 
                                        num_AAxAa_escA_cruaaxaa,
                                        num_AAxaa_escA_cruaaxaa, 
                                        num_AaxAa_escA_cruaaxaa,
                                        num_Aaxaa_escA_cruaaxaa, 
                                        num_aaxaa_escA_cruaaxaa))

write.csv(escA_cruaaxaa, "datos_escA_cruaaxaa.csv", row.names = FALSE)

drive_upload("datos_escA_cruaaxaa.csv", 
             path = "datos_R_TFM/datos_escA_cruaaxaa.csv")
