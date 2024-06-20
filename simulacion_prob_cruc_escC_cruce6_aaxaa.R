# ESCENARIO C

library(googledrive)
drive_auth()

set.seed(123)

# Simular para obtener P(aaxaa, j)
# Aunque en este caso se sabe que es un estado absorbente, por lo que, P_ij = 0
# y P_ii = 1
num_AAxAA_escC_cruaaxaa <- 0
num_AAxAa_escC_cruaaxaa <- 0
num_AAxaa_escC_cruaaxaa <- 0
num_AaxAa_escC_cruaaxaa <- 0
num_Aaxaa_escC_cruaaxaa <- 0
num_aaxaa_escC_cruaaxaa <- 0

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
  }
  
  descendencia_total <- data.frame() 
  descendencia_total <- do.call(rbind, descendencia) 
  
  # OJO!!!!!!! ESTO TENDRIA QUE VER SI SE CORTA O NO
  while (min(sum(subset(descendencia_total, linea == "linea1")$sexo == "Macho"),
             sum(subset(descendencia_total, linea == "linea1")$sexo == "Hembra"))
         + min(sum(subset(descendencia_total, linea == "linea2")$sexo == "Macho"), 
               sum(subset(descendencia_total, linea == "linea2")$sexo == "Hembra"))
         + min(sum(subset(descendencia_total, linea == "linea3")$sexo == "Macho"),
               sum(subset(descendencia_total, linea == "linea3")$sexo == "Hembra")) 
         < 3){
    descendencia <- list()
    for (i in 1:3){
      descendencia[[paste0("descendencia_",i)]] <- 
        camada(parentales[["progenitor_macho"]][i,], 
               parentales[["progenitor_hembra"]][i,], prob_mut = 5.7*10^(-9))
    }
    descendencia_total <- data.frame() 
    descendencia_total <- do.call(rbind, descendencia)
  }  
  
  # 2º PASO: función apareamiento
  ## A partir de la descendencia obtenida (camada), determina los progenitores 
  ## de la siguiente generación.
  if (min(sum(subset(descendencia_total, linea == "linea1")$sexo == "Macho"), 
          sum(subset(descendencia_total, linea == "linea1")$sexo == "Hembra")) 
      == 1 & 
      min(sum(subset(descendencia_total, linea == "linea2")$sexo == "Macho"), 
          sum(subset(descendencia_total, linea == "linea2")$sexo == "Hembra")) 
      ==  1 & 
      min(sum(subset(descendencia_total, linea == "linea3")$sexo == "Macho"), 
          sum(subset(descendencia_total, linea == "linea3")$sexo == "Hembra")) 
      == 1){
    parentales <- NULL
    parentales <- apareamiento("Escenario A", descendencia_total)
  } else {
    parentales <- NULL
    parentales <- apareamiento("Escenario C", descendencia_total)
  }
  
  # 3º PASO: 
  ## En función de cómo sea el cruce, se le suma uno para contar el número de 
  ## cruces que se han producido tras la simulación.
  for (j in 1:3){
    if (parentales[["progenitor_macho"]][j,]$alelos == "AA" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "AA"){ 
      num_AAxAA_escC_cruaaxaa <- num_AAxAA_escC_cruaaxaa + 1 }
    
    # se pueden tener dos opciones: cruce AA x Aa o cruce Aa x AA
    if (parentales[["progenitor_macho"]][j,]$alelos == "AA" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "Aa" | 
        parentales[["progenitor_macho"]][j,]$alelos == "Aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "AA"){ 
      num_AAxAa_escC_cruaaxaa <- num_AAxAa_escC_cruaaxaa + 1 }
    
    # se pueden tener dos opciones: cruce AA x aa o cruce aa x AA
    if (parentales[["progenitor_macho"]][j,]$alelos == "AA" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "aa" | 
        parentales[["progenitor_macho"]][j,]$alelos == "aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "AA"){ 
      num_AAxaa_escC_cruaaxaa <- num_AAxaa_escC_cruaaxaa + 1 }
    
    if (parentales[["progenitor_macho"]][j,]$alelos == "Aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "Aa"){ 
      num_AaxAa_escC_cruaaxaa <- num_AaxAa_escC_cruaaxaa + 1 }
    
    # se pueden tener dos opciones: cruce Aa x aa o cruce aa x Aa
    if (parentales[["progenitor_macho"]][j,]$alelos == "Aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "aa" | 
        parentales[["progenitor_macho"]][j,]$alelos == "aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "Aa"){ 
      num_Aaxaa_escC_cruaaxaa <- num_Aaxaa_escC_cruaaxaa + 1 }
    
    if (parentales[["progenitor_macho"]][j,]$alelos == "aa" & 
        parentales[["progenitor_hembra"]][j,]$alelos == "aa"){ 
      num_aaxaa_escC_cruaaxaa <- num_aaxaa_escC_cruaaxaa + 1 }
  }
  print(z)
}


# crear banco de datos con la información para que me lo guarde en forma de excell
escC_cruaaxaa <- data.frame(num_AAxAA = num_AAxAA_escC_cruaaxaa, 
                            num_AAxAa = num_AAxAa_escC_cruaaxaa, 
                            num_AAxaa = num_AAxaa_escC_cruaaxaa, 
                            num_AaxAa = num_AaxAa_escC_cruaaxaa, 
                            num_Aaxaa = num_Aaxaa_escC_cruaaxaa, 
                            num_aaxaa = num_aaxaa_escC_cruaaxaa, 
                            total = sum(num_AAxAA_escC_cruaaxaa, 
                                        num_AAxAa_escC_cruaaxaa, 
                                        num_AAxaa_escC_cruaaxaa, 
                                        num_AaxAa_escC_cruaaxaa, 
                                        num_Aaxaa_escC_cruaaxaa, 
                                        num_aaxaa_escC_cruaaxaa))

write.csv(escC_cruaaxaa, "datos_escC_cruaaxaa.csv", row.names = FALSE)

drive_upload("datos_escC_cruaaxaa.csv", 
             path = "datos_R_TFM/datos_escC_cruaaxaa.csv")
