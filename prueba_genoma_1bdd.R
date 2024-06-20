# --------------- UN ÚNICO BANCO DE DATOS CON TODO EL GENOMA ------------------

# La función apareamiento es la misma que hay en el script funciones.R
camada_mod <- function(parental_1, parental_2, prob_mut){
  descendencia <- NULL
  
  # 1º paso: crear banco de datos con la información de las crías:
  # id, linea y sexo  
  ## Van a haber 18 crías en total, 6 crías por línea.
  generacion_1 <- data.frame(id = c(1:6), 
                             linea = rep(paste0("linea", i), 6), 
                             sexo = sample(c("Hembra","Macho"), 6,
                                    prob = c(0.5,0.5), replace=TRUE))
  
  # No va a hacer falta especificarle el tamaño del genoma, porque este se va a 
  # saber por el tamaño del banco de datos de la generacion_0
  ## Se especifica que vaya desde 4 hasta length(parental_1)+3, porque se sabe 
  ## que las 3 primeras columnas del banco de datos son información de la cría: 
  ## id, linea, sexo y a partir de estas se encuentran los nucleótidos.
  ### De esta forma se consigue que la función camada pase por todos los 
  ### nucleótidos.
  for (j in 4:length(parental_1)){ 
    if (parental_1[,j] == "AA" & parental_2[,j] == "AA"){
      
      # Vector con probabilidad de: AA, Aa o aa, en función del cruce y si muta.
      prob_cruce_AAxAA <- c((1-prob_mut)*(1-prob_mut), 2*prob_mut*(1-prob_mut), 
                            prob_mut^2)
      # Multinomial con 3 niveles: AA, Aa y aa.
      descendencia <- rmultinom(1, 6, prob_cruce_AAxAA)
    }
    
    # Se pueden tener dos opciones: cruce AA x Aa o cruce Aa x AA.
    if (parental_1[,j] == "AA" & parental_2[,j] == "Aa" | 
        parental_1[,j] == "Aa" & parental_2[,j] == "AA"){
      
      prob_cruce_AAxAa <- c(1/2*(1 - prob_mut)*(1 - prob_mut), 
                            1/2*(1 - prob_mut)*(2*prob_mut + 1), 
                            1/2*prob_mut*(prob_mut + 1))
      
      descendencia <- rmultinom(1, 6, prob_cruce_AAxAa)
    }
    
    # Se pueden tener dos opciones: cruce AA x aa o cruce aa x AA.
    if (parental_1[,j] == "AA" & parental_2[,j] == "aa" | 
        parental_1[,j] == "aa" & parental_2[,j] == "AA"){
      
      prob_cruce_AAxaa <- c(0, 1 - prob_mut, prob_mut)
      
      descendencia <- rmultinom(1, 6, prob_cruce_AAxaa)
    }
    
    if (parental_1[,j] == "Aa" & parental_2[,j] == "Aa"){
      
      prob_cruce_AaxAa <- c(1/4*(1 - prob_mut)*(1 - prob_mut),
                            1/4*2*prob_mut*(1 - prob_mut) + 1/2*(1 - prob_mut), 
                            1/4*(prob_mut)^2 + 1/2*prob_mut + 1/4)
      
      descendencia <- rmultinom(1, 6, prob_cruce_AaxAa)
    }
    
    # Se pueden tener dos opciones: cruce Aa x aa o cruce aa x Aa.
    if (parental_1[,j] == "Aa" & parental_2[,j] == "aa" | 
        parental_1[,j] == "aa" & parental_2[,j] == "Aa"){
      
      prob_cruce_Aaxaa <- c(0, 1/2*(1 - prob_mut), 1/2*prob_mut + 1/2)
      
      descendencia <- rmultinom(1, 6, prob_cruce_Aaxaa)
    }
    
    if (parental_1[,j] == "aa" & parental_2[,j] == "aa"){
      
      prob_cruce_aaxaa <- c(0, 0, 1)
      
      descendencia <- rmultinom(1, 6, prob_cruce_aaxaa)
    }
    
    nombre_columna1 <- paste0("alelo", j-3) 
    # Se generan tantas columnas nuevas como alelos tenga el genoma del individuo.
    generacion_1[nombre_columna1] <- rep(1:3, descendencia)
    # Se recodifican los alelos: orden determinado por vector de probabilidades.
    generacion_1[nombre_columna1] <- car::recode(generacion_1[, j], 
                                                 "1='AA'; 2='Aa'; 3='aa'",
                                                 as.factor=FALSE)
  }
  generacion_1
}

# ----------------------------------------------------------------------------
set.seed(123)

# Banco de datos inicial (generación 0)
generacion_0 <- data.frame(id = rep(1:2, times = 3), 
                           linea = rep(c("linea1", "linea2", "linea3"), c(2,2,2)),
                           sexo = rep(c("Hembra", "Macho"), c(3))) 

total <- 2.7*10^9 # tamaño del genoma
for (i in 1:total){
  nombre_columna <- paste0("alelo_", i)
  generacion_0[nombre_columna] <- rep("AA", nrow(generacion_0))
}

save(generacion_0, file = "generacion_0.RData")
load("generacion_0.RData")

machos0 <- subset(generacion_0, sexo == "Macho")
hembras0 <- subset(generacion_0, sexo == "Hembra")

# PRIMERA GENERACIÓN

descendencia <- list()
for (i in 1:3){
  descendencia[[paste0("descendencia_",i)]] <- 
    camada_mod(machos0[i,], hembras0[i,], prob_mut = 5.7*10^(-9)) 
  
  while (sum(descendencia[[i]]$sexo == "Macho") == 0 | 
         sum(descendencia[[i]]$sexo == "Hembra") == 0){
    
    descendencia[[paste0("descendencia_",i)]] <- 
      camada_mod(machos0[i,], hembras0[i,], prob_mut = 5.7*10^(-9))
  }
}

descendencia_total <- data.frame() 
descendencia_total <- do.call(rbind, descendencia) 

parentales <- apareamiento("Escenario A", descendencia_total)


# SEGUNDO PASO: 50 generaciones, 5000 simulaciones

## Para n = 5/10/20/50 generaciones
num_AA <- NULL; num_AA_mean <- NULL
num_Aa <- NULL; num_Aa_mean <- NULL
num_aa <- NULL; num_aa_mean <- NULL

n <- 50

for (z in 1:5000){
  for (m in 1:n){
    descendencia <- list()
    
    for (i in 1:3){
      descendencia[[paste0("descendencia_",i)]] <- 
        camada_mod(parentales[["progenitor_macho"]][i,], 
                   parentales[["progenitor_hembra"]][i,], 
                   prob_mut = 5.7*10^(-9))
      
      while (sum(descendencia[[i]]$sexo == "Macho") == 0 | 
             sum(descendencia[[i]]$sexo == "Hembra") == 0){
        
        descendencia[[paste0("descendencia_",i)]] <- 
          camada_mod(parentales[["progenitor_macho"]][i,], 
                     parentales[["progenitor_hembra"]][i,], 
                     prob_mut = 5.7*10^(-9))
      }
    }
    
    descendencia_total <- data.frame() 
    descendencia_total <- do.call(rbind, descendencia)
    
    # Al dividir entre el total de crías que se obtiene por generación (18 
    # crías en total) da el valor medio de alelos AA por cria.
    num_AA[m] <- 
    sum(descendencia_total[,4:length(descendencia_total)] == "AA")/18*100
    num_Aa[m] <- 
    sum(descendencia_total[,4:length(descendencia_total)] == "Aa")/18*100
    num_aa[m] <- 
    sum(descendencia_total[,4:length(descendencia_total)] == "aa")/18*100
    
    parentales <- apareamiento("Escenario A", descendencia_total)
  }
  
  num_AA_mean <- rbind(num_AA_mean, num_AA)
  num_Aa_mean <- rbind(num_Aa_mean, num_Aa)
  num_aa_mean <- rbind(num_aa_mean, num_aa)
}

num_AA_mean_n50 <- round(apply(num_AA_mean, 2, mean))
num_Aa_mean_n50 <- round(apply(num_Aa_mean, 2, mean))
num_aa_mean_n50 <- round(apply(num_aa_mean, 2, mean))

