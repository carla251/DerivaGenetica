# ----------
# FUNCIONES 
# ----------

# en parental_1 y parental_2 tienes que tener una fila de un banco de datos
camada <- function(parental_1, parental_2, prob_mut){
  descendencia <- NULL
  
  if (parental_1$alelos == "AA" & parental_2$alelos == "AA"){
    
    # vector con probabilidad de: AA, Aa o aa, en función del cruce y si muta.
    prob_cruce_AAxAA <- c((1-prob_mut)*(1-prob_mut), 2*prob_mut*(1-prob_mut), 
                          prob_mut^2)
    # multinomial con 3 niveles: AA, Aa y aa
    descendencia <- rmultinom(1, 6, prob_cruce_AAxAA)
  }
  
  # se pueden tener dos opciones: cruce AA x Aa o cruce Aa x AA
  if (parental_1$alelos == "AA" & parental_2$alelos == "Aa" | 
      parental_1$alelos == "Aa" & parental_2$alelos == "AA"){
    
    prob_cruce_AAxAa <- c(1/2*(1 - prob_mut)*(1 - prob_mut), 
                          1/2*(1 - prob_mut)*(2*prob_mut + 1), 
                          1/2*prob_mut*(prob_mut + 1))
    
    descendencia <- rmultinom(1, 6, prob_cruce_AAxAa)
  }
  
  # se pueden tener dos opciones: cruce AA x aa o cruce aa x AA
  if (parental_1$alelos == "AA" & parental_2$alelos == "aa" | 
      parental_1$alelos == "aa" & parental_2$alelos == "AA"){
    
    prob_cruce_AAxaa <- c(0, 1 - prob_mut, prob_mut)
    
    descendencia <- rmultinom(1, 6, prob_cruce_AAxaa)
  }
  
  if (parental_1$alelos == "Aa" & parental_2$alelos == "Aa"){
    
    prob_cruce_AaxAa <- c(1/4*(1 - prob_mut)*(1 - prob_mut),
                          1/4*2*prob_mut*(1 - prob_mut) + 1/2*(1 - prob_mut), 
                          1/4*(prob_mut)^2 + 1/2*prob_mut + 1/4)
    
    descendencia <- rmultinom(1, 6, prob_cruce_AaxAa)
  }
  
  # se pueden tener dos opciones: cruce Aa x aa o cruce aa x Aa
  if (parental_1$alelos == "Aa" & parental_2$alelos == "aa" | 
      parental_1$alelos == "aa" & parental_2$alelos == "Aa"){
    
    prob_cruce_Aaxaa <- c(0, 1/2*(1 - prob_mut), 1/2*prob_mut + 1/2)
    
    descendencia <- rmultinom(1, 6, prob_cruce_Aaxaa)
  }
  
  if (parental_1$alelos == "aa" & parental_2$alelos == "aa"){
    
    prob_cruce_aaxaa <- c(0, 0, 1)
    
    descendencia <- rmultinom(1, 6, prob_cruce_aaxaa)
  }
  
  # se va a crear el banco de datos resultante
  ## hay 3 posibles lineas: linea1, linea2, linea3 -> fuera de la funcion: for
  ### 1 banco de datos con las 3 lineas: 18 crías en total, 6 crías por línea
  generacion_1 <- data.frame(id = c(1:6), linea = rep(paste0("linea", i), 6), 
                             alelos_rec = rep(1:3,descendencia), 
                             sexo = sample(c("Hembra","Macho"), 6, 
                                           prob = c(0.5,0.5), replace=TRUE))
  # se recodifican los alelos: orden determinado por el vector de probabilidades
  generacion_1$alelos <- car::recode(generacion_1$alelos, 
                                     "1='AA'; 2='Aa'; 3='aa'", as.factor=FALSE) 
  generacion_1
  
} 

# generacion_x: se proporciona el banco de datos resultante de la función camada
apareamiento <- function(escenario, generacion_x){
  # se crean previamente los vectores/listas necesarias
  machos <- NULL; hembras <- NULL
  machos1 <- NULL; hembras1 <- NULL
  machos2 <- NULL; hembras2 <- NULL
  machos3 <- NULL; hembras3 <- NULL
  
  linea1 <- NULL; linea2 <- NULL; linea3 <- NULL
  
  linea_sel <- NULL; camada_sel <- NULL; linea_sel2 <- NULL; camada_sel2 <- NULL
  
  machos_aleatorios <- NULL; hembras_aleatorias <- NULL
  
  parental_M <- list(); parental_H <- list(); progenitores <- list()
  
  # resample: función para seleccionar aleatoriamente un elemento 
  ## permite detectar únicamente los elementos presentes en x
  ## si x = 3, solo detecta el 3 y no x <= 3 (esto último lo hace sample)
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  
  if (escenario == "Escenario A"){
    
    # Agrupar la descendencia en función de la línea de la que provenga
    linea1 <- subset(generacion_x, linea == "linea1")
    linea2 <- subset(generacion_x, linea == "linea2")
    linea3 <- subset(generacion_x, linea == "linea3")
    
    # Agrupar en machos y hembras para poder seleccionar posteriormente al azar
    machos1 <- subset(linea1, sexo == "Macho")
    hembras1 <- subset(linea1, sexo == "Hembra")
    
    machos2 <- subset(linea2, sexo == "Macho")
    hembras2 <- subset(linea2, sexo == "Hembra")
    
    machos3 <- subset(linea3, sexo == "Macho")
    hembras3 <- subset(linea3, sexo == "Hembra")
    
    # Seleccionar el macho o la hembra de forma aleatoria
    macho_aleatorio1 <- resample(machos1$id, 1)
    hembra_aleatoria1 <- resample(hembras1$id, 1)
    
    macho_aleatorio2 <- resample(machos2$id, 1)
    hembra_aleatoria2 <- resample(hembras2$id, 1)
    
    macho_aleatorio3 <- resample(machos3$id, 1)
    hembra_aleatoria3 <- resample(hembras3$id, 1)
    
    # Agrupar todos los machos y hembras en un unico vector respectivamente
    machos_aleatorios <- c(macho_aleatorio1, macho_aleatorio2, macho_aleatorio3)
    hembras_aleatorias <- c(hembra_aleatoria1, hembra_aleatoria2, 
                            hembra_aleatoria3)
    
    # Generar una lista para machos y otra para hembras con toda la información 
    ## Id + linea -> permite identificarlos de forma única
    ## i = 3, porque solo se seleccionan 3 parejas 
    for (i in 1:3){
      parental_M[[paste0("parental_M",i)]] <- generacion_x[generacion_x$id == 
machos_aleatorios[i] & generacion_x$linea == paste0("linea", i), ] 
      parental_H[[paste0("parental_H",i)]] <- generacion_x[generacion_x$id == 
hembras_aleatorias[i] & generacion_x$linea == paste0("linea", i), ]
    }
  }
  
  if (escenario == "Escenario B"){
    
    # seleccionar al azar una de las líneas a partir de la cual se llevará 
    # a cabo el siguiente cruce
    linea_sel <- sample(c("linea1", "linea2", "linea3"), 1)
    # agrupar la descendencia en función de la línea seleccionada
    camada_sel <- subset(generacion_x, linea == linea_sel)
    
    # Mientras el número de machos y hembras de una camada sea distinto,
    ## selecciona otra camada al azar hasta encontrar una camada donde:
    ## número de machos y hembras sea el mismo
    while (sum(camada_sel$sexo == "Macho") != sum(camada_sel$sexo == "Hembra")){
      linea_sel <- sample(c("linea1", "linea2", "linea3")
                          [c("linea1", "linea2", "linea3") != linea_sel], 1)
      # agrupar la descendencia en función de la línea seleccionada
      camada_sel <- subset(generacion_x, linea == linea_sel)
    }
    
    # dicha camada se agrupa en hembras y machos 
    machos <- subset(camada_sel, sexo == "Macho")
    hembras <- subset(camada_sel, sexo == "Hembra")
    
    # Seleccionar al azar 3 machos y 3 hembras 
    machos_aleatorios <- sample(machos$id, 3)
    hembras_aleatorias <- sample(hembras$id, 3)
    
    # Guardar a través de una lista toda la información de interés de los machos
    # y hembras seleccionados. Para ello, tiene que coincidir el id.
    for (i in 1:3){
      parental_M[[paste0("parental_M",i)]] <- camada_sel[camada_sel$id == 
                                              machos_aleatorios[i], ]
      parental_H[[paste0("parental_H",i)]] <- camada_sel[camada_sel$id == 
                                              hembras_aleatorias[i], ]
    }
  }
  
  if (escenario == "Escenario C"){
    
    # Seleccionar al azar una de las líneas a partir de la cual se llevará  
    # a cabo el siguiente cruce.
    linea_sel <- sample(c("linea1", "linea2", "linea3"),1)
    # Agrupar la descendencia en función de la línea seleccionada.
    camada_sel <- subset(generacion_x, linea == linea_sel)
    
    # Mientras que en la camada seleccionada todas las crías sean macho o hembra
    while (sum(camada_sel$sexo == "Macho") == 0 | 
           sum(camada_sel$sexo == "Hembra") == 0){
      # selecciona otra camada
      linea_sel <- sample(c("linea1", "linea2", "linea3")
                          [c("linea1", "linea2", "linea3") != linea_sel], 1)
      # agrupa la descendencia en función de la nueva línea seleccionada
      camada_sel <- subset(generacion_x, linea == linea_sel)
    }
    
    # dicha camada se agrupa en hembras y machos 
    machos <- subset(camada_sel, sexo == "Macho")
    hembras <- subset(camada_sel, sexo == "Hembra")
    
    # si en la camada seleccionada hay el mismo número de machos que de hembras:
    # selecciona 3 machos y hembras
    if (sum(camada_sel$sexo == "Macho") == sum(camada_sel$sexo == "Hembra")){
      machos_aleatorios <- sample(machos$id, 3)
      hembras_aleatorias <- sample(hembras$id, 3)
      
      for (i in 1:3){
        parental_M[[paste0("parental_M",i)]] <- camada_sel[camada_sel$id == 
                                                machos_aleatorios[i], ]
        parental_H[[paste0("parental_H",i)]] <- camada_sel[camada_sel$id == 
                                                hembras_aleatorias[i], ]
      }
      
    } else{ # Si el número de machos y hembras no es el mismo: 
      # selecciona tantos machos y hembras como parejas mínimas puedas formar:
      ## le digo que haga un for desde 1 hasta el mínimo entre machos y hembras.
      for (i in 1:min(sum(camada_sel$sexo == "Macho"),
                      sum(camada_sel$sexo == "Hembra"))){
        machos_aleatorios[i] <- resample(machos$id, 1)
        hembras_aleatorias[i] <- resample(hembras$id, 1)
        
        # Guarda en el banco de datos machos/hembras aquellos que no han sido 
        # seleccionados, evitando así que los ya seleccionados puedan volver a 
        # ser seleccionados, eliminándolos.
        machos <- machos[machos$id != machos_aleatorios[i],]
        hembras <- hembras[hembras$id != hembras_aleatorias[i],]
        
        parental_M[[paste0("parental_M",i)]] <- camada_sel[camada_sel$id == machos_aleatorios[i], ]
        parental_H[[paste0("parental_H",i)]] <- camada_sel[camada_sel$id == hembras_aleatorias[i], ]
        
      }
      
      # Selecciona otra línea al azar distinta de la seleccionada anteriormente.
      linea_sel2 <- sample(c("linea1", "linea2", "linea3")
                           [c("linea1", "linea2", "linea3") != linea_sel],1) 
      camada_sel2 <- subset(generacion_x, linea == linea_sel2)
      
      # Mientras en dicha camada toda las crías son macho o hembra:
      while (sum(camada_sel2$sexo == "Macho") == 0 | 
             sum(camada_sel2$sexo == "Hembra") == 0){
        # selecciona una línea distinta a la seleccionada previamente (linea_sel)
        ## y distinta a la que se acaba de seleccionar (linea_sel2).
        linea_sel2 <- sample(c("linea1", "linea2", "linea3")
                             [c("linea1", "linea2", "linea3") != linea_sel & 
                             c("linea1", "linea2", "linea3") != linea_sel2], 1)
        # Agrupa la descendencia en función de la nueva línea seleccionada
        camada_sel2 <- subset(generacion_x, linea == linea_sel2)
      }
      
      # Si con la camada seleccionada anterior (camada_sel) y con la nueva
      # (camada_sel2) no se pueden generar las 3 parejas necesarias: 
      # utiliza la línea restante en vez de camada_sel2.
      ### Se suma el número mínimo entre machos y hembras de la primera camada
      ### (podrá ir de 0-3) con el mínimo entre machos y hembras de la 2 camada.
      if ((min(sum(camada_sel$sexo == "Macho"), 
               sum(camada_sel$sexo == "Hembra")) + 
           min(sum(camada_sel2$sexo == "Macho"), 
               sum(camada_sel2$sexo == "Hembra"))) < 3){
        # Selecciona la línea restante (distinta de linea_sel y linea_sel2).
        linea_sel2 <- sample(c("linea1", "linea2", "linea3")
                             [c("linea1", "linea2", "linea3") != linea_sel & 
                              c("linea1", "linea2", "linea3") != linea_sel2], 1)
        # Agrupa la descendencia en función de la nueva línea seleccionada.
        camada_sel2 <- subset(generacion_x, linea == linea_sel2)
      }
      
      # Dicha camada se agrupa en hembras y machos. 
      machos2 <- subset(camada_sel2, sexo == "Macho")
      hembras2 <- subset(camada_sel2, sexo == "Hembra")
      
      # Elige machos y hembras de la última camada seleccionada hasta generar 
      # las 3 parejas necesarias, teniendo en cuenta las parejas ya generadas 
      # con la camada seleccionada inicialmente. 
      for (i in (min(sum(camada_sel$sexo == "Macho"), 
                     sum(camada_sel$sexo == "Hembra"))+1):3){
        machos_aleatorios[i] <- resample(machos2$id, 1)
        hembras_aleatorias[i] <- resample(hembras2$id, 1)
        
        # Elimina los machos y hembras ya seleccionados anteriormente.
        machos2 <- machos2[machos2$id != machos_aleatorios[i],]
        hembras2 <- hembras2[hembras2$id != hembras_aleatorias[i],]
        
        parental_M[[paste0("parental_M",i)]] <- camada_sel2[camada_sel2$id == machos_aleatorios[i], ]
        parental_H[[paste0("parental_H",i)]] <- camada_sel2[camada_sel2$id == hembras_aleatorias[i], ]
      }
    }
  }
  
  if (escenario == "Escenario D"){
    # Se carga la librería dplyr para usar la función slice_sample:
    ## permite seleccionar un número aleatorio de filas de un banco de datos.
    library (dplyr) 
    
    m <- NULL; h <- NULL 
    
    machos <- subset(generacion_x, sexo == "Macho")
    hembras <- subset(generacion_x, sexo == "Hembra")
    
    for (i in 1:3){
      # Selecciona un macho y hembra del banco de datos de forma aleatoria:
      ## Se usa slice_sample, porque en el escenario D se considera toda la 
      ## descendencia como una "única" camada, pero se tienen id repetidos de 
      ## distintas líneas -> por ejemplo se tienen 3 crías de 3 líneas distintas
      ## con id = 1 y sino no se van a poder diferenciar.
      parental_M[[paste0("parental_M", i)]] <- slice_sample(machos, n = 1)
      parental_H[[paste0("parental_H",i)]] <- slice_sample(
        hembras[(hembras$linea != parental_M[[paste0("parental_M", i)]]$linea),],
        n = 1) 

      # Si todas las hembras que quedan tienen la misma línea que la del macho
      # seleccionado (TRUE):
      if (all(hembras$linea == parental_M[[paste0("parental_M", i)]]$linea) == 
          TRUE){
        # Elije a una hembra y en función de la línea de la hembra que queda, 
        # selecciona al macho.
        parental_H[[paste0("parental_H",i)]] <- slice_sample(hembras, n = 1) 
        parental_M[[paste0("parental_H",i)]] <- slice_sample(
          machos[(machos$linea != parental_H[[paste0("parental_H", i)]]$linea),],
          n = 1) 
      }
      
      # Al usar slice_sample se va a poder seleccionar líneas del banco de datos
      # ya seleccionadas previamente:
      ## guarda la línea del banco de datos que ha sido seleccionada al azar.
      ## Para ello tiene que coincidir id y linea.
      m <- which(machos$id == parental_M[[i]]$id & machos$linea == parental_M[[i]]$linea)
      h <- which(hembras$id == parental_H[[i]]$id & hembras$linea == parental_H[[i]]$linea)
      # Elimina dicha fila para que no pueda volver a ser seleccionada.
      machos <- machos[-m, ]
      hembras <- hembras[-h, ]
    }
  }
  
  progenitor_M <- data.frame(); progenitor_H <- data.frame()
  
  # Guarda las 3 bancos de datos que hay en la lista parental_X en uno solo:
  ## rbind -> combina secuencias de vectores/matrices por filas.
  ## do-call -> ejecutar una función a la lista de argumentos especificada.
  progenitor_M <- do.call(rbind, parental_M) 
  progenitor_H <- do.call(rbind, parental_H)
  
  progenitores <- list(progenitor_macho = progenitor_M, progenitor_hembra = progenitor_H)
  progenitores
}

