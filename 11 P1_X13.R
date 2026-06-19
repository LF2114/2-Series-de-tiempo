# -----------------------------------------------------------
# Felipe Ahumada
# 230226
# sistematizar la prueba 1: testing modelos por qs, aic, bic
# La diferencia estacional (comp D) debe ir entre 1:2, por cuanto sabemos que E estacionalidad en la serie
# -----------------------------------------------------------

{
library(seasonal)
library(seasonalview)
library(x13binary)
library(forecast)
library(stats)
library(lubridate)
library(dplyr)
library(tidyverse)
library(magrittr)
library(glue)
library(purrr)
}

rm(list = ls())

checkX13()
#Congratulations! 'seasonal' should work fine!


#1 data ------------------------------------------------------------------------
data <- readRDS('bases_final.RDS')

#2 funciones complementarias ---------------------------------------------------
source("50 Complementarias.R")


#3 gen series tiempo -----------------------------------------------------------

anio_i <- lubridate::year(min(data$fecha))
mes_i <- lubridate::month(min(data$fecha))

st <- ts(data = data$icl,
          frequency = 12,
          start = c(anio_i,mes_i))


#4 Models_result (resultados parciales modelos) ----------------------------

st_m <- readRDS("Models_result_st.rds")  
st_ranking <- st_m[[1]]
st_models <- st_m[[2]]



#lista almacenamiento 32 mejores modelos 
st_list <- vector('list', length(st_models))
regs <- regrs(y = 'st')

#i = 1 

#generacion 32 mejores modelos 
for (i in seq_along(st_models)) {
  st_list[[i]] <- 
    genmodel(y = st,
             regs = regs,
             model = st_models[[i]]$model)
}

saveRDS(st_list, "st_list.rds")

#5 32 graficos en grupos de 4 --------------------------------------------------

lista <- 
  list(c(1:4),
       c(5:8), 
       c(9:12),
       c(13:16),
       c(17:20),
       c(21:24),
       c(25:28),
       c(29:32))

#grafico ajuste estacional
#j = 1
for (j in 1:length(lista)) {
  
  #nombre archivo.png
  png(glue('st{lista[[j]]}-{lista[[j]][4]}_models.png'),
      width = 1200,
      height = 1000,
      res = 150)
  
  #grupos de graficos: 2x2
  par(mfrow = c(2,2))
  
  #i = 1
  for (i in 1:4) {
    
    plot(st_list[[ lista[[j]][i] ]],
         main = glue('st m{lista[[j]][i]} / {names(st_models)[ lista[[j]][i] ]} seasonal adj'),
         xlab = 'Anios',
         ylab = 'index')
  }
  
  dev.off()
  
}

#j = 1

#grafico distribucion residuos
for (j in 1:length(lista)) {
  
  #nombre archivo.png
  png(glue('st{lista[[j]]}-{lista[[j]][4]}_residuos.png'),
      width = 1200,
      height = 1000,
      res = 150)
  
  #grupos de graficos: 2x2
  par(mfrow = c(2,2))
  
  #i = 4
  for (i in 1:4) {
    
    hist(residuals(st_list[[ lista[[j]][i] ]]),
         main = glue('st m{lista[[j]][i]} / {names(st_models)[ lista[[j]][i] ]} residuos'),
         xlab = ''
         )
  }
  
  dev.off()
  
}




#6 generar tabulado resumen ----------------------------------------------------

#fitdf (manual)
vect <- c(1,0,2,1,1,1,2,1,3,4,
          1,2,2,3,3,2,2,4,1,3,
          2,2,3,4,2,3,2,3,3,3,
          1,2)

#test Ljung-Box
st_lb <- 
  map2(st_list,
       vect,
       ~ Box.test(residuals(.x),
                  lag = 24,
                  type = 'Ljung-Box',
                  fitdf = .y)
  )

lb <- tibble()

#i = 2

for (i in seq_along(st_lb)) {
  
  data <- tibble(lb = st_lb[[i]]$statistic,
                 pvlb = st_lb[[i]]$p.value)
  
  lb <- bind_rows(lb,data)
  
}

st_branking <- 
  bind_cols(st_ranking[c(1:32),],
            lb) 


#test shapiro (manual)
v <- c(0.9894,0.9897,0.9885,0.9915,
       0.9907,0.9894,0.9904,0.9928,
       0.9892,0.9898,0.9862,0.9898,
       0.9886,0.9923,0.9882,0.9861,
       0.9867,0.986 ,0.9832,0.9892,
       0.9926,0.9716,0.9681,0.9683,
       0.9828,0.9839,0.9771,0.9749,
       0.9919,0.971 ,0.9557,0.9615 )


st_branking %<>%
  mutate(pvsh = v,
         regs = vect)


saveRDS(st_branking, file = "st_branking.rds")








