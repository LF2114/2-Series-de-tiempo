# -----------------------------------------------------------
# Felipe Ahumada
# 230226
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
}


rm(list = ls())

checkX13()
#Congratulations! 'seasonal' should work fine!


#1 data ------------------------------------------------------------------------
data <- readRDS('bases_final.RDS')


#2 funciones complementarias ---------------------------------------------------
source("50 Complementarias.R")


#3 gen series tiempo: st -------------------------------------------------------
anio_i <- lubridate::year(min(data$fecha))
mes_i <- lubridate::month(min(data$fecha))
st <- ts(data = data$icl,
          frequency = 12,
          start = c(anio_i,mes_i))


#4 Generar set de modelos sarima -----------------------------------------------
models_sarima <- 
  expand.grid(
    p = 0:1,
    d = 0:2,
    q = 0:1,
    P = 0:1,
    D = 1:2, #rango: 1:2
    Q = 0:1 
  )


##1 Aplica st ------------------------------------------------------------------
#Almacena
results <- vector('list',nrow(models_sarima))
y <- st
regs <- regrs(y = 'st')

#i = 1
for (i in seq_len(nrow(models_sarima))) {
  g <- models_sarima[i, ] #g es df
  results[[i]] <- 
    model2(
      y = y,
      regs = regs,
      p = g$p,
      d = g$d,
      q = g$q,
      P = g$P,
      D = g$D,
      Q = g$Q
    )
}


#1.1 ordenar modelos por menor qs,aic,bic --------------------------------------
ranking <- tibble()

for (i in 1:length(results)) {
  
  tab <- tibble(
    index = i,
    modelo = results[[i]]$model,
    aic = results[[i]]$AICc,
    bic = results[[i]]$BIC,
    qsori = results[[i]]$qsori[1],
    pvori = results[[i]]$pvori[1],
    qsr = results[[i]]$QS['qs'],
    pvr = results[[i]]$QS['p-val'],
    trans = results[[i]]$Trans
  ) 
  ranking <- bind_rows(ranking,tab)
}

ranking %<>% arrange(., qsr,aic,bic)
ranking %>% 
  filter(modelo %in% c('(1 1 0)(0 1 1)',
                       '(0 2 1)(0 1 1)')) %>% print() #index52 / position32


#1.2 seleccionar 32 modelos con mejor ranking qs,aic,bic -----------------------
id <- ranking$index[c(1:32)] 
results_f <- vector('list',length(id))

#i = 1
for (i in seq_along(id)) {
  results_f[[i]] <- 
    setNames(list(results[[ id[i] ]]),
             glue('m{id[i]}')
    )
}
results_f %<>% flatten()



#1.3 outputs -------------------------------------------------------------------
#1 ranking / #2 results_f
Models_result <- 
  list(ranking = ranking,
       results_f = results_f)


saveRDS(Models_result,'Models_result_st.rds')














