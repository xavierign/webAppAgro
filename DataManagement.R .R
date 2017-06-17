####################################
### script to pre treat the data ###
####################################

#### 0. libraries and reset env ####
rm(list = ls())
require(mco)
require(xlsx)
require(zoo)
#### 1. load data ####

source('functions/generate_yield_file.R')

#generate_yield_file('data/HistRendData.csv','data/prorindes_edited.csv','data/YieldData.csv')

#convertir Yield Data csv en xls 
climate_table <- read.csv('data/climate_data.csv')
load('data/yield_table.RData')
# yield_table <- read.xlsx('data/YieldData.xlsx', sheetIndex=3)
# save(yield_table,file='data/yield_table.RData')
prices_table <- read.xlsx('data/YieldData.xlsx', sheetIndex=2,check.names=FALSE)
rownames(prices_table) <- prices_table$Year

crop_table <- read.xlsx('data/YieldData.xlsx', sheetIndex=1)
rownames(crop_table) <- crop_table$Etiqueta

estaciones_a_mantener <- c("CORONEL SUAREZ AERO", "Junin Aerodrome", 'Pehuajo Aerodrome', 'Tandil Aerodrome', 'NUEVE DE JULIO')
estaciones_a_renombrar <- c('CORONEL SUAREZ', 'JUNIN', 'PEHUAJO', 'TANDIL', '9 DE JULIO')

climate_table <- climate_table[climate_table$estacion %in% estaciones_a_mantener,]

climate_table$estacion <- as.character(climate_table$estacion)

for (i in 1:5) {
  climate_table$estacion[climate_table$estacion==estaciones_a_mantener[i]] <- estaciones_a_renombrar[i]
}
climate_table$fecha <- as.Date(climate_table$fecha)

depto_campana <- data.frame(yield_table[,c(3,4)])
depto_campana <- depto_campana[!duplicated(depto_campana),]

departamento <- depto_campana[1,"departamento"]
campana <- depto_campana[1,"campana"]




estaciones <- unique(climate_table$estacion)
calcular_pp <- function (fila) {
  anios <- strsplit(as.character(fila$campana),split="/")
  anio1 <- as.numeric(anios[[1]][1])
  anio2 <- as.numeric(anios[[1]][2])
  if (anio2 >50) {
    anio2 <- 1900 + anio2
  } else {
    anio2 <- 2000 + anio2
  }

  PP_series <- climate_table[climate_table$estacion==fila$departamento&
                climate_table$fecha <= as.Date(paste(anio2,'-03-31',sep=""))&
                climate_table$fecha >= as.Date(paste(anio1,'-10-01',sep="")), 'PP']  
  
  if (length(PP_series)<160) {
    return(NA)
  } else {
    return(sum(PP_series, na.rm=T))
  }
}

depto_campana$PP <- NA

for (rowid in 1:nrow(depto_campana)){
  print(as.character(depto_campana[rowid,'campana']))
  print(calcular_pp(depto_campana[rowid,]))
  depto_campana[rowid,'PP'] <- calcular_pp(depto_campana[rowid,])
}

#escenaridepto_campana
escenarios_clima <- depto_campana[!is.na(depto_campana$PP),]
lista_escenarios_clima <- list()
for (estacion in estaciones_a_renombrar){
  print(estacion)
  lista_escenarios_clima[[estacion]] <- escenarios_clima[escenarios_clima$departamento==estacion,c('campana','PP')]
  rownames(lista_escenarios_clima[[estacion]]) <- escenarios_clima[escenarios_clima$departamento==estacion,'campana']
}

#elijo una ubicacion
location <- 'TANDIL'

#elijo tipo de suelo
soil_type <- '(Argiudol tipico/Mar del Plata (tosca 140 cm))'

crop_domain <- as.character(unique(yield_table[yield_table$departamento==location&yield_table$suelo==soil_type,'etiqueta']))

#build scenario_table_list

scenario_df_indexes <- expand.grid(climate_year=rownames(lista_escenarios_clima[[location]]),price_year=rownames(prices_table))
scenario_df <- data.frame(scenario_df_indexes, 
                          PP=lista_escenarios_clima[[ubicacion]][scenario_df_indexes$climate_year,'PP'])

columns1 <- prices_table[scenario_df_indexes$price_year,unique(as.character(crop_table[crop_domain,'Cultivo']))]
scenario_df <- cbind(scenario_df,columns1)

columns2 <- prices_table[scenario_df_indexes$price_year,as.character(crop_table[crop_domain,'Cultivo'])]
colnames(columns2) <- paste0('price_',crop_domain)

scenario_df <- cbind(scenario_df,columns2)

#add yields
temp_yield_table <- yield_table[yield_table$departamento==location&yield_table$suelo==soil_type,]
get_yield <- function(climate_year, label,temp_yield_table) {
  rend <- temp_yield_table[temp_yield_table$campana==climate_year&
                     temp_yield_table$etiqueta==label,'rendimiento']
  return(rend)
}
for (crop in crop_domain) {
  temp_vector <- as.numeric(as.character(sapply(as.character(scenario_df$climate_year),label=crop,temp_yield_table=temp_yield_table,FUN = get_yield )))
  temp_df <- data.frame(temp_vector)
  colnames(temp_df) <- crop
  scenario_df <- cbind(scenario_df,temp_df)
}


