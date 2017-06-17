####################################
### script to pre treat the data ###
####################################

#### 0. libraries and reset env ####
rm(list = ls())
require(mco)
require(xlsx)
require(ggplot2)
source('functions/generate_yield_file.R')
source('functions/calculate_pp.R')


#### 1. load data ####
#convertir Yield Data csv en xls (Yield Data.csv is corrupted)

climate_table <- read.csv('data/climate_data.csv')
load('data/yield_table.RData')

# yield_table <- read.xlsx('data/YieldData.xlsx', sheetIndex=3)
# save(yield_table,file='data/yield_table.RData')

prices_table <- read.xlsx('data/YieldData.xlsx', sheetIndex=2,check.names=FALSE)

#generate_yield_file('data/HistRendData.csv','data/prorindes_edited.csv','data/YieldData.csv')

rownames(prices_table) <- prices_table$Year
crop_table <- read.xlsx('data/YieldData.xlsx', sheetIndex=1)
rownames(crop_table) <- crop_table$Etiqueta

#### 2. edit data ####

#a) climate_table: table with daily climatic information by station
estaciones_a_mantener <- c("CORONEL SUAREZ AERO", "Junin Aerodrome", 'Pehuajo Aerodrome', 'Tandil Aerodrome', 'NUEVE DE JULIO')
estaciones_a_renombrar <- c('CORONEL SUAREZ', 'JUNIN', 'PEHUAJO', 'TANDIL', '9 DE JULIO')

climate_table <- climate_table[climate_table$estacion %in% estaciones_a_mantener,]

climate_table$estacion <- as.character(climate_table$estacion)

for (i in 1:5) {
  climate_table$estacion[climate_table$estacion==estaciones_a_mantener[i]] <- estaciones_a_renombrar[i]
}
climate_table$fecha <- as.Date(climate_table$fecha)

#defines 
estaciones <- unique(climate_table$estacion)

#b) create a tabla departamento-campaña-pp

depto_campana <- data.frame(yield_table[,c(3,4)])
depto_campana <- depto_campana[!duplicated(depto_campana),]
depto_campana$PP <- NA

for (rowid in 1:nrow(depto_campana)){
  depto_campana[rowid,'PP'] <- calculate_pp(depto_campana[rowid,])
}

#creates lista_escenarios_clima
escenarios_clima <- depto_campana[!is.na(depto_campana$PP),]
lista_escenarios_clima <- list()
for (estacion in estaciones_a_renombrar){
  lista_escenarios_clima[[estacion]] <- escenarios_clima[escenarios_clima$departamento==estacion,c('campana','PP')]
  rownames(lista_escenarios_clima[[estacion]]) <- escenarios_clima[escenarios_clima$departamento==estacion,'campana']
}

#### 3. select location and soil type ####
#choose a location
location <- 'TANDIL'

#choose a soil type
soil_type <- '(Argiudol tipico/Mar del Plata (tosca 140 cm))'

#determine the crop domain
crop_domain <- as.character(unique(yield_table[yield_table$departamento==location&yield_table$suelo==soil_type,'etiqueta']))

#### 4. build scenario_table_list ####
scenario_df_indexes <- expand.grid(climate_year=rownames(lista_escenarios_clima[[location]]),price_year=rownames(prices_table))
scenario_df <- data.frame(scenario_df_indexes, 
                          PP=lista_escenarios_clima[[location]][scenario_df_indexes$climate_year,'PP'])

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

#only complete cases
scenario_df <- scenario_df[complete.cases(scenario_df),]

#### 5. plot the histogram of income ####
plot_income_table <- scenario_df[,c('Soja2','Soja5','Maíz5','Maíz4','Trigo2','Trigo4')] * 
              scenario_df[,paste('price_',c('Soja2','Soja5','Maíz5','Maíz4','Trigo2','Trigo4'),sep="")]

ggplot(plot_income_table) + geom_histogram(aes(Soja2),fill = "red", alpha = 0.2)+ 
                          geom_histogram(aes(Soja5),fill = "blue", alpha = 0.2) +
                          geom_histogram(aes(Maíz5),fill = "yellow", alpha = 0.2) 


