calculate_pp <- function (fila) {
  #this function calculates the rainfall by campana depto for ex. (fila = "PEHUAJO" "1999/00") and returns
  #the sum of rainfalls between 1999-10-01 and 2000-03-31
  
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
