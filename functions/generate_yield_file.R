#### function to generate yield file by crop and department ##

estimate_distance <- function(factor_series, series=series, quart_ref=quart_ref) {
  dist <- rep(NA, length(quart_ref))
  names(dist) <- names(quart_ref)
  series_fitted <- factor_series*series
  for (quarter_lev in names(quart_ref)) {
    dist[quarter_lev] <- as.numeric(quantile(series_fitted, as.numeric(quarter_lev)/100)) - as.numeric(quart_ref[quarter_lev])
  }
  out <- c(((sum(dist^2))^0.5)/100,sd(factor_series))
  return(out)
}

estimate_distance_tomin <- function(factor_series){
  out <- estimate_distance(factor_series, series=series, quart_ref=quart_ref) 
  return(out)
}

generate_yield_file <- function(HistRendDataFile,ProrindesDataFile,YieldDataFile) {
  # # to be removed
  # HistRendDataFile <- 'data/HistRendData.csv'
  # ProrindesDataFile <-'data/prorindes_edited.csv'
  # YieldDataFile <- 'data/yiedlData.csv'
  # ##
  
  HistRendData <- read.csv(HistRendDataFile)
  ProrindesData <- read.csv(ProrindesDataFile)
  Yield_list <- list()
  
  for (row_id in  1:nrow(ProrindesData)) { 
    print(row_id)
    cultivo <- as.character(ProrindesData[row_id,'cultivo'])
    localidad <- as.character(ProrindesData[row_id,'localidad'])
    cultivo_to_compare <- strsplit(cultivo,split=' ')[[1]][1]
    series <- HistRendData[ HistRendData$Cultivo==cultivo_to_compare&
                            HistRendData$Departamento==localidad,
                            'Rendimiento']
    
    quart_ref <- as.numeric(ProrindesData[row_id,c('p05','p25','p50','p75','p95')])
    names(quart_ref) <- c('05','25','50','75','95')
    
    r1 <- nsga2(estimate_distance,length(series), 2,
                generations=200, popsize=60*16,
                lower.bounds = rep(0.4,length(series)),
                upper.bounds = rep(10,length(series)),
                cprob=0.7, cdist=20,
                mprob=0.2, mdist=20,
                series=series, 
                quart_ref=quart_ref)
    
    id <- which.min(apply(r1$value,1,function(x) sum(x^2)))
    
    factor_vector <- r1$par[id,]
    
    fitted_series <- factor_vector*series
    
    Yield_list[[row_id]] <- data.frame(cultivo=cultivo,
                                       departamento=localidad,
                                       campana=HistRendData[ HistRendData$Cultivo==cultivo_to_compare&
                                                               HistRendData$Departamento==localidad,
                                                             'Campania'],
                                       suelo=as.character(ProrindesData[row_id,'suelo']),
                                       manejo=as.character(ProrindesData[row_id,'Manejo.Suelo']),
                                       rendimiento=fitted_series,
                                       stringsAsFactors = F)
    
  }
  out_df <- do.call("rbind", Yield_list)
  write.csv(out_df, file=YieldDataFile)
  return(out_df)
}
