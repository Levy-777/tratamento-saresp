regioes <- c("INTERIOR", "RM DA BAIXADA SANTISTA", "RM DE CAMPINAS", 
             "RM DE RIBEIRAO PRETO", "RM DE SAO PAULO", "RM DE SOROCABA", 
             "RM DO VALE DO PARAIBA E LITORAL NORTE")
dscomp <- c("MA", "LP")

previsao_combinada <- list()
i <- 1

for (regiao in regioes) {
  for (materias in dscomp){
    dados_regiao <- media_regiao_r[media_regiao_r["SERIEANO"] == "3M" & 
                                     media_regiao_r["DSCOMP"] == materias & 
                                     media_regiao_r["REGIAOMETROPOLITANA"] == regiao, ]
    
    dados_agregados <- aggregate(MEDPROF ~ ANO, data = dados_regiao, FUN = mean)
    modelo <- lm(MEDPROF ~ ANO, data = dados_agregados)
    
    nova_previsao <- data.frame(ANO = c(2023, 2024))
    previsao <- predict(modelo, nova_previsao)
    nova_previsao$MEDPROF <- round(previsao/500, 2)
    nova_previsao$REGIAO <- regiao
    nova_previsao$DSCOMP <- materias
    
    previsao_combinada[[i]] <- nova_previsao
    i <- i + 1
  }
}

previsao_combinada <- do.call(rbind, previsao_combinada)
head(previsao_combinada)


write.csv(previsao_combinada, file = "C:/workspace/artigo saresp/previsao_combinada.csv", row.names = FALSE)