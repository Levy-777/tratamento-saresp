boxplot(media_regiao_r$MEDPROF ~ media_regiao_r$REGIAOMETROPOLITANA)


boxplot(MEDPROF ~ REGIAOMETROPOLITANA,
        data = media_regiao_r,
        names = c("INTERIOR", "BAIXADA", "CAMPINAS", "RIBEIRAO", "SAO PAULO", "SOROCABA", "PARAIBA"),
        main = "DISPERSÃO DA MÉDIA DE PROFICIÊNCIA POR REGIÃO",
        xlab= "REGIÃO METROPOLITANA",
        ylab = "MÉDIAS DE PROFICIÊNCIA")
#las=2 inverte eixo x

head(media_regiao_r)

boxplot(MEDPROF ~ REGIAOMETROPOLITANA, 
        data = subset(media_regiao_r, REGIAOMETROPOLITANA == "INTERIOR"))
dev.new()
boxplot(MEDPROF ~ REGIAOMETROPOLITANA, 
        data = subset(media_regiao_r, REGIAOMETROPOLITANA == "RM DE SAO PAULO"))
dev.new()
boxplot(MEDPROF ~ REGIAOMETROPOLITANA,main="campinas" , 
        data = subset(media_regiao_r,REGIAOMETROPOLITANA == "RM DE CAMPINAS"))
dev.new()
boxplot(MEDPROF ~ REGIAOMETROPOLITANA, 
        data = subset(media_regiao_r, REGIAOMETROPOLITANA == "RM DA BAIXADA SANTISTA"))
dev.new()
boxplot(MEDPROF ~ REGIAOMETROPOLITANA, 
        data = subset(media_regiao_r, REGIAOMETROPOLITANA == "RM DE RIBEIRAO PRETO"))
dev.new()
boxplot(MEDPROF ~ REGIAOMETROPOLITANA, 
        data = subset(media_regiao_r, REGIAOMETROPOLITANA == "RM DE SOROCABA"))
dev.new()
boxplot(MEDPROF ~ REGIAOMETROPOLITANA, 
        data = subset(media_regiao_r, REGIAOMETROPOLITANA == "RM DO VALE DO PARAIBA E LITORAL NORTE"))
dev.new()




install.packages("lsr")
library(lsr)
modeloan <- aov(MEDPROF ~ PERIODO , data = media_regiao_r)
etaSquared(modeloan)

modelor <- lm(MEDPROF ~ PERIODO , data = media_regiao_r)
print(r_squared_model1 <- round(summary(modelor)$r.squared, 4))
