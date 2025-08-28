library(e1071)
library(ggplot2)
library(ggpubr)
set.seed(123)

notalp3m <- media_regiao_r[media_regiao_r$SERIEANO == "3M" & media_regiao_r$DSCOMP == "LP", ]
notalp3m <- notalp3m$MEDPROF
notalp3m <- notalp3m[notalp3m != 0 & !is.na(notalp3m)]
classificacaolp3m <- cut(notalp3m, breaks = c(-Inf, 250, 300, 375, Inf),
                     labels = c("Abaixo", "Basico", "Adequado", "Avancado"), right = FALSE)
dadoslp3m <- data.frame(notalp3m, classificacaolp3m)
dadoslp3m <- na.omit(dadoslp3m)
n <- nrow(dadoslp3m)

indiceslp3m <- sample(1:n, size = 0.7 * n)
treinolp3m <- dadoslp3m[indiceslp3m, ]
testelp3m <- dadoslp3m[-indiceslp3m, ]

modelo_svmlp3m <- svm(classificacaolp3m ~ notalp3m, data = treinolp3m, kernel = "linear")
predicoeslp3m <- predict(modelo_svmlp3m, newdata = testelp3m)

matriz_confusaolp3m <- table(Predito = predicoeslp3m, Real = testelp3m$classificacaolp3m)
print(matriz_confusaolp3m)

acuracialp3m <- sum(diag(matriz_confusaolp3m)) / sum(matriz_confusaolp3m)
cat("Acurácia:", round(acuracialp3m * 100, 2), "%\n")

testelp3m$predito <- predict(modelo_svmlp3m, newdata = testelp3m)

plotlp3m <- ggplot(testelp3m, aes(x = notalp3m, y = 0, color = predito)) +
  geom_jitter(height = 0.1, width = 0, size = 2, alpha = 0.7) +
  geom_vline(xintercept = c(250, 300, 375), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Abaixo" = "red", "Basico" = "orange", 
                                "Adequado" = "blue", "Avancado" = "darkgreen"), drop = FALSE) +
  labs(title = "Língua Portuguesa - 3° Ano", x = "", y = "", color = "Classificação") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



notama3m <- media_regiao_r[media_regiao_r$SERIEANO == "3M" & media_regiao_r$DSCOMP == "MA", ]
notama3m <- notama3m$MEDPROF
notama3m <- notama3m[notama3m != 0 & !is.na(notama3m)]
classificacaoma3m <- cut(notama3m, breaks = c(-Inf, 275, 350, 400, Inf),
                         labels = c("Abaixo", "Basico", "Adequado", "Avancado"), right = FALSE)
dadosma3m <- data.frame(notama3m, classificacaoma3m)
dadosma3m <- na.omit(dadosma3m)
n <- nrow(dadosma3m)

indicesma3m <- sample(1:n, size = 0.7 * n)
treinoma3m <- dadosma3m[indicesma3m, ]
testema3m <- dadosma3m[-indicesma3m, ]

modelo_svmma3m <- svm(classificacaoma3m ~ notama3m, data = treinoma3m, kernel = "linear")
predicoesma3m <- predict(modelo_svmma3m, newdata = testema3m)

matriz_confusaoma3m <- table(Predito = predicoesma3m, Real = testema3m$classificacaoma3m)
print(matriz_confusaoma3m)

acuraciama3m <- sum(diag(matriz_confusaoma3m)) / sum(matriz_confusaoma3m)
cat("Acurácia:", round(acuraciama3m * 100, 2), "%\n")

testema3m$predito <- predict(modelo_svmma3m, newdata = testema3m)

plotma3m <- ggplot(testema3m, aes(x = notama3m, y = 0, color = predito)) +
  geom_jitter(height = 0.1, width = 0, size = 2, alpha = 0.7) +
  geom_vline(xintercept = c(275, 350, 400), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Abaixo" = "red", "Basico" = "orange", 
                                "Adequado" = "blue", "Avancado" = "darkgreen"), drop = FALSE) +
  labs(title = "Matemática - 3° Ano", x = "", y = "", color = "Classificação") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



notalp9f <- media_regiao_r[media_regiao_r$SERIEANO == "9F" & media_regiao_r$DSCOMP == "LP", ]
notalp9f <- notalp9f$MEDPROF
notalp9f <- notalp9f[notalp9f != 0 & !is.na(notalp9f)]
classificacaolp9f <- cut(notalp9f, breaks = c(-Inf, 200, 275, 325, Inf),
                         labels = c("Abaixo", "Basico", "Adequado", "Avancado"), right = FALSE)
dadoslp9f <- data.frame(notalp9f, classificacaolp9f)
dadoslp9f <- na.omit(dadoslp9f)
n <- nrow(dadoslp9f)

indiceslp9f <- sample(1:n, size = 0.7 * n)
treinolp9f <- dadoslp9f[indiceslp9f, ]
testelp9f <- dadoslp9f[-indiceslp9f, ]

modelo_svmlp9f <- svm(classificacaolp9f ~ notalp9f, data = treinolp9f, kernel = "linear")
predicoeslp9f <- predict(modelo_svmlp9f, newdata = testelp9f)

matriz_confusaolp9f <- table(Predito = predicoeslp9f, Real = testelp9f$classificacaolp9f)
print(matriz_confusaolp9f)

acuracialp9f <- sum(diag(matriz_confusaolp9f)) / sum(matriz_confusaolp9f)
cat("Acurácia:", round(acuracialp9f * 100, 2), "%\n")

testelp9f$predito <- predict(modelo_svmlp9f, newdata = testelp9f)

plotlp9f <- ggplot(testelp9f, aes(x = notalp9f, y = 0, color = predito)) +
  geom_jitter(height = 0.1, width = 0, size = 2, alpha = 0.7) +
  geom_vline(xintercept = c(200, 275, 325), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Abaixo" = "red", "Basico" = "orange", 
                                "Adequado" = "blue", "Avancado" = "darkgreen"), drop = FALSE) +
  labs(title = "Língua Portuguesa - 9° Ano", x = "", y = "", color = "Classificação") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



notama9f <- media_regiao_r[media_regiao_r$SERIEANO == "9F" & media_regiao_r$DSCOMP == "MA", ]
notama9f <- notama9f$MEDPROF
notama9f <- notama9f[notama9f != 0 & !is.na(notama9f)]
classificacaoma9f <- cut(notama9f, breaks = c(-Inf, 225, 300, 350, Inf),
                         labels = c("Abaixo", "Basico", "Adequado", "Avancado"), right = FALSE)
dadosma9f <- data.frame(notama9f, classificacaoma9f)
dadosma9f <- na.omit(dadosma9f)
n <- nrow(dadosma9f)

indicesma9f <- sample(1:n, size = 0.7 * n)
treinoma9f <- dadosma9f[indicesma9f, ]
testema9f <- dadosma9f[-indicesma9f, ]

modelo_svmma9f <- svm(classificacaoma9f ~ notama9f, data = treinoma9f, kernel = "linear")
predicoesma9f <- predict(modelo_svmma9f, newdata = testema9f)

matriz_confusaoma9f <- table(Predito = predicoesma9f, Real = testema9f$classificacaoma9f)
print(matriz_confusaoma9f)

acuraciama9f <- sum(diag(matriz_confusaoma9f)) / sum(matriz_confusaoma9f)
cat("Acurácia:", round(acuraciama9f * 100, 2), "%\n")

testema9f$predito <- predict(modelo_svmma9f, newdata = testema9f)

plotma9f <- ggplot(testema9f, aes(x = notama9f, y = 0, color = predito)) +
  geom_jitter(height = 0.1, width = 0, size = 2, alpha = 0.7) +
  geom_vline(xintercept = c(225, 300, 350), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Abaixo" = "red", "Basico" = "orange", 
                                "Adequado" = "blue", "Avancado" = "darkgreen"), drop = FALSE) +
  labs(title = "Matemática - 9° Ano", x = "", y = "", color = "Classificação") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


graficos_combinados <- ggarrange(plotlp3m, plotma3m, plotlp9f, plotma9f,
                                 common.legend = TRUE, legend = "right")
graficos_combinados <- annotate_figure(graficos_combinados,
                                       top = text_grob(label = "Dispersão das Médias", face = "bold", size = 26))

graficos_combinados