#lendo o df

exploratoria_nova <- respostas

#############EXPLORATÓRIA DA AMOSTRA###################


############1. VARIÁVEIS ISOLADAS ##############

#frequencia em festivais

plot_explo_1 <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = frequencia, fill = "orange")) +
  theme(legend.position = "none") +
  coord_flip()

plot_explo_1

#local

plot_explo_2 <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = local, fill = "orange")) +
  theme(legend.position = "none") +
  coord_flip()

#sexo

plot_explo_3 <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = sexo, fill = "orange")) +
  theme(legend.position = "none") +
  coord_flip()

#gastos

plot_explo_4 <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = gasto, fill = "orange")) +
  theme(legend.position = "none") +
  coord_flip()

#idade

plot_explo_5 <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = idade, fill = "orange")) +
  theme(legend.position = "none") +
  coord_flip()

#região

plot_explo_6 <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = regiao, fill = "orange")) +
  theme(legend.position = "none") +
  coord_flip()

#pacote para posicionar os gráficos em grid

install.packages("gridExtra")
library(gridExtra)

grid.arrange(plot_explo_5, plot_explo_6, plot_explo_3,
             plot_explo_1, plot_explo_2, plot_explo_4, nrow = 3)

#############2. Cruzando pares de variáveis #################

#idade x sexo

pares_idade_sexo <- ggplot(data = exploratoria_nova) + 
  geom_bar(mapping = aes(x = idade, fill = sexo),
           position = "fill") + 
  coord_flip()

pares_idade_sexo

#gasto x frequencia

pares_gasto_frequencia <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = gasto, fill = frequencia),
           position = "fill") +
  coord_flip()

pares_gasto_frequencia

#regiao x local

pares_regiao_local <- ggplot(data = exploratoria_nova) +
  geom_bar(mapping = aes(x = regiao, fill = local),
           position = "fill") +
  coord_flip()

pares_regiao_local

pares_final <- grid.arrange(pares_idade_sexo, pares_gasto_frequencia, pares_regiao_local, nrow = 3)

##################ANÁLISE LIKERT#####################

#pacote para gerar gráficos com dados de escala likert

install.packages("likert")
library(likert)


#convertendo para fatores

exploratoria_likert <- exploratoria_nova

exploratoria_likert[ , 2:6] <- lapply(exploratoria_likert[ , 2:6], function(x){ factor(x, 
                                                                                       levels = c("Irrelevante","Sem muita importância", 
                                                                                                  "Mais ou menos importante", 
                                                                                                  "Muito importante", "Essencial"),                                  
                                                                                       labels = c("Irrelevante","Sem muita importância", 
                                                                                                  "Mais ou menos importante", 
                                                                                                  "Muito importante", "Essencial"))})

##################ANÁLISE EXPECTATIVA ELEMENTOS GERAIS###############

lik_exp <- likert(as.data.frame(exploratoria_likert[ , 2:6]))


plot(lik_exp, wrap = 60, text.size=5) + theme(axis.text.y = element_text(size="10"))

##################ANÁLISE EXPECTATIVA POR SUBGRUPOS##################

#IDADE

lik_exp_idade <- likert(as.data.frame(exploratoria_likert[ , 2:6]), grouping = exploratoria_likert$idade)
plot(lik_exp_idade, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))


#REGIÃO


lik_exp_regiao <- likert(as.data.frame(exploratoria_likert[ , 2:6]), grouping = exploratoria_likert$regiao)
plot(lik_exp_regiao, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#FREQUÊNCIA


lik_exp_frequencia <- likert(as.data.frame(exploratoria_likert[ , 2:6]), grouping = exploratoria_likert$frequencia)
plot(lik_exp_frequencia, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#GASTO

lik_exp_gasto <- likert(as.data.frame(exploratoria_likert[ , 2:6]), grouping = exploratoria_likert$gasto)
plot(lik_exp_gasto, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#SEXO

lik_exp_sexo <- likert(as.data.frame(exploratoria_likert[ , 2:6]), grouping = exploratoria_likert$sexo)
plot(lik_exp_sexo, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#LOCAL

lik_exp_local <- likert(as.data.frame(exploratoria_likert[ , 2:6]), grouping = exploratoria_likert$local)
plot(lik_exp_local, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))


##################ANÁLISE REALIDADE ELEMENTOS GERAIS###############

exploratoria_likert[ , 7:11] <- lapply(exploratoria_likert[ , 7:11], 
                                       function(x){factor(x, levels = c("Péssimo", "Ruim",
                                                                        "Regular", "Bom",
                                                                        "Ótimo"),
                                                          labels = c("Péssimo", "Ruim",
                                                                     "Regular", "Bom",
                                                                     "Ótimo")
                                       )
                                         
                                       }
)



lik_real <- likert(as.data.frame(exploratoria_likert[ , 7:11]))

plot(lik_real, wrap = 60, text.size=6) + theme(axis.text.y = element_text(size="10"))

#IDADE

lik_real_idade <- likert(as.data.frame(exploratoria_likert[ , 7:11]), grouping = exploratoria_likert$idade)
plot(lik_real_idade, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#REGIÃO

lik_real_regiao <- likert(as.data.frame(exploratoria_likert[ , 7:11]), grouping = exploratoria_likert$regiao)
plot(lik_real_regiao, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#FREQUÊNCIA

lik_real_frequencia <- likert(as.data.frame(exploratoria_likert[ , 7:11]), grouping = exploratoria_likert$frequencia)
plot(lik_real_frequencia, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#GASTO

lik_real_gasto <- likert(as.data.frame(exploratoria_likert[ , 7:11]), grouping = exploratoria_likert$gasto)
plot(lik_real_gasto, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

#SEXO

lik_real_sexo <- likert(as.data.frame(exploratoria_likert[ , 7:11]), grouping = exploratoria_likert$sexo)
plot(lik_real_sexo, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))


#LOCAL

lik_real_local <- likert(as.data.frame(exploratoria_likert[ , 7:11]), grouping = exploratoria_likert$local)
plot(lik_real_local, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))
