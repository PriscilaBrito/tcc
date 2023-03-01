# Carregando a base de dados
expectativa <- read_csv("~/MBA USP Data Science e Analytics/TCC/Análise/avaliacao_expectativa.csv")

expectativa

# Apresentando os dados
expectativa %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#frequências
summary(expectativa)

# Estabelecendo uma tabela de contingências
tab_expectativa <- table(expectativa$avaliacao,
                         expectativa$elemento)

tab_expectativa

#outra tabela de contingências
sjt.xtab(var.row = expectativa$avaliacao,
         var.col = expectativa$elemento,
         show.exp = TRUE) #inclui valores esperados


# Teste Qui-Quadrado
qui2_exp <- chisq.test(tab_expectativa)
qui2_exp

# Apresentando o Mapa de Calor dos Resíduos Padronizados Ajustados
data.frame(qui2_exp$stdres) %>%
  rename(avaliacao = 1,
         elemento = 2) %>%
  mutate(avaliacao = fct_relevel(avaliacao, "Irrelevante", 
                                 "Sem muita importância", "Mais ou menos importante", 
                                 "Muito importante", "Essencial")) %>%
  ggplot(aes(x = elemento, y = avaliacao, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none")

#####cálculos intermediários

#Decomposição da inércia principal total
It <- qui2_exp$statistic/nrow(expectativa)
It

#Construindo a matriz P
P <- 1/nrow(expectativa) * tab_expectativa
P

#Column profile
data.frame(tab_expectativa) %>% 
  group_by(Var2) %>% 
  summarise(Var1 = Var1,
            Massas = Freq / sum(Freq)) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") %>% 
  round(., digits = 3)

column_profile <- apply(tab_expectativa, MARGIN = 1, FUN = sum) / nrow(expectativa)
column_profile

#Row profiles
data.frame(tab_expectativa) %>% 
  group_by(Var1) %>% 
  summarise(Var2 = Var2,
            Massas = Freq / sum(Freq)) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") %>% 
  round(., digits = 3)

row_profile <- apply(tab_expectativa, MARGIN = 2, FUN = sum) / nrow(expectativa)
row_profile

#Matriz Dl
Dl <- diag(column_profile)
Dl

#Matriz Dc
Dc <- diag(row_profile)
Dc

#Matriz lc'
lc <- column_profile %o% row_profile
lc

#Matriz A
A <- diag(diag(Dl) ^ (-1/2)) %*% (P - lc) %*% diag(diag(Dc) ^ (-1/2))
A

#Curiosidade:
A_matriz <- qui2_exp$residuals / sqrt(nrow(expectativa))
A_matriz

#Matriz W
W_matriz <- t(A_matriz) %*% A_matriz
W_matriz

#Extraindo os eigenvalues da matriz W
eigenvalues <- eigen(W_matriz)
eigenvalues

sum(eigenvalues$values) #It
It

#Dimensionalidade dos dados
dimensoes <- min(nrow(A_matriz) - 1, ncol(A_matriz) - 1)
dimensoes

#Percentual da Inércia Total explicada
It_explicada <- eigenvalues$values[1:3] / It
It_explicada

#Cálculo das coordenadas do mapa perceptual

#Decomposição do valor singular da matriz A
decomp <- svd(x = A_matriz,
              nu = dimensoes,
              nv = dimensoes)

decomp

#Variável em linha - coordenada no eixo das abcissas
Xl_perfil <- diag((decomp$d[1]) * diag(diag(Dl)^(-1/2)) * decomp$u[,1])
Xl_perfil

#Variável em linha - coordenada no eixo das ordenadas
Yl_perfil <- diag((decomp$d[2]) * diag(diag(Dl)^(-1/2)) * decomp$u[,2])
Yl_perfil

#Variável em coluna - coordenada no eixo das abcissas
Xc_aplicacao <- diag((decomp$d[1]) * diag(diag(Dc)^(-1/2)) * decomp$v[,1])
Xc_aplicacao

#Variável em coluna - coordenada no eixo das ordenadas
Yc_aplicacao <- diag((decomp$d[2]) * diag(diag(Dc)^(-1/2)) * decomp$v[,2])
Yc_aplicacao


# Interpondo a ANACOR
anacor_expectativa <- CA(tab_expectativa)

# Mapa Perceptual Elegante
# Capturando todas as coordenadas num só objeto
ca_coordenadas <- rbind(anacor_expectativa$row$coord, anacor_expectativa$col$coord)
ca_coordenadas

# Capturando a quantidade de categorias por variável
id_var <- apply(expectativa[,1:2],
                MARGIN =  2,
                FUN = function(x) nlevels(as.factor(x)))
id_var

# Juntando as coordenadas e as categorias capturadas anteriormente
ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))

ca_coordenadas_final

# Mapa perceptual
ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Category, 
             fill = Variable,
             color = Variable,
             shape = Variable)) +
  geom_point(size = 2) +
  geom_label_repel(max.overlaps = 100,
                   size = 3,
                   color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor_expectativa$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor_expectativa$eig[2,2], digits = 2), "%"))) +
  scale_fill_viridis_d(option = "turbo") +
  scale_color_viridis_d(option = "turbo") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")
