### --- ANÁLISE DE AUTOCORRELAÇÃO ESPACIAL ---

# Pre-sets
rm(list = ls())

# Pacotes
pacotes <- c("rgdal",
             "tmap",
             "maptools",
             "tidyverse",
             "broom",
             "knitr",
             "kableExtra",
             "RColorBrewer", 
             "spdep", 
             "gtools",
             "PerformanceAnalytics",    # Matriz de correlação
             "spgwr",
             "reshape2",
             "ggpubr",
             "grid"
             )

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# DADOS ----
dados <- read.csv2(file = "dados_bcg.csv", sep = ';', dec = ',')

glimpse(dados)


## TRATAMENTO DE DADOS ----

dados$COD <- as.character(dados$COD)

# Número de NA
map_int(dados, ~ sum(is.na(.)))

# dados$cv_bcg <- replace(x = dados$cv_bcg, 
#                               list = is.na(dados$cv_bcg),
#                               values = 0)

# CV > 100
sum(dados$BCG_2019 > 100)
sum(dados$BCG_2020 > 100)
sum(dados$BCG_2021 > 100)
sum(dados$BCG_2022 > 100)

# Transformar valores > 100 em 100%
dados$BCG_2019 <- ifelse(dados$BCG_2019 > 100, 100, dados$BCG_2019)
dados$BCG_2020 <- ifelse(dados$BCG_2020 > 100, 100, dados$BCG_2020)
dados$BCG_2021 <- ifelse(dados$BCG_2021 > 100, 100, dados$BCG_2021)
dados$BCG_2022 <- ifelse(dados$BCG_2022 > 100, 100, dados$BCG_2022)

## ANÁLISE EXPLORATÓRIA ----

summary(dados)

# Distribuição da variável resposta
dados %>% 
  ggplot() +
  geom_histogram(aes(x = BCG_2019),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "CV BCG",
       y = "Frequência") +
  theme_bw()


dados %>% 
  ggplot() +
  geom_histogram(aes(x = BCG_2020),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "CV BCG",
       y = "Frequência") +
  theme_bw()


dados %>% 
  ggplot() +
  geom_histogram(aes(x = BCG_2021),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "CV BCG",
       y = "Frequência") +
  theme_bw()


dados %>% 
  ggplot() +
  geom_histogram(aes(x = BCG_2022),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "CV BCG",
       y = "Frequência") +
  theme_bw()

# Boxplot
df_long <- reshape2::melt(dados[,3:6])

nomes_colunas <- c("2019", "2020", "2021", "2022")
df_long$variable <- factor(df_long$variable, labels = nomes_colunas)

boxplot <- ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "grey88") +
  labs(x = "Ano", y = "CV BCG") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  scale_x_discrete(labels = nomes_colunas) +
    theme_bw()

ggsave("plot2_boxplot.png", plot = boxplot, width = 8, height = 6, dpi = 300)

# Matriz de correlação
# chart.Correlation(dados[3:14], histogram = TRUE, method = "spearman")


# ANÁLISE ESPACIAL ----

# Carregando um shapefile detrabalhado
shp_br <- readOGR(dsn = "BR_Municipios_2022", layer = "BR_Municipios_2022")

summary(shp_br)


shp_br@data <- shp_br@data %>% mutate(COD6 = str_sub(shp_br@data$CD_MUN, 0, 6))

# Combinar o shapefile com o data.frame
shp_dados <- merge(x = shp_br,
                   y = dados,
                   by.x = "COD6",
                   by.y = "COD")

head(shp_dados@data)


# Mapa temático CV BCG
# criando uma coluna para agrupar os valores
shp_dados@data$cv_escala19 <- cut(shp_dados@data$BCG_2019, c(0.00, 49.9, 89.9, 100), include.lowest = T)
shp_dados@data$cv_escala20 <- cut(shp_dados@data$BCG_2020, c(0.00, 49.9, 89.9, 100), include.lowest = T)
shp_dados@data$cv_escala21 <- cut(shp_dados@data$BCG_2021, c(0.00, 49.9, 89.9, 100), include.lowest = T)
shp_dados@data$cv_escala22 <- cut(shp_dados@data$BCG_2022, c(0.00, 49.9, 89.9, 100), include.lowest = T)

labels <- c("<50%", 
            "50% a 89,9%",
            "≥90%")

mapa1 <- tm_shape(shp = shp_dados) +
  tm_fill(col = "cv_escala19",
          palette = "magma",
          force.limits = TRUE,
          labels = labels,
          colorNA = "white",
          textNA = "Valores ausentes",
          title = "2019") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))


mapa2 <- tm_shape(shp = shp_dados) +
  tm_fill(col = "cv_escala20",
          palette = "magma",
          force.limits = TRUE,
          labels = labels,
          colorNA = "white",
          textNA = "Valores ausentes",
          title = "2020") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))


mapa3 <- tm_shape(shp = shp_dados) +
  tm_fill(col = "cv_escala21",
          palette = "magma",
          force.limits = TRUE,
          labels = labels,
          colorNA = "white",
          textNA = "Valores ausentes",
          title = "2021") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))


mapa4 <- tm_shape(shp = shp_dados) +
  tm_fill(col = "cv_escala22",
          palette = "magma",
          force.limits = TRUE,
          labels = labels,
          colorNA = "white",
          textNA = "Valores ausentes",
          title = "2022") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))

mapas <- tmap_arrange(mapa1, mapa2, mapa3, mapa4, nrow = 2, ncol = 2)
tmap_save(mapas, filename = "mapas1.png", width = 10, height = 8, dpi = 300)


#####  ANÁLISE ESPACIAL  #######
# Observações com o mesmo nome
shp_dados@data$NM_MUN[duplicated(shp_dados@data$NM_MUN)]

shp_dados@data %>% 
  filter(NM_MUN == "MUNDO NOVO")


# Acrescentar a sigla do Estado ao nome do município:
shp_dados@data <- shp_dados@data %>% 
  mutate(city = paste(NM_MUN, SIGLA_UF))

# Estabelecendo uma vizinhança:
vizinhos_queen <- poly2nb(pl = shp_dados,
                          queen = TRUE,
                          row.names = shp_dados@data$city)

summary(vizinhos_queen)


# Definindo uma matriz de vizinhanças com padronização em linha:
matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = T)

# No caso, a ilha diz respeito ao município de Ilhabela. Para contornar a
# situação, podemos utilizar o argumento zero.policy = TRUE.
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B",
                        zero.policy = TRUE) # Com esse argumento o R faz a matriz mesmo com ilhas


# Observando a matriz de contiguidades, critério queen, padronizada em linha:
colnames(matrizW_queen_linha) <- rownames(matrizW_queen_linha)

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_queen) <- shp_dados@data$NM_MUN

sum(is.na(shp_dados@data$BCG_2019))
sum(is.na(shp_dados@data$BCG_2020))
sum(is.na(shp_dados@data$BCG_2021))
sum(is.na(shp_dados@data$BCG_2022))

shp_dados@data$BCG_2019 <- ifelse(is.na(shp_dados@data$BCG_2019), 0, shp_dados@data$BCG_2019)
shp_dados@data$BCG_2020 <- ifelse(is.na(shp_dados@data$BCG_2020), 0, shp_dados@data$BCG_2020)
shp_dados@data$BCG_2021 <- ifelse(is.na(shp_dados@data$BCG_2021), 0, shp_dados@data$BCG_2021)
shp_dados@data$BCG_2022 <- ifelse(is.na(shp_dados@data$BCG_2022), 0, shp_dados@data$BCG_2022)

# Autocorrelação Global – a Estatística I de Moran ------------------------

# Para o cálculo da Estatística I de Moran, nosso algoritmo esperará como
# declaração um objeto de classe listw. Como exemplificação, voltaremos a 
# utilizar o objeto matrizW_queen:
listw_queen <- mat2listw(matrizW_queen_linha)

# Após isso, poderemos utilizar a função moran.test():

# 2019
moranI_19 <- moran.test(x = shp_dados@data$BCG_2019, 
                     listw = listw_queen,
                     zero.policy = T)

mor19 <- tidy(moranI_19)


# 2020
moranI_20 <- moran.test(x = shp_dados@data$BCG_2020, 
                        listw = listw_queen,
                        zero.policy = T)

mor20 <- tidy(moranI_20)


# 2021
moranI_21 <- moran.test(x = shp_dados@data$BCG_2021, 
                        listw = listw_queen,
                        zero.policy = T)

mor21 <- tidy(moranI_21)


# 2020
moranI_22 <- moran.test(x = shp_dados@data$BCG_2022, 
                        listw = listw_queen,
                        zero.policy = T)

mor22 <- tidy(moranI_22)


resultado <- rbind(mor19, mor20, mor21, mor22)
Ano <- c("2019", "2020", "2021", "2022")
resultado <- cbind(Ano, resultado)
print(resultado)


# O Diagrama da Estatística I de Moran ------------------------------------
moran.plot(x = shp_dados@data$BCG_2019, 
                         listw = listw_queen, 
                         zero.policy = TRUE,
                         main = "2019",
                         xlab = "CV BCG", 
                         ylab = "Spatially lagged CV BCG",
                         pch = 19)


moran.plot(x = shp_dados@data$BCG_2020, 
                           listw = listw_queen, 
                           zero.policy = TRUE,
                           main = "2020",
                           xlab = "CV BCG", 
                           ylab = "Spatially lagged CV BCG",
                           pch = 19)


moran.plot(x = shp_dados@data$BCG_2021, 
                           listw = listw_queen, 
                           zero.policy = TRUE,
                           main = "2021",
                           xlab = "CV BCG", 
                           ylab = "Spatially lagged CV BCG",
                           pch = 19)


moran.plot(x = shp_dados@data$BCG_2022, 
                           listw = listw_queen, 
                           zero.policy = TRUE,
                           main = "2022",
                           xlab = "CV BCG", 
                           ylab = "Spatially lagged CV BCG",
                           pch = 19)



# Autocorrelação Local – a Estatística Moran Local ------------------------


######## 2019

# Considerando a variável poverty do objeto shp_co, podemos aferir sua 
# Estatística Moran Local, com o uso da função localmoran(), como se segue:
moran_local_19 <- localmoran(x =shp_dados@data$BCG_2019, 
                          listw = listw_queen,
                          zero.policy = T)


# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_dados:
moran_local_mapa_19 <- cbind(shp_dados, moran_local_19)


quantile(moran_local_mapa_19@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))


# Plotando a Estatística Moran Local de forma espacial, sem problemas na escala
# de cores:
moran_local_mapa_19@data <- moran_local_mapa_19@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5)))


mapa_lisa1 <- tm_shape(shp = moran_local_mapa_19) +
  tm_fill(col = "faixa_quantis",
          palette = "magma",
          title = "Quantis") +
  tm_layout(title = "2019",
            frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



######## 2020

# Considerando a variável poverty do objeto shp_co, podemos aferir sua 
# Estatística Moran Local, com o uso da função localmoran(), como se segue:
moran_local_20 <- localmoran(x =shp_dados@data$BCG_2020, 
                          listw = listw_queen,
                          zero.policy = T)


# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_dados:
moran_local_mapa_20 <- cbind(shp_dados, moran_local_20)


quantile(moran_local_mapa_20@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))


# Plotando a Estatística Moran Local de forma espacial, sem problemas na escala
# de cores:
moran_local_mapa_20@data <- moran_local_mapa_20@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5)))


mapa_lisa2 <- tm_shape(shp = moran_local_mapa_20) +
  tm_fill(col = "faixa_quantis",
          palette = "magma",
          title = "Quantis") +
  tm_layout(title = "2020",
            frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



######## 2021

# Considerando a variável poverty do objeto shp_co, podemos aferir sua 
# Estatística Moran Local, com o uso da função localmoran(), como se segue:
moran_local_21 <- localmoran(x =shp_dados@data$BCG_2021, 
                          listw = listw_queen,
                          zero.policy = T)


# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_dados:
moran_local_mapa_21 <- cbind(shp_dados, moran_local_21)


quantile(moran_local_mapa_21@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))


# Plotando a Estatística Moran Local de forma espacial, sem problemas na escala
# de cores:
moran_local_mapa_21@data <- moran_local_mapa_21@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5)))


mapa_lisa3 <- tm_shape(shp = moran_local_mapa_21) +
  tm_fill(col = "faixa_quantis",
          palette = "magma",
          title = "Quantis") +
  tm_layout(title = "2021",
            frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



######## 2022

# Considerando a variável poverty do objeto shp_co, podemos aferir sua 
# Estatística Moran Local, com o uso da função localmoran(), como se segue:
moran_local_22 <- localmoran(x =shp_dados@data$BCG_2022, 
                          listw = listw_queen,
                          zero.policy = T)


# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_dados:
moran_local_mapa_22 <- cbind(shp_dados, moran_local_22)


quantile(moran_local_mapa_22@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))


# Plotando a Estatística Moran Local de forma espacial, sem problemas na escala
# de cores:
moran_local_mapa_22@data <- moran_local_mapa_22@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5)))


mapa_lisa4 <- tm_shape(shp = moran_local_mapa_22) +
  tm_fill(col = "faixa_quantis",
          palette = "magma",
          title = "Quantis") +
  tm_layout(title = "2022",
            frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))


mapas_lisa <- tmap_arrange(mapa_lisa1, mapa_lisa2, mapa_lisa3, mapa_lisa4, nrow = 2, ncol = 2)
tmap_save(mapas_lisa, filename = "mapas_lisa.png", width = 10, height = 8, dpi = 300)



# Estabelecendo uma Clusterização LISA ------------------------------------

#### 2019

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes_19 <- vector(mode = "numeric", length = nrow(moran_local_19))

quadrantes_19

# Criando um vetor que contenha o centro das observações da variável poverty ao 
# redor de sua média:
centro_contatos_19 <- shp_dados@data$BCG_2019 - mean(shp_dados@data$BCG_2019)

centro_contatos_19

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_19 <- moran_local_19[,1] - mean(moran_local_19[,1])

centro_moran_local_19

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes_19[centro_contatos_19 > 0 & centro_moran_local_19 > 0] <- "Alto-Alto"
quadrantes_19[centro_contatos_19 > 0 & centro_moran_local_19 < 0] <- "Alto-Baixo"
quadrantes_19[centro_contatos_19 < 0 & centro_moran_local_19 > 0] <- "Baixo-Alto"
quadrantes_19[centro_contatos_19 < 0 & centro_moran_local_19 < 0] <- "Baixo-Baixo"

quadrantes_19

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes_19[moran_local_19[,5] > sig] <- "Não_significante"

quadrantes_19

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_19@data["quadrantes_19"] <- factor(quadrantes_19)


# Mapa
mapa_lisa_clust1 <- tm_shape(shp = moran_local_mapa_19) +
  tm_fill(col = "quadrantes_19",
          pal = c(`Alto-Alto` = "blue",
                  `Alto-Baixo` = "lightskyblue", 
                  `Baixo-Alto` = "coral", 
                  `Baixo-Baixo` = "red",
                  Não_significante = "white"),
          title = "Escala") +
  tm_layout(main.title = "2019",
    frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



#### 2020

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes_20 <- vector(mode = "numeric", length = nrow(moran_local_20))

quadrantes_20

# Criando um vetor que contenha o centro das observações da variável poverty ao 
# redor de sua média:
centro_contatos_20 <- shp_dados@data$BCG_2020 - mean(shp_dados@data$BCG_2020)

centro_contatos_20

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_20 <- moran_local_20[,1] - mean(moran_local_20[,1])

centro_moran_local_20

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes_20[centro_contatos_20 > 0 & centro_moran_local_20 > 0] <- "HH"
quadrantes_20[centro_contatos_20 > 0 & centro_moran_local_20 < 0] <- "HL"
quadrantes_20[centro_contatos_20 < 0 & centro_moran_local_20 > 0] <- "LH"
quadrantes_20[centro_contatos_20 < 0 & centro_moran_local_20 < 0] <- "LL"

quadrantes_20

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes_20[moran_local_20[,5] > sig] <- "Não_significante"

quadrantes_20

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_20@data["quadrantes_20"] <- factor(quadrantes_20)


# Mapa
mapa_lisa_clust2 <- tm_shape(shp = moran_local_mapa_20) +
  tm_fill(col = "quadrantes_20",
          pal = c(HH = "blue",
                  HL = "lightskyblue", 
                  LH = "coral", 
                  LL = "red",
                  Não_significante = "white"),
          title = "CV 2020") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



#### 2021

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes_21 <- vector(mode = "numeric", length = nrow(moran_local_21))

quadrantes_21

# Criando um vetor que contenha o centro das observações da variável poverty ao 
# redor de sua média:
centro_contatos_21 <- shp_dados@data$BCG_2021 - mean(shp_dados@data$BCG_2021)

centro_contatos_21

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_21 <- moran_local_21[,1] - mean(moran_local_21[,1])

centro_moran_local_21

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes_21[centro_contatos_21 > 0 & centro_moran_local_21 > 0] <- "HH"
quadrantes_21[centro_contatos_21 > 0 & centro_moran_local_21 < 0] <- "HL"
quadrantes_21[centro_contatos_21 < 0 & centro_moran_local_21 > 0] <- "LH"
quadrantes_21[centro_contatos_21 < 0 & centro_moran_local_21 < 0] <- "LL"

quadrantes_21

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes_21[moran_local_21[,5] > sig] <- "Não_significante"

quadrantes_21

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_21@data["quadrantes_21"] <- factor(quadrantes_21)


# Mapa
mapa_lisa_clust3 <- tm_shape(shp = moran_local_mapa_21) +
  tm_fill(col = "quadrantes_21",
          pal = c(HH = "blue",
                  HL = "lightskyblue", 
                  LH = "coral", 
                  LL = "red",
                  Não_significante = "white"),
          title = "CV 2021") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



#### 2022

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes_22 <- vector(mode = "numeric", length = nrow(moran_local_22))

quadrantes_22

# Criando um vetor que contenha o centro das observações da variável poverty ao 
# redor de sua média:
centro_contatos_22 <- shp_dados@data$BCG_2022 - mean(shp_dados@data$BCG_2022)

centro_contatos_22

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local_22 <- moran_local_22[,1] - mean(moran_local_22[,1])

centro_moran_local_22

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes_22[centro_contatos_22 > 0 & centro_moran_local_22 > 0] <- "HH"
quadrantes_22[centro_contatos_22 > 0 & centro_moran_local_22 < 0] <- "HL"
quadrantes_22[centro_contatos_22 < 0 & centro_moran_local_22 > 0] <- "LH"
quadrantes_22[centro_contatos_22 < 0 & centro_moran_local_22 < 0] <- "LL"

quadrantes_22

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes_22[moran_local_22[,5] > sig] <- "Não_significante"

quadrantes_22

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa_22@data["quadrantes_22"] <- factor(quadrantes_22)


# Mapa
mapa_lisa_clust4 <- tm_shape(shp = moran_local_mapa_22) +
  tm_fill(col = "quadrantes_22",
          pal = c(HH = "blue",
                  HL = "lightskyblue", 
                  LH = "coral", 
                  LL = "red",
                  Não_significante = "white"),
          title = "CV 2022") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



mapas_lisa_clusters <- tmap_arrange(mapa_lisa_clust1, mapa_lisa_clust2, mapa_lisa_clust3, mapa_lisa_clust4, nrow = 2, ncol = 2)
tmap_save(mapas_lisa_clusters, filename = "mapas_lisa_clusters.png", width = 10, height = 8, dpi = 300)


######
# Exploração Moran LISA

moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'HH')

### Clusters HH - 2019
df_HH19 <- moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'HH') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

df_HH19

# Cluster 1
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'HH', SIGLA_UF %in% c('TO', 'GO')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

# Cluster 2
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'HH', SIGLA_UF %in% c('CE', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

# Cluster 3
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'HH', SIGLA_UF %in% c('MG', 'ES', 'RJ', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

# Cluster 4
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'HH', SIGLA_UF %in% c('PR', 'SC', 'RS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)


### Clusters LL - 2019
df_LL19 <- moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'LL') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

df_LL19

# Cluster 1
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'LL', SIGLA_UF %in% c('RO', 'TO', 'MS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

# Cluster 2
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'LL', SIGLA_UF %in% c('PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

# Cluster 3
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'LL', SIGLA_UF %in% c('MG', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)

# Cluster 4
moran_local_mapa_19@data %>% dplyr::filter(quadrantes_19 == 'LL', SIGLA_UF %in% c('PR', 'SC', 'RS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2019, cv_escala19)


#################################

### Clusters HH - 2020
df_HH20 <- moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'HH') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

df_HH20

# Cluster 1
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'HH', SIGLA_UF %in% c('AM', 'TO', 'MT', 'MS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

# Cluster 2
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'HH', SIGLA_UF %in% c('MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

# Cluster 3
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'HH', SIGLA_UF %in% c('MG', 'ES', 'RJ', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

# Cluster 4
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'HH', SIGLA_UF %in% c('PR', 'SC', 'RS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)


### Clusters LL - 2020
df_LL20 <- moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'LL') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

df_LL20

# Cluster 1
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'LL', SIGLA_UF %in% c('PA', 'TO', 'MT', 'GO')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

# Cluster 2
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'LL', SIGLA_UF %in% c('PI', 'CE', 'RN', 'PE', 'AL', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

# Cluster 3
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'LL', SIGLA_UF %in% c('MG', 'ES', 'RJ', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)

# Cluster 4
moran_local_mapa_20@data %>% dplyr::filter(quadrantes_20 == 'LL', SIGLA_UF %in% c('PR', 'SC', 'RS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2020, cv_escala20)



#################################

### Clusters HH - 2021
df_HH21 <- moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'HH') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

df_LL21

# Cluster 1
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'HH', SIGLA_UF %in% c('RO', 'AM', 'PA', 'TO', 'MT', 'MS', 'GO')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

# Cluster 2
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'HH', SIGLA_UF %in% c('MA', 'PI', 'CE', 'RN', 'PE', 'SE', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

# Cluster 3
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'HH', SIGLA_UF %in% c('MG', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

# Cluster 4
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'HH', SIGLA_UF %in% c('PR', 'SC', 'RS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)


### Clusters LL - 2119
df_LL21 <- moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'LL') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

df_LL21

# Cluster 1
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'LL', SIGLA_UF %in% c('TO', 'MS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

# Cluster 2
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'LL', SIGLA_UF %in% c('MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

# Cluster 3
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'LL', SIGLA_UF %in% c('MG', 'RJ', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)

# Cluster 4
moran_local_mapa_21@data %>% dplyr::filter(quadrantes_21 == 'LL', SIGLA_UF %in% c('PR', 'SC', 'RS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2021, cv_escala21)




#################################

### Clusters HH - 2022
df_HH22 <- moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'HH') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

df_HH22

# Cluster 1
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'HH', SIGLA_UF %in% c('PA', 'TO', 'MT')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

# Cluster 2
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'HH', SIGLA_UF %in% c('PI', 'CE', 'RN', 'PE', 'AL', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

# Cluster 3
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'HH', SIGLA_UF %in% c('MG', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

# Cluster 4
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'HH', SIGLA_UF %in% c('PR', 'SC')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)


### Clusters LL - 2022
df_LL22 <- moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'LL') %>% 
  select(COD6, NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

df_LL22

# Cluster 1
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'LL', SIGLA_UF %in% c('PA', 'MS', 'MT', 'GO')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

# Cluster 2
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'LL', SIGLA_UF %in% c('MA', 'PB', 'PE', 'AL', 'SE', 'BA')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

# Cluster 3
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'LL', SIGLA_UF %in% c('MG', 'RJ', 'SP')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)

# Cluster 4
moran_local_mapa_22@data %>% dplyr::filter(quadrantes_22 == 'LL', SIGLA_UF %in% c('PR', 'SC', 'RS')) %>% 
  select(NM_MUN, SIGLA_UF, BCG_2022, cv_escala22)



# Join Alto-Alto
join_HH1 <- left_join(df_HH19, df_HH20, by = "COD6")
join_HH1

join_HH2 <- inner_join(df_HH20, df_HH21, by = "COD6")
join_HH2

join_HH3 <- inner_join(df_HH21, df_HH22, by = "COD6")
join_HH3



# Join Baixo-Baixo
join_LL1 <- inner_join(df_LL19, df_LL20, by = "COD6")
join_LL1

join_LL2 <- inner_join(df_LL20, df_LL21, by = "COD6")
join_LL2

join_LL3 <- inner_join(df_LL21, df_LL22, by = "COD6")
join_LL3
