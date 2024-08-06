# Instalação e carregamento dos pacotes utilizados

install.packages("tidyverse")

library(tidyverse)

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Carregando a base de dados
load(file = "canais.RData")


glimpse(canais)


# Visualização da base de dados
canais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Gráfico 3D com scatter
scatter3D(x=canais$Mn,
          y=canais$Fe,
          z=canais$SiO2,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Mn",
          ylab = "Fe",
          zlab = "SiO2",
          main = "AMOSTRA",
          clab = "Mn")>
  text3D(x=canais$Mn,
         y=canais$Fe,
         z=canais$SiO2,
         labels = rownames(canais),
         add = TRUE, cex = 1)

# Estatísticas descritivas
summary(canais)

#Boxplot para os dados de minério
bp <-  boxplot(canais$Mn ~ canais$LITO,
               main = "Boxplot para teores de Mn por Lito",
               ylab = "Teores de Mn",
               xlab = "Litologias",
               col = "green",
               pch = 16)

#Estatística de cada boxplot por lito
#Informações em cada linha (1 = lim.inf. , 2= 1 quart, 3= mediana, 4 = 3 quart., 5 = lim. sup.)
bp$stats

#Estatística de MN por LITO  (outra maneira de visualizar as informações)
group_by(canais, LITO) %>%
  summarise(
    mean = mean(Mn, na.rm = TRUE),
    median = median(Mn, na.rm = TRUE),
    sd = sd(Mn, na.rm = TRUE),
    min = min(Mn, na.rm = TRUE),
    max = max(Mn, na.rm = TRUE),
    obs = n())

## Como as variáveis Não estão na mesma unidade de medida, vamos padronizar

canais_padronizado <- as.data.frame(scale(canais[,10:18]))


## Todas as variáveis passam a ter média = 0 e desvio padrão = 1. Por exemplo:

round(mean(canais_padronizado$Mn,  na.rm = TRUE),3)
round(sd(canais_padronizado$Mn, na.rm = TRUE),3)

round(mean(canais_padronizado$Fe,na.rm = TRUE),3)
round(sd(canais_padronizado$Fe, na.rm = TRUE),3)

round(mean(canais_padronizado$K2O,na.rm = TRUE),3)
round(sd(canais_padronizado$Fe, na.rm = TRUE),3)

#---------- Esquema de aglomeração hierárquico ---------------------------------

# Matriz de dissimilaridades
matriz_D <- canais_padronizado %>% 
  dist(method = "euclidean")

# Method: parametrização da distância a ser utilizada

## "euclidean": distância euclidiana
## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
## "maximum": distância de Chebychev;
## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
## "canberra": distância de Canberra;
## "minkowski": distância de Minkowski

# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# 1º Teste: Elaboração da clusterização hierárquica
cluster_hier_single <- agnes(x = matriz_D, method = "single")

# O input é a matriz de distâncias obtida anteriormente

# Method é o tipo de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier_single, show_labels = F)


# 2º Teste: Elaboração da clusterização hierárquica como "complete linkage"
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")

# Construção do dendrograma "complete linkage"
dev.off()
fviz_dend(x = cluster_hier_complete, show_labels = F)


# 3º Teste: Elaboração da clusterização hierárquica como "average linkage"
cluster_hier_average <- agnes(x = matriz_D, method = "average")

# Construção do dendrograma "average linkage"
dev.off
fviz_dend(x = cluster_hier_average, show_labels = F)


## Vamos optar pelo complete linkage (average cria clusters com menos observações)
# Dendrograma com visualização dos clusters (definição de 3 clusters - k pode se h)
dev.off()
fviz_dend(x = cluster_hier_complete,
          h = 12,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())


# Definição do esquema hierárquico de aglomeração

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier_complete$height, decreasing = FALSE) 
coeficientes

# Tabela com o esquema de aglomeração. Interpretação do output:

## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio

esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)


# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
canais$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 5))
canais_padronizado$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 5))

# Visualização da base de dados com a alocação das observações nos clusters
canais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)


# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos

summary(anova_mn <- aov(formula = Mn ~ cluster_H,
                                data = canais_padronizado))

summary(anova_fe <- aov(formula = Fe ~ cluster_H,
                             data = canais_padronizado))

summary(anova_sio2 <- aov(formula = SiO2 ~ cluster_H,
                            data = canais_padronizado))

summary(anova_al2o3 <- aov(formula = Al2O3 ~ cluster_H,
                             data = canais_padronizado))

summary(anova_p <- aov(formula = P ~ cluster_H,
                            data = canais_padronizado))

summary(anova_mgo <- aov(formula = MGO ~ cluster_H,
                       data = canais_padronizado))

summary(anova_na2o <- aov(formula = NA2O ~ cluster_H,
                       data = canais_padronizado))

summary(anova_k2o <- aov(formula = K2O ~ cluster_H,
                               data = canais_padronizado))

summary(anova_cao <- aov(formula = CAO ~ cluster_H,
                       data = canais_padronizado))


## Todas auxiliam na formação de pelo menos um cluster

# O que os cluster indicam? Vamos interpretar algumas variáveis médias:

análise <- group_by(canais, cluster_H) %>%
  summarise(Mn = mean(Mn, na.rm = TRUE),
            Fe = mean(Fe, na.rm = TRUE),
            SiO2 = mean(SiO2, na.rm = TRUE),
            Al2O3 = mean(Al2O3, na.rm = TRUE),
            P = mean(P, na.rm = TRUE),
            MGO = mean(MGO, na.rm = TRUE),
            NA2O = mean(NA2O, na.rm = TRUE),
            K2O = mean(K2O, na.rm = TRUE),
            CAO = mean(CAO, na.rm = TRUE))

#---------- Esquema de aglomeração não hierárquico K-MEANS ---------------------
# Método de Elbow para identificação do número ótimo de clusters
## Apresenta a variação total dentro dos clusters para várias nº de clusters
## Em geral, quando há a dobra é um indício do número ótimo de clusters
fviz_nbclust(canais_padronizado[,1:9], kmeans, method = "wss", k.max = 10)


# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(canais_padronizado[,1:9],
                         centers = 4)
## centers: parametrização da quantidade de clusters


# Criando variável categórica para indicação do cluster no banco de dados
canais$cluster_K <- factor(cluster_kmeans$cluster)
canais_padronizado$cluster_K <- factor(cluster_kmeans$cluster)

# Visualização da base de dados
canais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Análise de variância de um fator (ANOVA)

summary(anova_mn <- aov(formula = Mn ~ cluster_K,
                        data = canais_padronizado))

summary(anova_fe <- aov(formula = Fe ~ cluster_K,
                         data = canais_padronizado))

summary(anova_sio2 <- aov(formula = SiO2 ~ cluster_K,
                          data = canais_padronizado))

summary(anova_al2o3 <- aov(formula = Al2O3 ~ cluster_K,
                           data = canais_padronizado))

summary(anova_p <- aov(formula = P ~ cluster_K,
                       data = canais_padronizado))

summary(anova_mgo <- aov(formula = MGO ~ cluster_K,
                         data = canais_padronizado))

summary(anova_na2o <- aov(formula = NA2O ~ cluster_K,
                          data = canais_padronizado))

summary(anova_k2o <- aov(formula = K2O ~ cluster_K,
                         data = canais_padronizado))

summary(anova_cao <- aov(formula = CAO ~ cluster_K,
                         data = canais_padronizado))

# Comparando os resultados dos esquemas hierárquico e não hierárquico
canais %>%
  select(LITO, cluster_H, cluster_K) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Vamos interpretar algumas variáveis médias (Classificadas via k-means):
análise_K <- group_by(canais, cluster_K) %>%
  summarise(Mn = mean(Mn, na.rm = TRUE),
            Fe = mean(Fe, na.rm = TRUE),
            SiO2 = mean(SiO2, na.rm = TRUE),
            Al2O3 = mean(Al2O3, na.rm = TRUE),
            P = mean(P, na.rm = TRUE),
            MGO = mean(MGO, na.rm = TRUE),
            NA2O = mean(NA2O, na.rm = TRUE),
            K2O = mean(K2O, na.rm = TRUE),
            CAO = mean(CAO, na.rm = TRUE))

#Salvando os dados para estudos posteriores
save(canais, file = "cluster_canais.RData")
# FIM!