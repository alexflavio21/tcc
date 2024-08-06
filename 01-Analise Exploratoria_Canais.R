# Instalação e carregamento dos pacotes utilizados

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "fBasics", #pacote de estatísitica básica
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
	           "correlation","see", "viridis"
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","Rcpp","equatiomatic")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregando o pacote de leitura do excel
library(readxl)

#-----Criano a função CV (Coeficiente de variação)--------
cv = function(variavel){
  coef_var = (sd(variavel, na.rm = T)/mean(variavel, na.rm = T))*100
  return(coef_var)
}


#lendo o arquivo .xls com dados
canais <- read_excel("Canaletas.xlsx")

#----Visualização dos dados--------
names(canais) #nomes das colunas
head(canais)  #print das primeiras linhas
tail(canais)  #print das últimas linhas

glimpse(canais)


# Transformando a variável LITO em FACTOR
canais$LITO <- factor(canais$LITO)


# Visualização da base de dados
canais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

#Visualização do mapa
plot(
  canais$X,
  canais$Y,
  main = "Amostras de MN",
  xlab = "X (m - Local)",
  ylab = "Y (m - Local)",
  asp = 1,
  pch = 16,
  cex = 1, 
  col = "red"
)

# Gráfico 3D com scatter
rownames(canais) <- canais$AMOSTRA

scatter3D(x=canais$Mn,
          y=canais$Fe,
          z=canais$SiO2,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Mn",
          ylab = "Fe",
          zlab = "SiO2",
          main = "Canais",
          clab = "Mn")>
  text3D(x=canais$Mn,
         y=canais$Fe,
         z=canais$SiO2,
         labels = rownames(canais),
         add = TRUE, cex = 1)

# Estatísticas descritivas TOTAIS
summary(canais)

# Fazer o histograma e para análise visual da distrubuição dos dados por LITO 
par(mfrow=c(1,2))
histPlot(as.timeSeries(canais$Mn [canais$LITO == 1],
     ylab = "Frequência", xlab = "Mn", main = "LITO 01"))
histPlot(as.timeSeries(canais$Mn [canais$LITO == 2],
     ylab = "Frequência", xlab = "Mn", main = "LITO 02"))


#Exploração visual do valor de MN médio
ggplotly(
  canais %>%
    group_by(LITO) %>%
    mutate(Mn_medio = mean(Mn, na.rm = TRUE)) %>% 
    ggplot() +
    geom_point(aes(x = LITO, y = Mn),color = "blue", alpha = 0.5, size = 4) +
    geom_line(aes(x = LITO, y = Mn_medio, 
                  group = 1, color = "Mn Médio"), size = 1.5) +
    scale_colour_viridis_d() +
    labs(x = "LITOLOGIA",
         y = "Mn Médio") +
    theme(legend.title = element_blank(),
          panel.border = element_rect(NA),
          panel.grid = element_line("grey"),
          panel.background = element_rect("white"),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90))
)


#Estatística descritiva para Mn  segmentado por LITO  (1 e 2)
group_by(canais, LITO) %>%
  summarise(
    mean = mean(Mn, na.rm = TRUE),
    median = median(Mn, na.rm = TRUE),
    sd = sd(Mn, na.rm = TRUE),
    min = min(Mn, na.rm = TRUE),
    max = max(Mn, na.rm = TRUE),
    obs = n())


# Boxplots por variável 
par(mfrow=c(1,1))
ggplotly(
  ggplot(canais,aes(x=LITO, y=Mn)) +
    geom_boxplot(fill=viridis_pal(begin=0.7)(0.15), width = 0.5) +
    stat_summary(fun=mean, geom = "point", col="yellow", shape= 8) +
    labs(x = "LITO",
         y = "MN",
         title = "Boxplot para variável Mn") +
    theme_ggside_classic()
)

cv = data.frame(stat_Mn_1 = cv(canais$Mn [canais$LITO == 1]),
                stat_Fe_1 = cv(canais$Fe [canais$LITO == 1]),
                stat_Si_1 = cv(canais$SiO2 [canais$LITO == 1]),
                stat_P_1 = cv(canais$P [canais$LITO == 1]),
                stat_Al_1 = cv(canais$Al2O3 [canais$LITO == 1]),
                stat_MGO_1 = cv(canais$MGO [canais$LITO == 1]),
                stat_NA2O_1 = cv(canais$NA2O [canais$LITO == 1]),
                stat_K2O_1 = cv(canais$K2O [canais$LITO == 1]),
                stat_Mn_2 = cv(canais$Mn [canais$LITO == 2]),
                stat_Fe_2 = cv(canais$Fe [canais$LITO == 2]),
                stat_Si_2 = cv(canais$SiO2 [canais$LITO == 2]),
                stat_P_2 = cv(canais$P [canais$LITO == 2]),
                stat_Al_2 = cv(canais$Al2O3 [canais$LITO == 2]),
                stat_MGO_2 = cv(canais$MGO [canais$LITO == 2]),
                stat_NA2O_2 = cv(canais$NA2O [canais$LITO == 2]),
                stat_K2O_2 = cv(canais$K2O [canais$LITO == 2])
                )

estat_final = data.frame (stat_Mn_1 = basicStats(canais$Mn [canais$LITO == 1]),
                          stat_Fe_1 = basicStats(canais$Fe [canais$LITO == 1]),
                          stat_Si_1 = basicStats(canais$SiO2 [canais$LITO == 1]),
                          stat_P_1 = basicStats(canais$P [canais$LITO == 1]),
                          stat_Al_1 = basicStats(canais$Al2O3 [canais$LITO == 1]),
                          stat_MGO_1 = basicStats(canais$MGO [canais$LITO == 1]),
                          stat_NA2O_1 = basicStats(canais$NA2O [canais$LITO == 1]),
                          stat_K2O_1 = basicStats(canais$K2O [canais$LITO == 1]),
                          stat_Mn_2 = basicStats(canais$Mn [canais$LITO == 2]),
                          stat_Fe_2 = basicStats(canais$Fe [canais$LITO == 2]),
                          stat_Si_2 = basicStats(canais$SiO2 [canais$LITO == 2]),
                          stat_P_2 = basicStats(canais$P [canais$LITO == 2]),
                          stat_Al_2 = basicStats(canais$Al2O3 [canais$LITO == 2]),
                          stat_MGO_2 = basicStats(canais$MGO [canais$LITO == 2]),
                          stat_NA2O_1 = basicStats(canais$NA2O [canais$LITO == 2]),
                          stat_K2O_2 = basicStats(canais$K2O [canais$LITO == 2])
                          )

names(estat_final) <- c ('stat_Mn_1', 'stat_Fe_1','stat_Si_1', 'stat_P_1', 'stat_Al_1', 'stat_MGO_1', 'stat_NA2O_1','stat_K2O_1',
                        'stat_Mn_2', 'stat_Fe_2','stat_Si_2', 'stat_P_2', 'stat_Al_2', 'stat_MGO_2', 'stat_NA2O_2','stat_K2O_2')

estat_completo = rbind(estat_final, cv)


dados_canais = data.frame(MN = canais$Mn,  
                           FE = canais$Fe,
                           SIO2 = canais$SiO2,
                           AL2O3 =canais$Al2O3,
                           P = canais$P,
                           MGO = canais$MGO,
                           NA2O = canais$NA2O,
                           K2O = canais$K2O,
                           CAO = canais$CAO,
                           LITO= canais$LITO)

#transformando novamente o variável LITO em Numérica (para que a proxima função funcione)
dados_canais$LITO <- as.numeric(dados_canais$LITO)

#Plotagem de todos os Histogramas e Boxplots para as variáveis de interesse
lst1 <- lapply (names(dados_canais),function(i) {hist(dados_canais[,i],20, 
                                                       col="lightblue", 
                                                       main=paste0("Histogram of ",i),
                                                       xlab=i);boxplot(dados_canais[,i],
                                                                       col="green",
                                                                       main=paste0("Boxplot of ",i),
                                                                       ylab = i)})
#Salvando os dados para estudos posteriores
save(canais, file = "canais.RData")
