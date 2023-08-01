### --- ANÁLISE DE SÉRIES TEMPORAIS ---

# Pre-sets
rm(list = ls())

# Pacotes
pacotes <- c(
             "tidyverse",
             "forecast",
             "tsibble", 
             "fable", 
             "tsibbledata", 
             "urca",
             "tseries",
             "FinTS",
             "TSdist",
             "ggpubr", 
             "broom",
             "kableExtra",
             "textreg",
             "officer",
             "data.table",
             "flextable",
             "trend",
             "Kendall",
             "seastests"
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
dados_st <- read.csv2(file = "dados_st.csv", sep = ';', dec = ',')

glimpse(dados_st)

# Série temporal do Brasil
ts_bcg <- ts(dados_st[29], start = c(2019, 1), end = c(2022, 12), frequency = 12)

# Plot
ts_bcg %>%
  autoplot()

# Análise descritiva
summary(ts_bcg)
sd(ts_bcg)
length(ts_bcg)

sum(ts_bcg)

# Análises de tendência e sazonalidade
sens.slope(ts_bcg, conf.level = 0.95) # Tste para variação na tendência
Kendall::MannKendall(ts_bcg) # Teste de tendência
kw(ts_bcg) # Teste de sazonalidade

##############################################
## Decomposição da série temporal
##############################################

# Decomposição pelo modelo ADITIVO
decompa=decompose(ts_bcg,type = "additive")
plot(decompa)

decompa$trend
decompa$seasonal
decompa$random

# Decomposição pelo modelo MULTIPLICATIVO
decompm=decompose(ts_bcg,type = "multiplicative")
plot(decompm)

decompm$trend
decompm$seasonal
decompm$random

plot1 <- autoplot(decompm) +
  labs(x = "Meses", title = "") +
  labs(title = "A") +
  theme_bw()

plot2 <- ggseasonplot(ts_bcg, labels = "both") +
  labs(x = "Meses", y = "Doses aplicadas", title = "B") +
  theme_bw() +
  guides(colour = guide_legend(title = "Séries temporais")) +
  theme(legend.position = "bottom")

monthplot(ts_bcg, ylab = "Doses aplicadas", xlab = "Meses", col.base = 1, lty.base = 2, lwd.base = 2)


plot_decomp <- ggarrange(plot1, plot2, ncol = 1)
ggsave("plot_decomp.png", width = 8, height = 8, dpi = 300)


##############################################
## Análise de modelos
##############################################

# separar a base de dados em uma janela para criar o modelo (dados de treino) e outra para prever (dados de teste)
treino = window(ts_bcg, start = c(2019, 1), end = c(2022, 3))
length(treino)

teste = window(ts_bcg, start = c(2022,4), end = c(2022,12))
length(teste)


#### fazendo as previsões e calculando a estatística MAPE de qualidade das previsões

# fazendo a previsão pelo alisamento exponencial simples
ses = ses(treino, h = 9)
(prevses = ses$mean)
(qualises = forecast::accuracy(prevses, teste))
(qualises = forecast::accuracy(prevses, teste)[5])
plot(ses)


# fazendo a previsão pelo Holt-Winters Sazonal Aditivo
(hwadd = hw(treino, h = 9, seasonal = "additive"))
(phwadd = hwadd$mean)
(qualihwa=forecast::accuracy(phwadd, teste)[5])
plot(hwadd)


# fazendo a previsão pelo Holt-Winters Sazonal Multiplicativo
(hwmult = hw(treino, h = 9, seasonal = "multiplicative"))
(phwmult = hwmult$mean)
(qualihwm = forecast::accuracy(phwmult, teste)[5])
plot(hwmult)


# Usando modelo ETS
# EXPONENTIAL SMOOTHING

# E (Error) T (Trend) S (Season)
# Erro: aditivo (A) ou multiplicativo (M)
# Tendência: nenhuma (N), aditiva (A), multiplicativa (M) ou amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

# Usando ETS
# N=none, A=additive, M=multiplicative e Z=automatic

bcg_ets <- ets(treino, model = "ZZZ")
summary(bcg_ets)
bcg_ets_forecasts <- forecast.ets(bcg_ets, h = 9)
summary(bcg_ets_forecasts)
forecast::accuracy(bcg_ets_forecasts$mean, teste)
(qualiets = forecast::accuracy(bcg_ets_forecasts$mean, teste)[5])

# Analisando os resíduos (erros) das previsões
# Condições:
# não podem ser correlacionados; se forem correlacionados ficaram informações
# nos resíduos que deveriam estar no modelo
# devem possui média zero, caso não seja então as previsões são viesadas

autoplot(bcg_ets$residuals)
acf(bcg_ets$residuals)

# Teste de Ljung-box
# H0: os resíduos são iid (modelo não exibe falhas de ajustes)
# H1: os resíduos não são iid (modelo exibe falhas de ajustes)
# não quero rejeitar H0 (quero um p-valor grande)

Box.test(bcg_ets$residuals, lag = 1, type = c("Ljung-Box"))

autoplot(bcg_ets_forecasts) +
  autolayer(teste)


##################################################
## Analisando as séries autoregressivas
##################################################
## Realização dos testes de Estacionariedade
## Precisamos do pacote URCA - Unit Root and Cointegration Test
###################################################################

# Teste de Dickey-Fuller
# H0: A série Não é Estacionária
# H1: A série é Estacionária

testeDF = ur.df(treino)
testeDF
summary(testeDF)

# Conclusão: p-value 0.52101 > 0.05 (95% confiança) - NÃO REJEITO H0,
# portanto a série não é estacionária

###################################################################################
### Estimação de um modelo ARIMA - Escolher, p, q e d
###################################################################################

ggtsdisplay(treino)

acf(treino)
pacf(treino, lag.max = 10)

# Quantas diferenciações seriam necessárias para tornar a série estacionária?
ndiffs(treino)

treino_dif <- diff(treino)
(testeDF_dif = ur.df(treino_dif))
summary(testeDF_dif)

ggtsdisplay(treino_dif)

acf(treino_dif)
pacf(treino_dif)


#####################################################################################
# modelos ARIMA com Sazonalidade - SARIMA, possui os parâmetros P, D e Q Sazonais.
# Fica SARIMA(p,d,q)(P,D,Q)
#####################################################################################

arimabcg = auto.arima(treino, trace = T)
summary(arimabcg)

#### validação e diagnóstico

checkresiduals(arimabcg)

# 1. teste de Ljung-Box p-value = 0.02099 < 0.05, rejeitamos H0, resíduos são correlacionados

# 2. Normalidade dos resíduos
ks.test(arimabcg$residuals, "pnorm", mean(arimabcg$residuals),
        sd(arimabcg$residuals))
# p-valor = 0.1182 > 0,05 - Não rejeito H0, ou seja, resíduos têm distribuição normal

# confirmada a existência de autocorrelação serial e normalidade dos resíduos
# Podemos verificar a estacionariedade de variância
# verificar se existe efeitos ARCH

ArchTest(arimabcg$residuals)

# p-valor 0.9733 > 0,05, não se rejeita a H0, garante não existência de efeitos ARCH

## Previsao para a série de BCG Brasil
prevbcg = forecast::forecast(arimabcg, h = 9)

autoplot(prevbcg) +
  autolayer(arimabcg$fitted) +
  autolayer(teste) +
  theme_bw()

forecast::accuracy(prevbcg, teste)
(qualiarima <- forecast::accuracy(prevbcg, teste)[2, 5])


# Analisando as acurácias das previsões
modelos = c("SES","HW_ADD", "HW_MULT", "ETS", "SARIMA")
mape = c(qualises, qualihwa, qualihwm, qualiets, qualiarima)
(tabela = data.frame(modelos, mape))


#####################################################################################
# modelo final ARIMA com Sazonalidade - SARIMA, possui os parâmetros P, D e Q Sazonais.
# Fica SARIMA(0,1,0)(1,1,0)
#####################################################################################

ggtsdisplay(ts_bcg)

ggAcf(ts_bcg)
ggPacf(ts_bcg)

ndiffs(ts_bcg)
testeDF = ur.df(ts_bcg)
summary(testeDF)

tab_DF <- as.data.frame(testeDF@testreg$coefficients)

tab_DF %>% 
  flextable() %>% 
  width(width = 2) %>% 
  style(pr_t = fp_text(font.size = 12,
                       font.family = 'Calibri'), part = "body") %>% 
  style(pr_t = fp_text(font.size = 12,
                       font.family = 'Calibri'), part = "header") %>% 
  bold(part = "header") %>% 
  bold(part = "body", j = 1) %>% 
  hline(border = fp_border(color = "black", width = 0.5)) %>% 
  align(align = "center", part = "all") %>% 
  bg(bg = "grey", part = "header")


bcg_dif <- diff(ts_bcg)

ggAcf(bcg_dif)
ggPacf(bcg_dif)

arimabcg_fin = auto.arima(ts_bcg, trace=T)
summary(arimabcg_fin)

trace <- capture.output({
  # assign so it doesn't pollute the output
  model <- auto.arima(ts_bcg, trace = T)
})

tab_trace <- as.data.frame(trace)


tab_trace %>% 
  flextable() %>% 
  width(width = 2) %>% 
  style(pr_t = fp_text(font.size = 12,
                       font.family = 'Calibri'), part = "body") %>% 
  style(pr_t = fp_text(font.size = 12,
                       font.family = 'Calibri'), part = "header") %>% 
  bold(part = "header") %>% 
  bold(part = "body", j = 1) %>% 
  hline(border = fp_border(color = "black", width = 0.5)) %>% 
  align(align = "center", part = "all") %>% 
  bg(bg = "grey", part = "header")


arimabcg_fin = Arima(ts_bcg, order = c(0,1,0), seasonal = c(0,1,0))
summary(arimabcg_fin)

#### validação e diagnóstico
checkresiduals(arimabcg_fin)

# 1. teste de Ljung-Box p-value = 0.2917 > 0.05, não rejeitamos H0, resíduos não são correlacionados

# 2. Normalidade dos resíduos
ks.test(arimabcg_fin$residuals, "pnorm", mean(arimabcg_fin$residuals),
        sd(arimabcg_fin$residuals))
# p-valor = 0.1554 > 0,05 - Não rejeito H0, ou seja, resíduos normais

# confirmada a existência de autocorrelação serial e normalidade dos resíduos
# Podemos verificar a estacionariedade de variância
# verificar se existe efeitos ARCH

ArchTest(arimabcg_fin$residuals)

# p-valor 0.9733 > 0,05, não se rejeita a H0, garante não existência
# de efeitos ARCH

## Previsão para a série de BCG Brasil
prevbcg_fin = forecast::forecast(arimabcg_fin, h = 12)

(
  plot_ts1 <- autoplot(prevbcg_fin) +
  autolayer(prevbcg_fin$mean, series = 'Predito') +
  autolayer(ts_bcg, series = 'Observado') +
  autolayer(arimabcg_fin$fitted, series = 'Ajustado') +
  labs(x = 'Mês', y = 'Doses aplicadas', title = "A") +
  theme_bw() +
  guides(colour = guide_legend(title = "Séries temporais")) +
    scale_color_manual(values = c("red", "black", "blue"))
  )


####################################################
## Análise com tratamento de outliers
####################################################

# Análise de outliers
bcg_outlier <- window(ts_bcg, start = c(2022, 1), end = c(2022, 12))
plot(bcg_outlier)

ts_outlier <- ts(dados_st[2:28], start = c(2022, 1), end = c(2022, 12), frequency = 12)
which.max(bcg_outlier)

(
  outliers <- ts_bcg |>
  stats::filter(
    ts_bcg < quantile(ts_bcg, 0.25) - 3*IQR(ts_bcg) |
      ts_bcg > quantile(ts_bcg, 0.75) + 3*IQR(ts_bcg)
  )
)


# Os dados de selecionados como outliers se deveu a uma a´licação anormal de doses
# da vacina BCG no ano de 2022, que se deveu a um possível desabastecimento
ts_interpolada <- ts_bcg
ts_interpolada[c(41, 42, 43)] <- NA

# Interpolação dos dados por transformação de Box-Cox
ts_interpolada <- na.interp(ts_interpolada, lambda = "auto") 

plot(ts_interpolada)


### Interpolação ARIMA ----------
ggtsdisplay(ts_interpolada)

acf(ts_interpolada)
pacf(ts_interpolada)

ndiffs(ts_interpolada)

(testeDF = ur.df(ts_interpolada))
summary(testeDF)

bcg_dif2 <- diff(ts_interpolada)

acf(bcg_dif2)
pacf(bcg_dif2)


######################################
### Plot ACF e PACF de todas as séries
plt1 <- ggAcf(ts_bcg) +
  theme_bw() +
  labs(title = "A")

plt2 <- ggPacf(ts_bcg) + 
  theme_bw() +
  labs(title = "B")

plt3 <- ggAcf(bcg_dif) +
  theme_bw() +
  labs(title = "C")

plt4 <- ggPacf(bcg_dif) + 
  theme_bw() +
  labs(title = "D")

plt5 <- ggAcf(ts_interpolada) +
  theme_bw() +
  labs(title = "E")

plt6 <- ggPacf(ts_interpolada) + 
  theme_bw() +
  labs(title = "F")

plt7 <- ggAcf(bcg_dif2) +
  theme_bw() +
  labs(title = "G")

plt8 <- ggPacf(bcg_dif2) + 
  theme_bw() +
  labs(title = "H")

plot_acf_pacf <- ggarrange(plt1, plt2, plt3, plt4, plt5, plt6, plt7, plt8, ncol = 2, nrow = 4)
ggsave("plot_acf_pacf.png", width = 8, height = 10, dpi = 300)

######################################


arimabcg_interp = auto.arima(ts_interpolada, trace=T)
summary(arimabcg_interp)

arimabcg_interp = Arima(ts_interpolada, order = c(0,1,0), seasonal = c(1,1,0))
summary(arimabcg_interp)
#### validação e diagnóstico

checkresiduals(arimabcg_interp)


# 1. teste de Ljung-Box p-value = 0.2917 > 0.05, não rejeitamos H0, resíduos não são
# correlacionados

# 2. Normalidade dos resíduos
ks.test(arimabcg_interp$residuals, "pnorm", mean(arimabcg_interp$residuals),
        sd(arimabcg_interp$residuals))
# p-valor = 0.1554 > 0,05 - Não rejeito H0, ou seja, resíduos normais

# confirmada a existência de autocorrelação serial e normalidade dos resíduos
# Podemos verificar a estacionariedade de variância
# verificar se existe efeitos ARCH

ArchTest(arimabcg_interp$residuals)

# p-valor 0.9733 > 0,05, não se rejeita a H0, garante não existência
# de efeitos ARCH

## Previsao para a série de BCG Brasil

prevbcg_interp = forecast::forecast(arimabcg_interp, h = 12)

(
  plot_ts2 <- autoplot(prevbcg_interp) +
    autolayer(prevbcg_interp$mean, series = 'Predito') +
    autolayer(ts_interpolada, series = 'Observado') +
    autolayer(arimabcg_interp$fitted, series = 'Ajustado') +
    labs(x = 'Mês', y = 'Doses aplicadas', title = "B") +
    theme_bw() +
    guides(colour = guide_legend(title = "Séries temporais"),
           fill = guide_legend(title = "Intervalo de predição")) +
    scale_color_manual(values = c("red", "black", "blue"))
)


ts_sinasc <- ts(dados_st$Sinasc, start = c(2019, 1), end = c(2022, 12), frequency = 12)
prev_sinasc <- snaive(ts_sinasc, h = 12)
plot(prev_sinasc)


plot_ts3 <- autoplot(prev_sinasc) +
  autolayer(prev_sinasc$mean, series = 'Predito') +
  autolayer(ts_sinasc, series = 'Observado') +
  autolayer(prev_sinasc$fitted, series = 'Ajustado') +
  labs(x = 'Mês', y = 'Nascidos vivos', title = "C") +
  theme_bw() +
  guides(colour = guide_legend(title = "Séries temporais"),
         fill = guide_legend(title = "Intervalo de predição")) +
  scale_color_manual(values = c("red", "black", "blue"))


plot_ts4 <- autoplot(prevbcg_interp$mean) +
  autolayer(ts_interpolada, series = 'ST c/ interpolação') +
  autolayer(ts_bcg, series = 'ST original') +
  autolayer(ts_sinasc, series = "ST Sinasc") +
  autolayer(prevbcg_interp$mean, series = 'Predito ST c/ interpolação') +
  autolayer(prevbcg_fin$mean, series = 'Predito ST original') +
  autolayer(prev_sinasc$mean, series = 'Predito Sinasc') +
  labs(x = 'Mês', y = 'Frequência absoluta', title = "D") +
  theme_bw() +
  guides(colour = guide_legend(title = "Séries temporais")) +
  scale_color_manual(values = c("green3", "blue", "purple", "deeppink", "black", "darkorange"))


plot_st_final_interp <- ggarrange(plot_ts1, plot_ts2, plot_ts3, plot_ts4, ncol = 1, nrow = 4)
ggsave("plot_st_final.png", width = 8, height = 8, dpi = 300)

### Análise da similaridade entre as predições
prev1 <- as.vector(prevbcg_fin$mean)
prev2 <- as.vector(prevbcg_interp$mean)
prev3 <- as.vector(prev_sinasc$mean)

Mod1_euclidean <- round(TSDistances(log(prev1), log(prev3), distance = "euclidean"), 3)
Mod2_euclidean <- round(TSDistances(log(prev2), log(prev3), distance = "euclidean"), 3)

Mod1_dtw <- round(TSDistances(log(prev1), log(prev3), distance = "dtw"), 3)
Mod2_dtw <- round(TSDistances(log(prev2), log(prev3), distance = "dtw"), 3)


Metrica = c("Euclidiana", "DTW")
`ARIMA(0,1,0)(0,1,0)[12]` = c(Mod1_euclidean, Mod1_dtw)
`ARIMA(0,1,0)(1,1,0)[12]` = c(Mod2_euclidean, Mod2_dtw)
(tabela2 = data.frame(Metrica, `ARIMA(0,1,0)(0,1,0)[12]`, `ARIMA(0,1,0)(1,1,0)[12]`))
