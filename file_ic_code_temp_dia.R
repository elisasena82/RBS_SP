#IMPORTANDO DADOS
library(readxl) #LER EXCEL
library(data.table) #CONSTRUIR DATA FRAMES
library(reshape2)
library(ggplot2)
library(evir) #RECORDES

files_temperatura = list.files("C:\\Users\\User\\Documents\\CA - Unifesp\\Iniciação Científica\\Dados IC\\Planilhas_Dados_Temp", pattern=".xls", full.names=T)

#CRIANDO TABELA BASE MEDIA
temp_dia_med = NULL
for (i in seq_along(files_temperatura)) {
 
  df_serie_cronol_med = read_excel(files_temperatura[i], sheet = 13, range = "B3:N33", col_names = F)
  names(df_serie_cronol_med)[names(df_serie_cronol_med)=="...1"]="dia"
  
  long_temp_dia_med = melt(df_serie_cronol_med, id = c("dia"))
  long_temp_dia_med$ANO = 1935 + i
  long_temp_dia_med = long_temp_dia_med[,c(4,2,1,3)]
  names(long_temp_dia_med)[names(long_temp_dia_med)=="variable"]="mes"
  names(long_temp_dia_med)[names(long_temp_dia_med)=="value"]="temp"
  
  temp_dia_med = rbind(temp_dia_med, long_temp_dia_med)
}
temp_dia_med = na.omit(temp_dia_med)

sapply(long_temp_dia_med, class)
temp_dia_med$mes = as.numeric(temp_dia_med$mes)
temp_dia_med$dia = as.numeric(temp_dia_med$dia)

#DATA FRAME TE MAX ORGANIZATION 
names(temp_dia_med)[names(temp_dia_med) == "...1"] = 1
names(temp_dia_med)[names(temp_dia_med) == "...2"] = 2
names(temp_dia_med)[names(temp_dia_med) == "...3"] = 3
names(temp_dia_med)[names(temp_dia_med) == "...4"] = 4
names(temp_dia_med)[names(temp_dia_med) == "...5"] = 5
names(temp_dia_med)[names(temp_dia_med) == "...6"] = 6
names(temp_dia_med)[names(temp_dia_med) == "...7"] = 7
names(temp_dia_med)[names(temp_dia_med) == "...8"] = 8
names(temp_dia_med)[names(temp_dia_med) == "...9"] = 9
names(temp_dia_med)[names(temp_dia_med) == "...10"] = 10
names(temp_dia_med)[names(temp_dia_med) == "...11"] = 11
names(temp_dia_med)[names(temp_dia_med) == "...12"] = 12

#PLOTANDO DISTRIBUICAO MED TEMPERATURA DIARIA
df_med_61_90 = subset(temp_dia_med, subset = ANO>1960 & ANO<1991 , select = c("ANO", "mes", "dia", "temp"))
hist(temp_dia_med$temp, col = "blue", breaks = 20 , ylim=NULL, main = NULL, xlab = "TEMPERATURA DIARIA")

#CRIANDO TABELA BASE MAXIMA
temp_dia_max = NULL
for (i in seq_along(files_temperatura)) {
  
  df_serie_cronol_max = read_excel(files_temperatura[i], sheet = 14, range = "B3:N33", col_names = F)
  names(df_serie_cronol_max)[names(df_serie_cronol_max)=="...1"]="dia"
  
  long_temp_dia_max = melt(df_serie_cronol_max, id = c("dia"))
  long_temp_dia_max$ANO = 1935 + i
  long_temp_dia_max = long_temp_dia_max[,c(4,2,1,3)]
  names(long_temp_dia_max)[names(long_temp_dia_max)=="variable"]="mes"
  names(long_temp_dia_max)[names(long_temp_dia_max)=="value"]="temp"
  
  temp_dia_max = rbind(temp_dia_max, long_temp_dia_max)
}
temp_dia_max = na.omit(temp_dia_max)


sapply(long_temp_dia_max, class)
temp_dia_max$mes = as.numeric(temp_dia_max$mes)
temp_dia_max$dia = as.numeric(temp_dia_max$dia)


#DATA FRAME TE MAX ORGANIZATION 
names(temp_dia_max)[names(temp_dia_max) == "...1"] = 1
names(temp_dia_max)[names(temp_dia_max) == "...2"] = 2
names(temp_dia_max)[names(temp_dia_max) == "...3"] = 3
names(temp_dia_max)[names(temp_dia_max) == "...4"] = 4
names(temp_dia_max)[names(temp_dia_max) == "...5"] = 5
names(temp_dia_max)[names(temp_dia_max) == "...6"] = 6
names(temp_dia_max)[names(temp_dia_max) == "...7"] = 7
names(temp_dia_max)[names(temp_dia_max) == "...8"] = 8
names(temp_dia_max)[names(temp_dia_max) == "...9"] = 9
names(temp_dia_max)[names(temp_dia_max) == "...10"] = 10
names(temp_dia_max)[names(temp_dia_max) == "...11"] = 11
names(temp_dia_max)[names(temp_dia_max) == "...12"] = 12


#PLOTANDO DISTRIBUICAO MAX TEMPERATURA DIARIA
df_max_61_90 = subset(temp_dia_max, subset = ANO>1960 & ANO<1991, select = c("ANO", "mes", "dia", "temp"))
hist(temp_dia_max$temp, col = "blue", breaks = 20 , ylim=NULL, main = NULL, xlab = "TEMPERATURA DIARIA")


#CRIANDO TABELA BASE MINIMA
temp_dia_min = NULL
for (i in seq_along(files_temperatura)) {
  
  df_serie_cronol_min = read_excel(files_temperatura[i], sheet = 15, range = "B3:N33", col_names = F)
  names(df_serie_cronol_min)[names(df_serie_cronol_min)=="...1"]="dia"
  
  long_temp_dia_min = melt(df_serie_cronol_min, id = c("dia"))
  long_temp_dia_min$ANO = 1935 + i
  long_temp_dia_min = long_temp_dia_min[,c(4,2,1,3)]
  names(long_temp_dia_min)[names(long_temp_dia_min)=="variable"]="mes"
  names(long_temp_dia_min)[names(long_temp_dia_min)=="value"]="temp"
  
  temp_dia_min = rbind(temp_dia_min, long_temp_dia_min)
}
temp_dia_min = na.omit(temp_dia_min)


sapply(long_temp_dia_min, class)
temp_dia_min$mes = as.numeric(temp_dia_min$mes)
temp_dia_min$dia = as.numeric(temp_dia_min$dia)


#DATA FRAME TE MIN ORGANIZATION 
names(temp_dia_min)[names(temp_dia_min) == "...1"] = 1
names(temp_dia_min)[names(temp_dia_min) == "...2"] = 2
names(temp_dia_min)[names(temp_dia_min) == "...3"] = 3
names(temp_dia_min)[names(temp_dia_min) == "...4"] = 4
names(temp_dia_min)[names(temp_dia_min) == "...5"] = 5
names(temp_dia_min)[names(temp_dia_min) == "...6"] = 6
names(temp_dia_min)[names(temp_dia_min) == "...7"] = 7
names(temp_dia_min)[names(temp_dia_min) == "...8"] = 8
names(temp_dia_min)[names(temp_dia_min) == "...9"] = 9
names(temp_dia_min)[names(temp_dia_min) == "...10"] = 10
names(temp_dia_min)[names(temp_dia_min) == "...11"] = 11
names(temp_dia_min)[names(temp_dia_min) == "...12"] = 12


#PLOTANDO DISTRIBUICAO MIN TEMPERATURA DIARIA
df_min_61_90 = subset(temp_dia_min, subset = ANO>1960 & ANO<1991, select = c("ANO", "mes", "dia", "temp"))
hist(temp_dia_min$temp, col = "blue", breaks = 20 , ylim=NULL, main = NULL)

#PONTO 1
tn10p = quantile(df_min_61_90$temp, 0.10)
df_tn10p_dias = subset(temp_dia_min, subset = temp<10.4, select = c("ANO","temp"))
df_tn10p_ano = aggregate(df_tn10p_dias$temp, by = list(ANO=df_tn10p_dias$ANO), FUN = length)
names(df_tn10p_ano)[names(df_tn10p_ano)=="x"]="length"

#PONTO 2
tn90p = quantile(df_min_61_90$temp, 0.90)
df_tn90p_dias = subset(temp_dia_min, subset = temp>18.8, select = c("ANO","temp"))
df_tn90p_ano = aggregate(df_tn90p_dias$temp, by = list(ANO=df_tn90p_dias$ANO), FUN = length)
names(df_tn90p_ano)[names(df_tn90p_ano)=="x"]="length"

#PONTO 3
tx10p = quantile(df_max_61_90$temp, 0.10)
df_tx10p_dias = subset(temp_dia_max, subset = temp<18.4, select = c("ANO","temp"))
df_tx10p_ano = aggregate(df_tx10p_dias$temp, by = list(ANO=df_tx10p_dias$ANO), FUN = length)
names(df_tx10p_ano)[names(df_tx10p_ano)=="x"]="length"

#PONTO 4
tx90p = quantile(df_max_61_90$temp, 0.90)
df_tx90p_dias = subset(temp_dia_max, subset = temp>30.4, select = c("ANO","temp"))
df_tx90p_ano = aggregate(df_tx90p_dias$temp, by = list(ANO=df_tx90p_dias$ANO), FUN = length)
names(df_tx90p_ano)[names(df_tx90p_ano)=="x"]="length"


#RECORDES PARA CIMA
num_rec_med_high = records(temp_dia_med$temp, do.plot = F)
num_rec_max_high = records(temp_dia_max$temp, do.plot = F)
num_rec_min_high = records(temp_dia_min$temp, do.plot = F)

#RECORDES PARA BAIXO
num_rec_med_low = records(-1*temp_dia_med$temp, do.plot = F)
num_rec_max_low = records(-1*temp_dia_max$temp, do.plot = F)
num_rec_min_low = records(-1*temp_dia_min$temp, do.plot = F)

#SIMETRIA RECORDES
s_med = log(tail(num_rec_med_high$number, n = 1)/tail(num_rec_med_low$number, n = 1))
s_max = log(tail(num_rec_max_high$number, n = 1)/tail(num_rec_max_low$number, n = 1))
s_min = log(tail(num_rec_min_high$number, n = 1)/tail(num_rec_min_low$number, n = 1))
