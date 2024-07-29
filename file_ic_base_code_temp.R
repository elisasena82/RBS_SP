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
#CORRECOES TEMP_DIA_MED
temp_dia_med = na.omit(temp_dia_med)
temp_dia_med$temp = round(temp_dia_med$temp, 1)


#ALTERACAO DE CLASSE
sapply(long_temp_dia_med, class)
temp_dia_med$mes = as.numeric(temp_dia_med$mes)
temp_dia_med$dia = as.numeric(temp_dia_med$dia)

#DATA FRAME TE MAX ORGANIZATION 
#names(temp_dia_med)[names(temp_dia_med) == "...2"] = 1
#names(temp_dia_med)[names(temp_dia_med) == "...3"] = 2
#names(temp_dia_med)[names(temp_dia_med) == "...4"] = 3
#names(temp_dia_med)[names(temp_dia_med) == "...5"] = 4
#names(temp_dia_med)[names(temp_dia_med) == "...6"] = 5
#names(temp_dia_med)[names(temp_dia_med) == "...7"] = 6
#names(temp_dia_med)[names(temp_dia_med) == "...8"] = 7
#names(temp_dia_med)[names(temp_dia_med) == "...9"] = 8
#names(temp_dia_med)[names(temp_dia_med) == "...10"] = 9
#names(temp_dia_med)[names(temp_dia_med) == "...11"] = 10
#names(temp_dia_med)[names(temp_dia_med) == "...12"] = 11
#names(temp_dia_med)[names(temp_dia_med) == "...13"] = 12


#PLOTANDO DISTRIBUICAO MED TEMPERATURA DIARIA
df_med_61_90 = subset(temp_dia_med, subset = ANO>1960 & ANO<1991 , select = c("ANO", "mes", "dia", "temp"))
hist(temp_dia_med$TEMP, col = "blue", breaks = 20 , ylim=NULL, main = NULL, xlab = "TEMPERATURA DIARIA")


#DF NOME INGLES
colnames(temp_dia_med) = c('YEAR','MONTH','DAY','TEMP')
colnames(df_med_61_90) = c('YEAR','MONTH','DAY','TEMP')


#ESTAT. RECORDES
num_rec_med_high = records(temp_dia_med$TEMP, do.plot = F)
num_rec_med_low = records(-1*temp_dia_med$TEMP, do.plot = F)
s_med = log(tail(num_rec_med_high$number, n = 1)/tail(num_rec_med_low$number, n = 1))


#EXPORTANDO DF BASE
write.csv(temp_dia_med, file = "DF_MEAN_DAY_TEMP.csv", row.names = F)
write.csv(df_med_61_90, file = "DF_MEAN_DAY_TEMP_61_90.csv", row.names = F)


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
temp_dia_max$temp = round(temp_dia_max$temp, 1)


#ALTERACAO DE CLASSE
sapply(long_temp_dia_max, class)
temp_dia_max$mes = as.numeric(temp_dia_max$mes)
temp_dia_max$dia = as.numeric(temp_dia_max$dia)


#DATA FRAME TE MAX ORGANIZATION 
#names(temp_dia_max)[names(temp_dia_max) == "...2"] = 1
#names(temp_dia_max)[names(temp_dia_max) == "...3"] = 2
#names(temp_dia_max)[names(temp_dia_max) == "...4"] = 3
#names(temp_dia_max)[names(temp_dia_max) == "...5"] = 4
#names(temp_dia_max)[names(temp_dia_max) == "...6"] = 5
#names(temp_dia_max)[names(temp_dia_max) == "...7"] = 6
#names(temp_dia_max)[names(temp_dia_max) == "...8"] = 7
#names(temp_dia_max)[names(temp_dia_max) == "...9"] = 8
#names(temp_dia_max)[names(temp_dia_max) == "...10"] = 9
#names(temp_dia_max)[names(temp_dia_max) == "...11"] = 10
#names(temp_dia_max)[names(temp_dia_max) == "...12"] = 11
#names(temp_dia_max)[names(temp_dia_max) == "...13"] = 12


#PLOTANDO DISTRIBUICAO MAX TEMPERATURA DIARIA
df_max_61_90 = subset(temp_dia_max, subset = ANO>1960 & ANO<1991, select = c("ANO", "mes", "dia", "temp"))
hist(temp_dia_max$temp, col = "blue", breaks = 20 , ylim=NULL, main = NULL, xlab = "TEMPERATURA DIARIA")


#DF NOME INGLES
colnames(temp_dia_max) = c('YEAR','MONTH','DAY','TEMP')
colnames(df_max_61_90) = c('YEAR','MONTH','DAY','TEMP')


#ESTAT. RECORDES
num_rec_max_high = records(temp_dia_max$TEMP, do.plot = F)
num_rec_max_low = records(-1*temp_dia_max$TEMP, do.plot = F)
s_max = log(tail(num_rec_max_high$number, n = 1)/tail(num_rec_max_low$number, n = 1))


#EXPORTANDO DF BASE
write.csv(temp_dia_max, file = "DF_MAX_DAY_TEMP.csv", row.names = F)
write.csv(df_max_61_90, file = "DF_MAX_DAY_TEMP_61_90.csv", row.names = F)


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
temp_dia_min$temp = round(temp_dia_min$temp, 1)


#ALTERACAO DE CLASSE
sapply(long_temp_dia_min, class)
temp_dia_min$mes = as.numeric(temp_dia_min$mes)
temp_dia_min$dia = as.numeric(temp_dia_min$dia)


#DATA FRAME TEMP MIN ORGANIZATION 
#names(temp_dia_min)[names(temp_dia_min) == "...2"] = 1
#names(temp_dia_min)[names(temp_dia_min) == "...3"] = 2
#names(temp_dia_min)[names(temp_dia_min) == "...4"] = 3
#names(temp_dia_min)[names(temp_dia_min) == "...5"] = 4
#names(temp_dia_min)[names(temp_dia_min) == "...6"] = 5
#names(temp_dia_min)[names(temp_dia_min) == "...7"] = 6
#names(temp_dia_min)[names(temp_dia_min) == "...8"] = 7
#names(temp_dia_min)[names(temp_dia_min) == "...9"] = 8
#names(temp_dia_min)[names(temp_dia_min) == "...10"] = 9
#names(temp_dia_min)[names(temp_dia_min) == "...11"] = 10
#names(temp_dia_min)[names(temp_dia_min) == "...12"] = 11
#names(temp_dia_min)[names(temp_dia_min) == "...13"] = 12


#PLOTANDO DISTRIBUICAO MAX TEMPERATURA DIARIA
df_min_61_90 = subset(temp_dia_min, subset = ANO>1960 & ANO<1991, select = c("ANO", "mes", "dia", "temp"))
hist(temp_dia_min$temp, col = "blue", breaks = 20 , ylim=NULL, main = NULL, xlab = "TEMPERATURA DIARIA")


#DF NOME INGLES
colnames(temp_dia_min) = c('YEAR','MONTH','DAY','TEMP')
colnames(df_min_61_90) = c('YEAR','MONTH','DAY','TEMP')


#ESTAT. RECORDES
num_rec_min_high = records(temp_dia_min$TEMP, do.plot = F)
num_rec_min_low = records(-1*temp_dia_min$TEMP, do.plot = F)
s_min = log(tail(num_rec_min_high$number, n = 1)/tail(num_rec_min_low$number, n = 1))


#EXPORTANDO DF BASE
write.csv(temp_dia_min, file = "DF_MIN_DAY_TEMP.csv", row.names = F)
write.csv(df_min_61_90, file = "DF_MIN_DAY_TEMP_61_90.csv", row.names = F)
