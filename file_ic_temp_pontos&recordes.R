#IMPORTANDO DADOS
library(readxl) #LER EXCEL
library(data.table) #CONSTRUIR DATA FRAMES
library(reshape2)
library(ggplot2)
library(evir) #RECORDES

files_temperatura = list.files("C:\\Users\\User\\Documents\\CA - Unifesp\\Iniciação Científica\\Dados IC\\Planilhas_Dados_Temp", pattern=".xls", full.names=T)


#PONTO 1
tn10p = quantile(df_min_61_90$TEMP, 0.10)
df_tn10p_dias = subset(temp_dia_min, subset = TEMP<9.9, select = c("YEAR","TEMP"))
df_tn10p_ano = aggregate(df_tn10p_dias$TEMP, by = list(YEAR=df_tn10p_dias$YEAR), FUN = length)
names(df_tn10p_ano)[names(df_tn10p_ano)=="x"]="LENGTH"


#PLOT SIMPLES
plot(df_tn10p_ano$YEAR, df_tn10p_ano$LENGTH)


#ESTAT. RECORDES
rec_tn10p_ano_high = records(df_tn10p_ano$LENGTH, do.plot = F)
rec_tn10p_ano_low = records(-1*df_tn10p_ano$LENGTH, do.plot = F)
s_tn10p_ano = log(tail(rec_tn10p_ano_high$number, n = 1/tail(rec_tn10p_ano_low$number, n = 1)))

i = 0
df_tn10p_ano$RECORDS = 'NULL'
while (i < 87) {
  i=i+1
  
  if(df_tn10p_ano[i,1] == 1937){
    df_tn10p_ano[i,3] = 'HIGH' 
  }
  if(df_tn10p_ano[i,1] == 1939){
    df_tn10p_ano[i,3] = 'HIGH' 
  }
  if(df_tn10p_ano[i,1] == 1942){
    df_tn10p_ano[i,3] = 'HIGH' 
  }
  if(df_tn10p_ano[i,1] == 1940){
    df_tn10p_ano[i,3] = 'LOW' 
  }
  if(df_tn10p_ano[i,1] == 1950){
    df_tn10p_ano[i,3] = 'LOW' 
  }
  if(df_tn10p_ano[i,1] == 1967){
    df_tn10p_ano[i,3] = 'LOW' 
  }
  if(df_tn10p_ano[i,1] == 1977){
    df_tn10p_ano[i,3] = 'LOW' 
  }
}

round(s_tn10p_ano,5)
observation1.1 = "ρ  =  ln ( Rec. HIGH / Rec. LOW)  =  1.38629"
observation2.1 = "The initial data represents a high and low record, simultaneously."
observation3.1 = "TN10p is a climatological index that considers the number of days per year with minimum \n temperature under quantile 0.10 of the daily minimum temperature distribution between 1961 and 1990."
"Daily avarege temperature distribution between 1961 and 1990."

#PLOT GGPLOT2 (ELABORADO)
ggplot(df_tn10p_ano, aes(x = YEAR, y = LENGTH, colour = RECORDS, shape = RECORDS, fill = RECORDS))+
  geom_point()+
  scale_color_manual(values = c('blue','red', 'black'))+
  scale_shape_manual(values = c(24,25,21))+
  scale_fill_manual(values = c('blue','red','black'))+
  scale_x_continuous(breaks = seq(0,2025,20)) + 
  scale_y_continuous(breaks = seq(0,110,20))+
  theme_bw()+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        element_line(linewidth = 0.5),
        axis.title = element_text(size = 11, hjust = 0.5),
        axis.ticks.length.y = unit(-.20, "cm"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 10),
        axis.ticks.length.x = unit(-.20, "cm"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold", margin = margin(t = 100, b = 1, l = 6)),
        plot.caption = element_text(size = 10))+  
  labs(x = "YEAR", y = "TN10p")+
  labs(caption = paste0("\n",observation1.1,"\n","\n",observation2.1,"\n","\n",observation3.1))


#EXPORTANDO DF BASE
write.csv(df_tn10p_dias, file = "DF_TN10P_DIAS.csv", row.names = F)


#PONTO 2
tn90p = quantile(df_min_61_90$TEMP, 0.90)
df_tn90p_dias = subset(temp_dia_min, subset = TEMP>18.8, select = c("YEAR","TEMP"))
df_tn90p_ano = aggregate(df_tn90p_dias$TEMP, by = list(YEAR=df_tn90p_dias$YEAR), FUN = length)
names(df_tn90p_ano)[names(df_tn90p_ano)=="x"]="LENGTH"


#PLOT SIMPLES
plot(df_tn90p_ano$ANO, df_tn90p_ano$length)


#ESTAT. RECORDES
rec_tn90p_ano_high = records(df_tn90p_ano$LENGTH, do.plot = F)
rec_tn90p_ano_low = records(-1*df_tn90p_ano$LENGTH, do.plot = F)
s_tn90p_ano = log(tail(rec_tn90p_ano_high$number, n = 1/tail(rec_tn90p_ano_low$number, n = 1)))

i = 0
df_tn90p_ano$RECORDS = 'NULL'
while (i < 87) {
  i=i+1
  
  if(df_tn90p_ano[i,1] == 1937){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1938){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1940){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1958){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1966){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1973){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1980){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1983){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1984){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1986){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 1996){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
  if(df_tn90p_ano[i,1] == 2009){
    df_tn90p_ano[i,3] = 'HIGH' 
  }
}

round(s_tn90p_ano,5)
observation1.2 = "ρ  =  ln ( Rec. HIGH / Rec. LOW)  =  2.56495"
observation2.2 = "The initial data represents a high and low record, simultaneously."
observation3.2 = "TN90p is a climatological index that considers the number of days per year with minimum \n temperature above quantile 0.90 of the daily minimum temperature distribution between 1961 and 1990."

#PLOT GGPLOT2 (ELABORADO)
ggplot(df_tn90p_ano, aes(x = YEAR, y = LENGTH, colour = RECORDS, shape = RECORDS, fill = RECORDS))+
  geom_point()+
  scale_color_manual(values = c('blue','black'))+
  scale_shape_manual(values = c (24,21))+
  scale_fill_manual(values = c('blue','black'))+
  scale_x_continuous(breaks = seq(0,2025,20)) + 
  scale_y_continuous(breaks = seq(15,100,20))+
  theme_bw()+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        element_line(linewidth = 0.5),
        axis.title = element_text(size = 11, hjust = 0.5),
        axis.ticks.length.y = unit(-.20, "cm"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 10),
        axis.ticks.length.x = unit(-.20, "cm"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold", margin = margin(t = 100, b = 1, l = 6)),
        plot.caption = element_text(size = 10))+  
  labs(x = "YEAR", y = "TN90p")+
  labs(caption = paste0("\n",observation1.2,"\n","\n",observation2.2,"\n","\n",observation3.2))

  
#EXPORTANDO DF BASE
write.csv(df_tn90p_dias, file = "DF_TN90P_DIAS.csv", row.names = F)


observation1.3 = "Green dashed line = quantile 0.1"
observation2.3 = "Red dashed line = quantile 0.90"
observation3.3 = "Distribution of minimum daily temperatures between 1961 and 1990."

#PLOTANDO DISTRIBUICAO MIN TEMPERATURA DIARIA PERCENTIL 0.10 E PERCENTIL 0.90 COM GGPLOT2 (ELABORADO)
ggplot(df_min_61_90, aes(x=df_min_61_90$TEMP, y=stat(count)))+
  geom_histogram(
    bins = 25, 
    col=("#3366FF"),
    fill=("lightblue"),
    alpha=I(.5))+
  geom_density(aes(x=TEMP), col = "#000066")+
  geom_vline(
    xintercept = quantile(df_min_61_90$TEMP,probs=c(0.1)),
    color="#339900",
    linetype="dashed",
    size=0.5)+
  geom_vline(
    xintercept = quantile(df_min_61_90$TEMP,probs=c(0.9)),
    color="#FF0000",
    linetype="dashed",
    size=0.5)+
  scale_x_continuous(breaks = seq(1,50,3))+
  scale_y_continuous(breaks = seq(100,1200,200))+
  theme_bw()+
  theme(
    plot.margin = margin(1.5, 1.5, 1.25, 1.25, "cm"),
    element_line(linewidth = 0.5),
    axis.title = element_text(size = 10, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.y = element_text(size = 9),
    axis.ticks.length.y = unit(.20, "cm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(size = 9),
    axis.ticks.length.x = unit(.20, "cm"))+
  labs(x = "TEMPERATURE", y = " DAYS (FREQUENCY)")+
  labs(caption = paste0("\n",observation1.3,"\n","\n",observation2.3,"\n","\n",observation3.3))


#PONTO 3
tx10p = quantile(df_max_61_90$TEMP, 0.10)
df_tx10p_dias = subset(temp_dia_max, subset = TEMP<18.4, select = c("YEAR","TEMP"))
df_tx10p_ano = aggregate(df_tx10p_dias$TEMP, by = list(YEAR=df_tx10p_dias$YEAR), FUN = length)
names(df_tx10p_ano)[names(df_tx10p_ano)=="x"]="LENGTH"


#PLOT SIMPLES
plot(df_tx10p_ano$ANO, df_tx10p_ano$length)


#ESTAT. RECORDES
rec_tx10p_ano_high = records(df_tx10p_ano$LENGTH, do.plot = F)
rec_tx10p_ano_low = records(-1*df_tx10p_ano$LENGTH, do.plot = F)
s_tx10p_ano = log(tail(rec_tx10p_ano_high$number, n = 1/tail(rec_tx10p_ano_low$number, n = 1)))

i = 0
df_tx10p_ano$RECORDS = 'NULL'
while (i < 87) {
  i=i+1
  
  if(df_tx10p_ano[i,1] == 1937){
    df_tx10p_ano[i,3] = 'HIGH' 
  }
  if(df_tx10p_ano[i,1] == 1938){
    df_tx10p_ano[i,3] = 'HIGH' 
  }
  if(df_tx10p_ano[i,1] == 1941){
    df_tx10p_ano[i,3] = 'HIGH' 
  }
  if(df_tx10p_ano[i,1] == 1956){
    df_tx10p_ano[i,3] = 'HIGH' 
  }
  if(df_tx10p_ano[i,1] == 1939){
    df_tx10p_ano[i,3] = 'LOW' 
  }
  if(df_tx10p_ano[i,1] == 1946){
    df_tx10p_ano[i,3] = 'LOW' 
  }
  if(df_tx10p_ano[i,1] == 1961){
    df_tx10p_ano[i,3] = 'LOW' 
  }
  if(df_tx10p_ano[i,1] == 1977){
    df_tx10p_ano[i,3] = 'LOW' 
  }
}

round(s_tx10p_ano,5)
observation1.4 = "ρ  =  ln ( Rec. HIGH / Rec. LOW)  =  1.60944"
observation2.4 = "The initial data represents a high and low record, simultaneously."
observation3.4 = "TX10p is a climatological index that considers the number of days per year with maximum \n temperature under quantile 0.10 of the daily maximum temperature distribution between 1961 and 1990."

#PLOT GGPLOT2 (ELABORADO)
ggplot(df_tx10p_ano, aes(x = YEAR, y = LENGTH, colour = RECORDS, shape = RECORDS, fill = RECORDS))+
  geom_point(aes(color = RECORDS))+
  scale_color_manual(values=c('blue','red', 'black'))+
  scale_shape_manual(values = c(24,25,21))+
  scale_fill_manual(values = c('blue','red','black'))+
  scale_x_continuous(breaks = seq(0,2025,20)) + 
  scale_y_continuous(breaks = seq(20,80,15))+
  theme_bw()+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        element_line(linewidth = 0.5),
        axis.title = element_text(size = 11, hjust = 0.5),
        axis.ticks.length.y = unit(-.20, "cm"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 10),
        axis.ticks.length.x = unit(-.20, "cm"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold", margin = margin(t = 100, b = 1, l = 6)),
        plot.caption = element_text(size = 10))+  
  labs(x = "YEAR", y = "TX10p")+
  labs(caption = paste0("\n",observation1.4,"\n","\n",observation2.4,"\n","\n",observation3.4))


#EXPORTANDO DF BASE
write.csv(df_tx10p_dias, file = "DF_TX10P_DIAS.csv", row.names = F)


#PONTO 4
tx90p = quantile(df_max_61_90$TEMP, 0.90)
df_tx90p_dias = subset(temp_dia_max, subset = TEMP>30.4, select = c("YEAR","TEMP"))
df_tx90p_ano = aggregate(df_tx90p_dias$TEMP, by = list(YEAR=df_tx90p_dias$YEAR), FUN = length)
names(df_tx90p_ano)[names(df_tx90p_ano)=="x"]="LENGTH"


#PLOT SIMPLES
plot(df_tx90p_ano$ANO, df_tx90p_ano$length)


#ESTAT. RECORDES
rec_tx90p_ano_high = records(df_tx90p_ano$LENGTH, do.plot = F)
rec_tx90p_ano_low = records(-1*df_tx90p_ano$LENGTH, do.plot = F)
s_tx90p_ano = log(tail(rec_tx90p_ano_high$number, n = 1/tail(rec_tx90p_ano_low$number, n = 1)))

i = 0
df_tx90p_ano$RECORDS = 'NULL'
while (i < 87) {
  i=i+1
  
  if(df_tx90p_ano[i,1] == 1939){
    df_tx90p_ano[i,3] = 'HIGH' 
  }
  if(df_tx90p_ano[i,1] == 1961){
    df_tx90p_ano[i,3] = 'HIGH' 
  }
  if(df_tx90p_ano[i,1] == 1963){
    df_tx90p_ano[i,3] = 'HIGH' 
  }
  if(df_tx90p_ano[i,1] == 1990){
    df_tx90p_ano[i,3] = 'HIGH' 
  }
  if(df_tx90p_ano[i,1] == 2002){
    df_tx90p_ano[i,3] = 'HIGH' 
  }
  if(df_tx90p_ano[i,1] == 2014){
    df_tx90p_ano[i,3] = 'HIGH' 
  }
  if(df_tx90p_ano[i,1] == 1937){
    df_tx90p_ano[i,3] = 'LOW' 
  }
  if(df_tx90p_ano[i,1] == 1943){
    df_tx90p_ano[i,3] = 'LOW' 
  }
}

round(s_tx90p_ano,5)
observation1.5 = "ρ  =  ln ( Rec. HIGH / Rec. LOW)  =  1.94591"
observation2.5 = "The initial data represents a high and low record, simultaneously."
observation3.5 = "TX90p is a climatological index that considers the number of days per year with maximum \n temperature above quantile 0.90 of the daily maximum temperature distribution between 1961 and 1990."

#PLOT GGPLOT2 (ELABORADO)
ggplot(df_tx90p_ano, aes(x = YEAR, y = LENGTH, colour = RECORDS, shape = RECORDS, fill = RECORDS))+
  geom_point()+
  scale_color_manual(values=c('blue','red','black'))+
  scale_shape_manual(values = c(24,25,21))+
  scale_fill_manual(values = c('blue','red','black'))+
  scale_x_continuous(breaks = seq(0,2025,20)) + 
  scale_y_continuous(breaks = seq(0,110,20))+
  theme_bw()+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        element_line(linewidth = 0.5),
        axis.title = element_text(size = 11, hjust = 0.5),
        axis.ticks.length.y = unit(-.20, "cm"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 10),
        axis.ticks.length.x = unit(-.20, "cm"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold", margin = margin(t = 100, b = 1, l = 6)),
        plot.caption = element_text(size = 10))+  
  labs(x = "YEAR", y = "TX90p")+
  labs(caption = paste0("\n",observation1.5,"\n","\n",observation2.5,"\n","\n",observation3.5))


#EXPORTANDO DF BASE
write.csv(df_tx90p_dias, file = "DF_TX90P_DIAS.csv", row.names = F)


observation1.6 = "Green dashed line = quantile 0.1"
observation2.6 = "Red dashed line = quantile 0.9"
observation3.6 = "Distribution of the daily maximim temperature between 1961-1990 "

#PLOTANDO DISTRIBUICAO MIN TEMPERATURA DIARIA PERCENTIL 0.10 E PERCENTIL 0.90 COM GGPLOT2 (ELABORADO)
ggplot(df_max_61_90, aes(x=df_max_61_90$TEMP, y=stat(count)))+
  geom_histogram(
    bins = 26, 
    col=("#3366FF"),
    fill=("lightblue"),
    alpha=I(.5))+
  geom_density(aes(x=TEMP), col = "#000066")+
  geom_vline(
    xintercept = quantile(df_max_61_90$TEMP,probs=c(0.1)),
    color="#339900",
    linetype="dashed",
    size=0.5)+
  geom_vline(
    xintercept = quantile(df_max_61_90$TEMP,probs=c(0.9)),
    color="#FF0000",
    linetype="dashed",
    size=0.5)+
  scale_x_continuous(breaks = seq(10,50,4))+
  scale_y_continuous(breaks = seq(100,1200,200))+
  theme_bw()+
  theme(
    plot.margin = margin(1.5, 1.5, 1.25, 1.25, "cm"),
    element_line(linewidth = 0.5),
    axis.title = element_text(size = 10, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.y = element_text(size = 9),
    axis.ticks.length.y = unit(.20, "cm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(size = 9),
    axis.ticks.length.x = unit(.20, "cm"))+
  labs(x = "TEMPERATURE", y = " DAYS (FREQUENCY)")+
  labs(caption = paste0("\n",observation1.6,"\n","\n",observation2.6,"\n","\n",observation3.6))



#RECORDES PARA CIMA
num_rec_med_high = records(temp_dia_med$TEMP, do.plot = F)
num_rec_max_high = records(temp_dia_max$TEMP, do.plot = F)
num_rec_min_high = records(temp_dia_min$TEMP, do.plot = F)

#RECORDES PARA BAIXO
num_rec_med_low = records(-1*temp_dia_med$TEMP, do.plot = F)
num_rec_max_low = records(-1*temp_dia_max$TEMP, do.plot = F)
num_rec_min_low = records(-1*temp_dia_min$TEMP, do.plot = F)

#SIMETRIA RECORDES
s_med = log(tail(num_rec_med_high$number, n = 1)/tail(num_rec_med_low$number, n = 1))
s_max = log(tail(num_rec_max_high$number, n = 1)/tail(num_rec_max_low$number, n = 1))
s_min = log(tail(num_rec_min_high$number, n = 1)/tail(num_rec_min_low$number, n = 1))
