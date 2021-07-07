# Carga de libreria #

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)

# Carga de informacion

df<-read.csv("Casos_Diarios_Estado_Nacional_Confirmados_20200715.csv")
str(df)

# Cambio de variable tiempo a long format
df_long<-df %>%
  pivot_longer(cols=starts_with("X"),
               names_to="fecha",
               values_to="casos")

head(df_long)

# Cambio de fecha a formato fecha en R

df_long$fecha<- df_long$fecha %>%
  str_remove("X") %>%
  dmy()

df_long<-rename(df_long, estado=nombre)

str(df_long)

# Definicion de fechas de corte para evaluar la efectividad del confinamiento

# 30 de Mayo y 15 de Julio

confinamiento<-"2020-05-30"

df_long$conf<-df_long$fecha<confinamiento

df_long$conf[df_long$conf==TRUE]<-'InicioPandemia'
df_long$conf[df_long$conf==FALSE]<-'Confinamiento'

#Subconjuntio de la base sin considerar acumulado nacional

df_long_estados<-subset(df_long,estado!='Nacional')

#Resultados del de confirmados por etapa de confinamiento

ggplot(subset(df_long,estado=='Nacional'),aes(fill=conf,y=casos,x=conf)) +
  geom_bar(position="dodge",stat="identity")

#Resultados del de confirmados por etapa de confinamiento y estado

ggplot(df_long_estados,aes(fill=conf,y=casos,x=conf)) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Resultados de confinamiento por estado") +
  facet_wrap(~estado)+
  theme(legend.position="none")+
  xlab("")

conf_est<-df_long_estados  %>%
  group_by(estado,conf) %>%
  summarise(casos=sum(casos,na.rm=TRUE))%>%
  spread(conf,casos)

conf_est$delta<-conf_est$Confinamiento-conf_est$InicioPandemia

conf_est[order(conf_est$delta),]

# Delta de los confirmados por covid-19 por estado

barplot(conf_est$delta,
        names=conf_est$estado,
        las=2,
        main='Crecimiento de confirmados al establecer confinamiento',
        xlab="Estados de la Republica",
        ylab="delta",
        col="skyblue")

conf_est_por<-df_long_estados  %>%
  group_by(estado) %>%
  summarise(casos=sum(casos,na.rm=TRUE),poblacion=max(poblacion))

conf_est_por$por<-100*conf_est_por$casos/conf_est_por$poblacion

plot_por<-ggplot(conf_est_por,aes(x=poblacion,y=por))+
  geom_point(size=6)

print(plot_por + labs( title= "Porcentaje de confirmados por poblacion", y="% Confirmados de la poblacion estatal", x = "Poblacion por estado")+geom_text(aes(label=estado),hjust=0,vjust=0))

# Regresion Lineal

acum_dia<-df_long_estados  %>%
  group_by(conf,fecha) %>%
  summarise(casos=sum(casos,na.rm=TRUE))

ggplot(acum_dia, aes(fecha,casos,group=conf,col=conf)) + 
  geom_point() + geom_smooth(method=lm)



# Propuesta: analizar coeficientes de las betas para verificar si el cambio porcentual por dia ha bajado
