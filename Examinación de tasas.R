library(dplyr)
library(ggplot2)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(ggthemes)
library(table1)

setwd("~/Blogs Personales/JHU-Files/COVID-19/csse_covid_19_data/csse_covid_19_time_series")

Confirmed<-read.csv('time_series_19-covid-Confirmed.csv')
Deaths<-read.csv('time_series_19-covid-Deaths.csv')

#Seleccionando la ultima fecha

Confirmed<-Confirmed%>%dplyr::select(Province.State, 
                                     Country.Region,
                                     Lat,
                                     Long,
                                     Case.3.14.20=X3.14.20)

Deaths<-Deaths%>%dplyr::select(Province.State, 
                                     Country.Region,
                                    
                                     Death.3.14.20=X3.14.20)

#Haciendo un merge de ambos dataframes

Merged<-Confirmed%>%inner_join(Deaths, by=c("Province.State", "Country.Region"))

#Quiero hacer un analisis separado por region , pais y mundo

#Mundo es lo mas fácil

worldrate<-sum(Merged$Death.3.14.20)/sum(Merged$Case.3.14.20)*100
worldrate

#Tasa por region

RegionRate<-Merged%>%mutate(fatrate=(Death.3.14.20/Case.3.14.20)*100)
#Si no hay casos usamos 0
RegionRate$fatrate[is.na(RegionRate$fatrate)] <- 0

summary(RegionRate$fatrate)

#Density plot

annotation <- data.frame(
  x = mean(RegionRate$fatrate)+4,
  y = 0.20,
  label =paste("Tasa Promedio:",round(mean(RegionRate$fatrate), digits=5), "%") )

p1<-ggplot(RegionRate, aes(x=fatrate)) + 
  geom_density(color="steelblue", fill="darkorange", size=1)+
  geom_vline(aes(xintercept=mean(fatrate)),
                              color="steelblue", linetype="dashed", size=1)+
  coord_cartesian(xlim=c(-1,22))+
  ggtitle("Tasas de mortalidad vs. densidad por región y provincia")+
  xlab("Tasa de mortalidad en %")+
  ylab("Densidad")+
  theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=annotation, aes( x=x, y=y, label=label),                 , 
            color="steelblue", 
            size=4 , fontface="bold" )

p1

#Seleccionemos todo lo que tenga casos positivos

Nonzerocase<-Merged%>%filter(Case.3.14.20>0)

Nonzeroregion<-Nonzerocase%>%mutate(fatrate=(Death.3.14.20/Case.3.14.20)*100)

summary(Nonzeroregion$fatrate)


#Density plot

annotation2 <- data.frame(
  x = mean(RegionRate$fatrate)+4,
  y = 0.8,
  label =paste("Tasa Promedio:",round(mean(Nonzeroregion$fatrate), digits=5), "%") )

p2<-ggplot(Nonzeroregion, aes(x=fatrate)) + 
  geom_density(color="steelblue", fill="darkorange", size=1)+
  geom_vline(aes(xintercept=mean(fatrate)),
             color="steelblue", linetype="dashed", size=1)+
  coord_cartesian(xlim=c(-1,22))+
  ggtitle("Tasas de mortalidad vs. densidad por región y provincia (regiones con casos)")+
  xlab("Tasa de mortalidad en %")+
  ylab("Densidad")+
  theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=annotation2, aes( x=x, y=y, label=label),                 , 
            color="steelblue", 
            size=4 , fontface="bold" )

p2

#Que países tienen tasa de 100%?
RegionRate[which(RegionRate$fatrate==100),]
#Guyana y sudan un solo caso

#Agrupando por pais
CountryGrouped<-Merged%>%group_by(Country.Region)%>%
                summarise(Lat=first(Lat),
                          Long=first(Long),
                          Case.3.14.20=sum(Case.3.14.20),
                          Death.3.14.20=sum(Death.3.14.20))

CountryRate<-CountryGrouped%>%
              mutate(fatrate=(Death.3.14.20/Case.3.14.20)*100)
#Density plot

annotation3 <- data.frame(
  x = mean(CountryRate$fatrate)+4,
  y = 0.80,
  label =paste("Tasa Promedio:",round(mean(CountryRate$fatrate), digits=5), "%") )

p3<-ggplot(CountryRate, aes(x=fatrate)) + 
  geom_density(color="steelblue", fill="darkorange", size=1)+
  geom_vline(aes(xintercept=mean(fatrate)),
             color="steelblue", linetype="dashed", size=1)+
  coord_cartesian(xlim=c(-1,22))+
  ggtitle("Tasas de mortalidad vs. densidad por país")+
  xlab("Tasa de mortalidad en %")+
  ylab("Densidad")+
  theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=annotation3, aes( x=x, y=y, label=label),                 , 
            color="steelblue", 
            size=4 , fontface="bold" )

p3



#Si no hay casos usamos 0
CountryRate$fatrate[is.na(CountryRate$fatrate)] <- 0

#Filtrando casos con 0
NonzeroCount<-CountryRate%>%filter(Case.3.14.20>0)



annotation4 <- data.frame(
  x = mean(NonzeroCount$fatrate)+4,
  y = 0.80,
  label =paste("Tasa Promedio:",round(mean(NonzeroCount$fatrate), digits=5), "%") )

p4<-ggplot(NonzeroCount, aes(x=fatrate)) + 
  geom_density(color="steelblue", fill="darkorange", size=1)+
  geom_vline(aes(xintercept=mean(fatrate)),
             color="steelblue", linetype="dashed", size=1)+
  coord_cartesian(xlim=c(-1,22))+
  ggtitle("Tasas de mortalidad vs. densidad por país (con casos)")+
  xlab("Tasa de mortalidad en %")+
  ylab("Densidad")+
  theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=annotation4, aes( x=x, y=y, label=label),                 , 
            color="steelblue", 
            size=4 , fontface="bold" )

p4


##Paises y regiones donde hubieron muertes

DeathRegion<-RegionRate%>%filter(fatrate>0)
summary(DeathRegion$fatrate)


annotation5 <- data.frame(
  x = mean(DeathRegion$fatrate)+5,
  y = 0.4,
  label =paste("Tasa Promedio:",round(mean(DeathRegion$fatrate), digits=5), "%") )

p5<-ggplot(DeathRegion, aes(x=fatrate)) + 
  geom_density(color="steelblue", fill="darkorange", size=1)+
  geom_vline(aes(xintercept=mean(fatrate)),
             color="steelblue", linetype="dashed", size=1)+
  coord_cartesian(xlim=c(-1,22))+
  ggtitle("Tasas de mortalidad vs. densidad por región (con muertes)")+
  xlab("Tasa de mortalidad en %")+
  ylab("Densidad")+
  theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=annotation5, aes( x=x, y=y, label=label),                 , 
            color="steelblue", 
            size=4 , fontface="bold" )

p5

#Pais con muertes

##Paises y regiones donde hubieron muertes

DeathCountry<-CountryRate%>%filter(fatrate>0)
summary(DeathCountry$fatrate)


annotation6 <- data.frame(
  x = mean(DeathCountry$fatrate)+7.5,
  y = 0.2,
  label =paste("Tasa Promedio:",round(mean(DeathCountry$fatrate), digits=5), "%") )

p6<-ggplot(DeathCountry, aes(x=fatrate)) + 
  geom_density(color="steelblue", fill="darkorange", size=1)+
  geom_vline(aes(xintercept=mean(fatrate)),
             color="steelblue", linetype="dashed", size=1)+
  coord_cartesian(xlim=c(-1,22))+
  ggtitle("Tasas de mortalidad vs. densidad por región (con muertes)")+
  xlab("Tasa de mortalidad en %")+
  ylab("Densidad")+
  theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=annotation6, aes( x=x, y=y, label=label),                 , 
            color="steelblue", 
            size=4 , fontface="bold" )

p6




####
#Region Provincia#


mybins<-c(0,1,2,3,4,100)
mypalette <- colorBin( palette="Reds", domain=RegionRate$fatrate,
                       na.color="transparent",  bins=mybins)

mytext <- paste(
  "País:",RegionRate$Country.Region,"<br/>",
  "Provincia/Estado:", RegionRate$Province.State,"<br/>", 
  "Tasa:",RegionRate$fatrate ,"%", sep=" ")%>%
  lapply(htmltools::HTML)

RegionMap<-leaflet(RegionRate)%>%addTiles() %>%
  addProviderTiles("Esri.WorldImagery")%>% 
addCircleMarkers(~Long, ~Lat, 
               fillColor = ~mypalette(fatrate), fillOpacity = 0.7, 
               color="white", radius=7, stroke=FALSE,
                          label = mytext,
                      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                                   textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~fatrate, opacity=0.9,
             title = "Mortalidad", position = "bottomright" )

saveWidget(RegionMap, file="~/Blogs Personales/COVID19Tasas/RegionMap.html")



###############
#Pais

mybins2<-c(0,1,2,3,4,5,100)
mypalette <- colorBin( palette="Reds", domain=CountryRate$fatrate,
                       na.color="transparent",  bins=mybins2)

mytext2 <- paste(
  "País:",CountryRate$Country.Region,"<br/>",
   
  "Tasa:",CountryRate$fatrate ,"%", sep=" ")%>%
  lapply(htmltools::HTML)

CountryMap<-leaflet(CountryRate)%>%addTiles() %>%
  addProviderTiles("Esri.WorldImagery")%>% 
  addCircleMarkers(~Long, ~Lat, 
                   fillColor = ~mypalette(fatrate), fillOpacity = 0.7, 
                   color="white", radius=7, stroke=FALSE,
                   label = mytext2,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~fatrate, opacity=0.9,
             title = "Mortalidad", position = "bottomright" )

saveWidget(CountryMap, file="~/Blogs Personales/COVID19Tasas/CountryMap.html")

