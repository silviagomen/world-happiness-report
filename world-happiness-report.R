rm(list=ls())

# Cargar los datos
data15 <- read.csv("2015.csv", header = TRUE, stringsAsFactors = TRUE)
data16 <- read.csv("2016.csv", header = TRUE, stringsAsFactors = TRUE)
data17 <- read.csv("2017.csv", header = TRUE, stringsAsFactors = TRUE)
data18 <- read.csv("2018.csv", header = TRUE, stringsAsFactors = FALSE)
data19 <- read.csv("2019.csv", header = TRUE, stringsAsFactors = TRUE)

# observar datos
summary(data15)
summary(data16)
summary(data17)
summary(data18)
summary(data19)

# cambiar nombres a columnas para que coincidan
colnames(data15)[c(3,4,6,8,10,12)] = c("Rank","Score","Economy","Health","Corruption","Dystopia")
colnames(data16)[c(3,4,7,9,11,13)] = c("Rank","Score","Economy","Health","Corruption","Dystopia")
colnames(data17)[c(2,3,6,8,11,12)] = c("Rank","Score","Economy","Health","Corruption","Dystopia")
colnames(data18)[c(1,2,4,5,6,7,9)] = c("Rank","Country","Economy","Family","Health","Freedom","Corruption")
colnames(data19)[c(1,2,4,5,6,7,9)] = c("Rank","Country","Economy","Family","Health","Freedom","Corruption")

# cambiar ese dato que está mal escrito el NA
data18$Corruption = as.numeric(gsub("N/A", NA, data18$Corruption))

# quitar las variables que sobran
library(dplyr)
data15$Standard.Error = NULL
data16 <- select(data16, -Lower.Confidence.Interval, -Upper.Confidence.Interval)
data17 <- select(data17, -Whisker.high, -Whisker.low)

# hacemos que haya los mismos paises en todo
data15 = data15[data15$Country %in% data16$Country,]
data15 = data15[data15$Country %in% data17$Country,]
data15 = data15[data15$Country %in% data18$Country,]
data15 = data15[data15$Country %in% data19$Country,]

data16 = data16[data16$Country %in% data15$Country,]
data17 = data17[data17$Country %in% data15$Country,]
data18 = data18[data18$Country %in% data15$Country,]
data19 = data19[data19$Country %in% data15$Country,]

# añadimos la variable región a las tablas donde no está
data15 = data15[order(data15$Country,decreasing=FALSE),]
data17 = data17[order(data17$Country,decreasing=FALSE),]
data18 = data18[order(data18$Country,decreasing=FALSE),]
data19 = data19[order(data19$Country,decreasing=FALSE),]
data17$Region = data15$Region
data18$Region = data15$Region
data19$Region = data15$Region
data15 = data15[order(data15$Rank,decreasing=FALSE),]
data17 = data17[order(data17$Rank,decreasing=FALSE),]
data18 = data18[order(data18$Rank,decreasing=FALSE),]
data19 = data19[order(data19$Rank,decreasing=FALSE),]

# reordenar columnas
data15 <- data15[,c(3,1:2,4:11)]
data16 <- data16[,c(3,1:2,4:11)]
data17 <- data17[,c(2,1,11,3:10)]
data18 <- data18[,c(1:2,10,3:9)]
data19 <- data19[,c(1:2,10,3:9)]

# calcular dystopia para data18 y 19
data18$Dystopia = data18$Score - rowSums(data18[,5:10])
data19$Dystopia = data19$Score - rowSums(data19[,5:10])

# poner una columna de año para juntarlas luego y poder distinguirlas
data15$Year = "2015"
data16$Year = "2016"
data17$Year = "2017"
data18$Year = "2018"
data19$Year = "2019"

# guardar como csv para poder usarlas luego
write.csv(data15, "data15.csv", row.names = FALSE)
write.csv(data16, "data16.csv", row.names = FALSE)
write.csv(data17, "data17.csv", row.names = FALSE)
write.csv(data18, "data18.csv", row.names = FALSE)
write.csv(data19, "data19.csv", row.names = FALSE)

# juntar todos en uno
data = rbind(data15,data16,data17,data18,data19)

# y guardarlo
write.csv(data,"data.csv",row.names = FALSE)

rm(list=ls())
# subir los datos corregidos y la tabla nueva
data15 <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)
data16 <- read.csv("data16.csv", header = TRUE, stringsAsFactors = TRUE)
data17 <- read.csv("data17.csv", header = TRUE, stringsAsFactors = TRUE)
data18 <- read.csv("data18.csv", header = TRUE, stringsAsFactors = FALSE)
data19 <- read.csv("data19.csv", header = TRUE, stringsAsFactors = TRUE)
data <- read.csv("data.csv", header = TRUE, stringsAsFactors = TRUE)

data15$Year <- as.factor(data15$Year)
data16$Year <- as.factor(data16$Year)
data17$Year <- as.factor(data17$Year)
data18$Year <- as.factor(data18$Year)
data19$Year <- as.factor(data19$Year)
data$Year <- as.factor(data$Year)

data15$Rank <- as.factor(data15$Rank)
data16$Rank <- as.factor(data16$Rank)
data17$Rank <- as.factor(data17$Rank)
data18$Rank <- as.factor(data18$Rank)
data19$Rank <- as.factor(data19$Rank)
data$Rank <- as.factor(data$Rank)


# STACKED BARPLOT RANKING
rm(list=ls())

# cargar librerías de este apartado
library(reshape2)
library(ggplot2)

# subir datos
data <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)
data <- data[c(2,5:11)]

#verticalizar
data = melt(data,id.vars = "Country")

# crear gráfico
ggplot(data, aes(fill=variable, y=value, x=Country)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("País") + ylab("")

# mejorar gráfico
# crear variable rank para ordenarla
ranking = as.data.frame(seq(1:141))

# como son 7 variables, copio y pego el ranking 7 veces, y lo añado a data
ranking2 = NULL
for (i in 1:7) {ranking2 = rbind(ranking2,ranking)}
data$rank = ranking2

# primeros 10 países 
top10 = data[order(data$rank,decreasing=FALSE)[1:70],]
ggplot(top10, aes(fill=variable, y=value, x=Country)) + 
  geom_bar(position="stack", stat="identity", width = 0.7) +
  xlab("") + ylab("") + 
  coord_flip() +
  scale_fill_manual(values = c("#355070","#515575","#6D597A","#B56576","#E56B6F","#E88C7D","#EAAC8B")) +
  theme_bw() +
  ggtitle("Países con mayor puntuación total 2015")

# ultimos 10 países 
fin10 = data[order(data$rank,decreasing=TRUE)[1:70],]
ggplot(fin10, aes(fill=variable, y=value, x=Country)) + 
  geom_bar(position="stack", stat="identity", width = 0.7) +
  xlab("") + ylab("") +
  coord_flip() +
  scale_fill_manual(values = c("#355070","#515575","#6D597A","#B56576","#E56B6F","#E88C7D","#EAAC8B")) +
  theme_bw() +
  ggtitle("Países con menor puntuación total 2015")

# primeros 5 y ultimos 5
top5 = data[order(data$rank,decreasing=FALSE)[1:35],]
fin5 = data[order(data$rank,decreasing=TRUE)[1:35],]
datos = rbind(top5,fin5)
ggplot(datos, aes(fill=variable, y=value, x=Country)) + 
  geom_bar(position="stack", stat="identity", width = 0.7) +
  xlab("") + ylab("") +
  coord_flip() +
  scale_fill_manual(values = c("#355070","#515575","#6D597A","#B56576","#E56B6F","#E88C7D","#EAAC8B")) +
  theme_bw() +
  ggtitle("Países con mayor y menor puntuación total")



# HISTOGRAMA PUNTUACIONES POR AÑO
rm(list=ls())

# subir datos
data15 <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)
data16 <- read.csv("data16.csv", header = TRUE, stringsAsFactors = TRUE)
data17 <- read.csv("data17.csv", header = TRUE, stringsAsFactors = TRUE)
data18 <- read.csv("data18.csv", header = TRUE, stringsAsFactors = FALSE)
data19 <- read.csv("data19.csv", header = TRUE, stringsAsFactors = TRUE)
data <- read.csv("data.csv", header = TRUE, stringsAsFactors = TRUE)

# crear histogramas
par(mfrow=c(2,3))
hist(data15$Score,main = "Distribución puntuación 2015", xlab = "Score", ylab = "Frecuencia", col = "gold")
hist(data16$Score,main = "Distribución puntuación 2016", xlab = "Score", ylab = "Frecuencia", col = "gold")
hist(data17$Score,main = "Distribución puntuación 2017", xlab = "Score", ylab = "Frecuencia", col = "gold")
hist(data18$Score,main = "Distribución puntuación 2018", xlab = "Score", ylab = "Frecuencia", col = "gold")
hist(data19$Score,main = "Distribución puntuación 2019", xlab = "Score", ylab = "Frecuencia", col = "gold")
hist(data$Score,main = "Distribución puntuación total", xlab = "Score", ylab = "Frecuencia", col = "gold")


# MEDIA SCORE POR AÑO
dataMedias <- data.frame(año = as.factor(2015:2019),
                         media = c(mean(data15$Score),mean(data16$Score),
                                             mean(data17$Score),mean(data18$Score),mean(data19$Score)))
                           
mediaTotal <- data.frame(año = "total",
                         media = mean(dataMedias$media))

dataMedias <- rbind(dataMedias,mediaTotal)

# graficarlo, en el segundo gráfico se aprecia más la diferencia, aunque sea mínima
par(mfrow=c(1,2))
barplot(height = dataMedias$media, names = dataMedias$año, main = "Puntuación media por año", xlab = "año", ylab = "media", col = "light blue",las=2)
plot(dataMedias,las=2)


# GRÁFICO NÚMERO DE PAÍSES POR REGIÓN
rm(list=ls())

# subir datos
data15 <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)

# hacer gráfico y usar summary para ver bien los nombres
par(mfrow=c(1,1))
plot(data15$Region,main = "Regiones",ylab = "Frecuencia",col = "pink",las=2)
summary(data15$Region)


# 5 MEJORES Y PEORES PUNTUACIONES POR PAISES 
top15 = data15[order(data15$Rank,decreasing=FALSE)[1:5],]
bottom15 = data15[order(data15$Rank,decreasing=TRUE)[1:5],]

# hacer gráfico, color azul a los que mejoran y rojo a los que empeoran
par(mfrow=c(1,2))
barplot(height = top15$Score, names = top15$Country, main = "5 mejores puntuaciones 2015", ylab = "Score",
        col = "#3891a6", las = 2, ylim = c(0,10))
barplot(height = bottom15$Score, names = bottom15$Country, main = "5 peores puntuaciones 2015", ylab = "Score", 
        col = "#b80c09", las = 2, ylim = c(0,10))



# BOXPLOT HAPPINESS POR REGIÓN
rm(list=ls())

# subir datos
data15 <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)
data16 <- read.csv("data16.csv", header = TRUE, stringsAsFactors = TRUE)
data17 <- read.csv("data17.csv", header = TRUE, stringsAsFactors = TRUE)
data18 <- read.csv("data18.csv", header = TRUE, stringsAsFactors = TRUE)
data19 <- read.csv("data19.csv", header = TRUE, stringsAsFactors = TRUE)

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)

levels(data15$Region) <- c("A","B","C","D","E","F","G","H","I","J")
data15 %>%
  ggplot( aes(x=Region, y=Score, fill=Region)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  ggtitle("Felicidad por regiones") +
  xlab("Regiones")

# BOXPLOT HAPPINESS POR REGIÓN Y  AÑOS
data = data15[3]
data$s2015 = data15$Score
data$s2016 = data16$Score
data$s2017 = data17$Score
data$s2018 = data18$Score
data$s2019 = data19$Score

#verticalizar
library(reshape2)
data = melt(data,id.vars = "Region")

# como este se ve mal, probamos con otro
ggplot(data, aes(x=Region, y=value, fill=variable)) + 
  geom_boxplot()

p2 <- ggplot(data, aes(x=Region, y=value, fill=variable)) + 
    geom_boxplot() +
    facet_wrap(~Region, scale="free")
p2

p1 <- ggplot(data, aes(x=Region, y=value, fill=variable)) + 
  geom_boxplot() +
  facet_wrap(~variable)
p1


# RELACIONES Y CORRELACIONES ENTRE VARIABLES NUMERICAS

# subir librerias
library(psych)
library(corrplot)

# poner solo variables numéricas
dataNum = data15[,sapply(data15,is.numeric)]
dataNum[c(1,10)] = NULL

# relación
pairs.panels(dataNum,hist.col = "light blue",density = FALSE)

# correlación
corrplot(cor(dataNum), method="shade", shade.col=NA, tl.col="black", 
         tl.srt=45, addgrid.col="black", type="lower", diag=FALSE, cl.pos="n")




# MAPAS
# Fuentes:
  # https://www.youtube.com/watch?v=sLiNAsmpXP0
  # https://thematicmapping.org/downloads/world_borders.php
rm(list=ls())

# cargar librerias
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(gpclib)

# cargar archivos
mapa_mundial <- readOGR(dsn=".",layer="TM_WORLD_BORDERS-0.3")

# guardar csv con paises
write.csv(mapa_mundial@data$NAME,file="paises.csv",row.names = FALSE)

# hacer grafico del mapa
plot(mapa_mundial, axes=TRUE)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
# https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true

# transformar info polígonos a dataframe
mapa_mundial_fortified <- fortify(mapa_mundial, region = "NAME")

# hacer que los nombres de los países coincidan con el archivo
paises <- read.csv("paises.csv", header = TRUE, stringsAsFactors = TRUE)
data <- read.csv("data.csv", header = TRUE, stringsAsFactors = TRUE)

data$Country = gsub("South Korea", "Korea, Republic of", data$Country)
data$Country = gsub("Moldova", "Republic of Moldova", data$Country)
data$Country = gsub("Libya", "Libyan Arab Jamahiriya", data$Country)
data$Country = gsub("Vietnam", "Viet Nam", data$Country)
data$Country = gsub("Palestinian Territories", "Palestine", data$Country)
data$Country = gsub("Iran", "Iran (Islamic Republic of)", data$Country)
data$Country = gsub("Congo \\(Kinshasa\\)", "Democratic Republic of the Congo", data$Country)
data$Country = gsub("Myanmar", "Burma", data$Country)
data$Country = gsub("Congo \\(Brazzaville\\)", "Congo", data$Country)
data$Country = gsub("Tanzania", "United Republic of Tanzania", data$Country)
data$Country = gsub("Ivory Coast", "Cote d'Ivoire", data$Country)
data$Country = gsub("Syria", "Syrian Arab Republic", data$Country)

data = data[data$Country %in% paises$x,] # hay menos observaciones porque eliminamos Kosovo

# categorizar datos de score
data$Felicidad[data$Score >= 7.5] = "7.5 +"
data$Felicidad[data$Score >= 7 & data$Score < 7.5] = "7-7.5"
data$Felicidad[data$Score >= 6 & data$Score < 7] = "6-7"
data$Felicidad[data$Score >= 5 & data$Score < 6] = "5-6"
data$Felicidad[data$Score >= 4 & data$Score < 5] = "4-5"
data$Felicidad[data$Score >= 3 & data$Score < 4] = "3-4"
data$Felicidad[data$Score < 3] = "< 3"

# cambiar los nombres de las variables para que coincidan con las del mapa
colnames(data)[2] = "id"

# hacer que las columnas id de los archivos esten en minusculas
mapa_mundial_fortified$id = tolower(mapa_mundial_fortified$id) 
data$id = tolower(data$id)

# usamos los datos de cada año
data15 = data[data$Year == "2015",]
  # guardamos un csv de 2015 para más adelante
  write.csv(data15, "mapdata15.csv", row.names = FALSE)
data15 = data15[c(2,4,13)]
data16 = data[data$Year == "2016",]
data16 = data16[c(2,4,13)]
data17 = data[data$Year == "2017",]
data17 = data17[c(2,4,13)]
data18 = data[data$Year == "2018",]
data18 = data18[c(2,4,13)]
data19 = data[data$Year == "2019",]
  # guardamos un csv de 2019 para más adelante
  write.csv(data19, "mapdata19.csv", row.names = FALSE)
data19 = data19[c(2,4,13)]

# juntar las dos bases de datos
mapa1 = merge(mapa_mundial_fortified, data15, by = "id", all = TRUE)
mapa2 = merge(mapa_mundial_fortified, data16, by = "id", all = TRUE)
mapa3 = merge(mapa_mundial_fortified, data17, by = "id", all = TRUE)
mapa4 = merge(mapa_mundial_fortified, data18, by = "id", all = TRUE)
mapa5 = merge(mapa_mundial_fortified, data19, by = "id", all = TRUE)

# ordenar los puntos para graficar
mapa1 = mapa1[order(mapa1$order),]
mapa2 = mapa2[order(mapa2$order),]
mapa3 = mapa3[order(mapa3$order),]
mapa4 = mapa4[order(mapa4$order),]
mapa5 = mapa5[order(mapa5$order),]

# hacer mapas de todos los años
mapa15 <- ggplot(mapa1, aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=Felicidad)) +
            theme_void() +
            scale_fill_brewer(palette="Blues",na.value = "Grey") +
            labs(x="",y="",fill="Score",title = "Felicidad 2015") +
            theme(plot.title = element_text(hjust = 0.5))
mapa15


mapa16 <- ggplot(mapa2, aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=Felicidad)) +
            theme_void() +
            scale_fill_brewer(palette="Blues",na.value = "Grey") +
            labs(x="",y="",fill="Score",title = "Felicidad 2016") +
            theme(plot.title = element_text(hjust = 0.5))
mapa16

mapa17 <- ggplot(mapa3, aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=Felicidad)) +
            theme_void() +
            scale_fill_brewer(palette="Blues",na.value = "Grey") +
            labs(x="",y="",fill="Score",title = "Felicidad 2017") +
            theme(plot.title = element_text(hjust = 0.5))
mapa17

mapa18 <- ggplot(mapa4, aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=Felicidad)) +
            theme_void() +
            scale_fill_brewer(palette="Blues",na.value = "Grey") +
            labs(x="",y="",fill="Score",title = "Felicidad 2018") +
            theme(plot.title = element_text(hjust = 0.5))
mapa18

mapa19 <- ggplot(mapa5, aes(x=long, y=lat, group=group)) +
            geom_polygon(aes(fill=Felicidad)) +
            theme_void() +
            scale_fill_brewer(palette="Blues",na.value = "Grey") +
            labs(x="",y="",fill="Score",title = "Felicidad 2019") +
            theme(plot.title = element_text(hjust = 0.5))
mapa19


# MAPA EVOLUCIÓN

# hacer un data frame nuevo con paises, puntuaciones y una columna nueva de resta
dataComp <- data15[1]
dataComp$score15 <- data15$Score
dataComp$score19 <- data19$Score
dataComp$resta = dataComp$score19 - dataComp$score15

# categorizar datos y eliminar variables
dataComp$result[dataComp$resta >= 0.05] = "Mejora"
dataComp$result[dataComp$resta <= -0.05] = "Empeora"
dataComp$result[dataComp$resta > -0.05 & dataComp$resta < 0.05] = "Igual"
dataComp[2:3] = NULL

# juntar las dos bases de datos
mapa6 = merge(mapa_mundial_fortified, dataComp, by = "id", all = TRUE)

# ordenar los puntos para graficar
mapa6 = mapa6[order(mapa6$order),]

# hacer mapas de todos los años
mapaComp <- ggplot(mapa6, aes(x=long, y=lat, group=group)) +
              geom_polygon(aes(fill=result), colour = "black", size = 0.05) +
              theme_void() +
              scale_fill_manual(values = c("red","grey","steelblue","black")) +
              labs(x="",y="",fill="Cambio",title = "Felicidad 2015-2019") +
              theme(plot.title = element_text(hjust = 0.5))
mapaComp

# guardamos csv dataComp
write.csv(dataComp, "dataComp.csv", row.names = FALSE)


# GRÁFICOS EVOLUCIÓN
rm(list=ls())

# subir datos
data <- read.csv("dataComp.csv", header = TRUE, stringsAsFactors = TRUE)
data15 <- read.csv("mapdata15.csv", header = TRUE, stringsAsFactors = TRUE)
data$region <- data15$Region

# número de paises que mejoran/empeoran por región
ggplot(data,aes(x=result, fill = region)) +
  geom_bar(width = 0.5) +
  scale_fill_hue(c = 80) +
  theme(panel.background = element_rect(fill = "white", colour="grey"),panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
  labs(x="",y="número de paises",title="Evolución")

# distribución del cambio
par(mfrow=c(1,1))
hist(data$resta,main = "Evolución felicidad", xlab = "Cambio", ylab = "Frecuencia", col = "pink")

# 5 que mas mejoran y empeoran 
emp5 = data[order(data$resta,decreasing=FALSE)[1:5],]
mej5 = data[order(data$resta,decreasing=TRUE)[1:5],]

# grafico
par(mfrow=c(1,2))
barplot(height = mej5$resta, names = mej5$id, main = "Mejoran", ylab = "Cambio",
        col = "#3891a6", las = 2, ylim = c(-0.5,0.5))
barplot(height = emp5$resta, names = emp5$id, main = "Empeoran", ylab = "Cambio", 
        col = "#b80c09", las = 2, ylim = c(-0.5,0.5))


# STACKED BARPLOT TODOS
rm(list=ls())
# Fuente: https://www.r-graph-gallery.com/circular-barplot.html

# librerías
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)

# subir los datos 
data <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)

# cambiar nombre a las variables y factores de group
data[c(1,4,12)] = NULL
names(data)[1] = "individual"
names(data)[2] = "group"
levels(data$group) <- c("A","B","C","D","E","F","G","H","I","J")

# verticalizar
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# crear barras con NA para usar de separador entre grupos
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep(seq(1, nrow(data)/nObsType), each=nObsType)

# nombre y posición del nombre de cada barra
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# crear lineas para el circulo interior
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# hacer las lineas de escala
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# hacer gráfico
p <- ggplot(data) +      
  
  # barras apiladas
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # añadir las lineas para la escala
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2.5, xend = start, yend = 2.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 7.5, xend = start, yend = 7.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # escribir texto de la escala
  ggplot2::annotate("text", x = rep(max(data$id),4), y = c(0, 2.5, 5, 7.5), label = c("0", "2.5", "5", "7.5") , color="grey", size=2.5 , angle=0, fontface="bold", hjust=1) +
  
  # límites y tema
  ylim(-10,10) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # poner nombres de las barras
  geom_text(data=label_data, aes(x=id, y=tot+1, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # poner nombres del círculo interior
  geom_segment(data=base_data, aes(x = start, y = -1, xend = end, yend = -1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -2, label=group), hjust=c(0.5,1,1,1,1,0,0,0,0,0), colour = "black", alpha=0.7, size=3, fontface="bold", inherit.aes = FALSE)

# ejecutar gráfico
p


# STACKED BARPLOT TOP
rm(list=ls())
# Fuente: https://www.r-graph-gallery.com/circular-barplot.html

# subir librerías
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)

# subir datos
data <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)

# hacer data frames del top 10 y ultimos 10, guardarlos como csv
top20 = data[order(data$Rank,decreasing=FALSE)[1:20],]
last20 = data[order(data$Rank,decreasing=TRUE)[1:20],]
write.csv(top20, "top20.csv", row.names = FALSE)
write.csv(last20, "last20.csv", row.names = FALSE)

rm(list=ls())
#subimos los csv creados, los juntamos y eliminamos las variables que no nos interesan
data1 <- read.csv("top20.csv", header = TRUE, stringsAsFactors = TRUE)
data2 <- read.csv("last20.csv", header = TRUE, stringsAsFactors = TRUE)
data <- rbind(data1,data2)
data[c(1,4,12)] = NULL

# cambiar nombres a las variables y a los niveles de group 
names(data)[1] = "individual"
names(data)[2] = "group"
levels(data$group) <- c("A","B","C","D","E","F","G","H")

# verticalizar
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# crear barras con NA para usar de separador entre grupos
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# nombre y posición del nombre de cada barra
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# crear lineas para el circulo interior
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# hacer las lineas de escala
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# hacer gráfico
p <- ggplot(data) +      
  
  # barras apiladas
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # añadir las lineas para la escala
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2.5, xend = start, yend = 2.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 7.5, xend = start, yend = 7.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # escribir texto de la escala
  ggplot2::annotate("text", x = rep(max(data$id),4), y = c(0, 2.5, 5, 7.5), label = c("0", "2.5", "5", "7.5") , color="grey", size=2.5 , angle=0, fontface="bold", hjust=1) +
  
  # límites y tema 
  ylim(-10,10) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # poner nombres de las barras
  geom_text(data=label_data, aes(x=id, y=tot+1, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # poner nombres del círculo interior
  geom_segment(data=base_data, aes(x = start, y = -1, xend = end, yend = -1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -2, label=group), hjust=c(0.5,1,1,1,1,0,0,0), colour = "black", alpha=0.7, size=3, fontface="bold", inherit.aes = FALSE)

# ejecutar gráfico
p


# API GOOGLE
rm(list=ls())

# cargar librerias
library(curl)
library(geosphere)
library(leaflet)
library(maps)
library(rgeolocate)
library(rworldmap)
library(ggmap)
library(googleway)

# cargar key de la API
register_google(key = "AIzaSyCDug0_AK6eDH-bHdMDCIeampsprYFc0LU")

# cargar datos
data <- read.csv("data15.csv", header = TRUE, stringsAsFactors = TRUE)

# vemos que el número 1 fue Suiza
data[order(data$Rank,decreasing=FALSE)[1],]

suiza <- tryCatch(geocode("Berna, Suiza", output = "latlona", source = "google"),
                 warning = function(w) data.frame(lon = NA, lat = NA, address = NA))

# foto de Berna, capital de Suiza
mapgilbert1 <- get_map(location = c(lon = suiza$lon, lat = suiza$lat), zoom = 13,
                      maptype = "satellite", scale = 2)

mapaSuiza = ggmap(mapgilbert1) + 
            geom_point(aes(x = lon, y = lat, colour = "red"),data = suiza,alpha = .9)

plot(mapaSuiza)

# vemos que el último fue Togo
data[order(data$Rank,decreasing=TRUE)[1],]

togo <- tryCatch(geocode("Lomé, Togo", output = "latlona", source = "google"),
                  warning = function(w) data.frame(lon = NA, lat = NA, address = NA))

# foto de Lomé, capital de Togo
mapgilbert2 <- get_map(location = c(lon = togo$lon, lat = togo$lat), zoom = 13,
                      maptype = "satellite", scale = 2)

mapaTogo = ggmap(mapgilbert2) + 
  geom_point(aes(x = lon, y = lat, colour = "red"),data = togo,alpha = .9)

plot(mapaTogo)

