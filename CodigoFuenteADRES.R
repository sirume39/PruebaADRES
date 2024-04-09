
#Hola soy Simón, Gracias por esta oportunidad!
#Cualquier duda o inquietud te dejo mi correo:
#simonrume3911@gmail.com

#LIBRERIAS-------------------------------------------------------------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("corrplot")
# install.packages("gridExtra")
#install.packages("DescTools")
#install.packages("DBI")
#install.packages("RSQLite")

library(dplyr)
library(ggplot2)
library(data.table)
library(corrplot)
library(gridExtra)
library(DescTools)
library(DBI)
library(RSQLite)

#Directorio de trabajo-------------------------------------------------------------------------------------
setwd("C:/SIMON/PruebaADRES")

#Conexion a la Base de datos---------------------------------------------------------------------------------------------
basededatos <- dbConnect(SQLite(), dbname = "BasededatosADRES.sqlite")

dbListTables(basededatos)#Tablas existentes en la base de datos

#Queries para extraer los datos de la base de datos----------------------------------------------------------------------
regionVSPresta <- dbGetQuery(basededatos,"select Region, count(muni_nombre) as Cantidad 
                                          from Prestadores as p, Municipios as m 
                                          where m.Municipio = p.muni_nombre group by Region;")

muniVSPresta <-  dbGetQuery(basededatos,"select muni_nombre, count(muni_nombre) as Cantidad 
                                        from Prestadores group by muni_nombre;")

muniPob <- dbGetQuery(basededatos,"select Municipio, Poblacion from Municipios;")

clpr <- dbGetQuery(basededatos,"select clpr_nombre, count(clpr_nombre) as Cantidad 
                                from Prestadores GROUP by clpr_nombre;")

caracter <- dbGetQuery(basededatos,"select caracter, count(caracter) as Cantidad 
                                    from Prestadores where caracter is not NULL GROUP by caracter;")

fechasVSclpr <- dbGetQuery(basededatos,"select clpr_nombre,fecha_radicacion, fecha_vencimiento from Prestadores;")

#Tratamiento adicional de datos------------------------------------------------------------------------------------------

regionVSPresta <- regionVSPresta[order(regionVSPresta$Cantidad, decreasing = TRUE),]#Ordenar de mayor a menor
regionVSPresta$Region <- factor(regionVSPresta$Region, levels = regionVSPresta$Region)#Variable categorica a factor para graficar

muniVSPresta <- muniVSPresta[order(muniVSPresta$Cantidad, decreasing = TRUE),]#Ordenar de mayor a menor
muniVSPresta$muni_nombre <- factor(muniVSPresta$muni_nombre, levels = muniVSPresta$muni_nombre)#Variable categorica a factor para graficar

colnames(muniVSPresta)[colnames(muniVSPresta) == "muni_nombre"] <- "Municipio" #Cambiar nombre de columna

pobVSCant <- merge(muniVSPresta, muniPob, by="Municipio")
pobVSCant <- pobVSCant[!duplicated(pobVSCant$Municipio),]
pobVSCant$proporcion <- round(pobVSCant$Poblacion / pobVSCant$Cantidad)
pobVSCant <- pobVSCant[order(pobVSCant$Poblacion, decreasing = TRUE),]#Ordenar de mayor a menor
pobVSCant$Municipio <- factor(pobVSCant$Municipio, levels = pobVSCant$Municipio)#Variable categorica a factor para graficar

conteo <- table(cut(muniVSPresta$Cantidad, breaks = c(0,2,5,10,20,50, Inf)))#Corte en intervalos para el conteo
conteo_df <- as.data.frame(conteo) #Convierte el resultado en datframe para graficar
colnames(conteo_df) <- c("Intervalo", "Cantidad") #Agrega nombre a las columnas
conteo_df$IntervaloGrafico <- c("2 o Menos", "2 a 5", "5 a 10", "10 a 20", "20 a 50", "50 o Más")#Crea los textos de intervalos para la grafica
conteo_df$IntervaloGrafico <- factor(conteo_df$IntervaloGrafico, levels = conteo_df$IntervaloGrafico)#Variable categorica a factor para graficar

clpr$porcentaje <- clpr$Cantidad / sum(clpr$Cantidad) * 100 #Saca el porcentaje de la cantidad de prestadores
caracter$porcentaje <- caracter$Cantidad / sum(caracter$Cantidad) * 100 #Saca el porcentaje de la cantidad de prestadores

clpr$clpr_nombre <- c("IPS", "Objeto Social", "Independiente", "Transporte Especial") #Cambia los valores de la columna

fechasVSclpr$fecha_radicacion <- as.Date(as.character(fechasVSclpr$fecha_radicacion), format = "%Y%m%d")
fechasVSclpr$fecha_vencimiento <- as.Date(as.character(fechasVSclpr$fecha_vencimiento), format = "%Y%m%d")
fechasVSclpr$resta <- fechasVSclpr$fecha_vencimiento - Sys.Date()

conteo2 <- table(cut(as.numeric(fechasVSclpr$resta), breaks = c(-Inf,0,31,365,730,Inf)))#Corte en intervalos para el conteo
conteo_df2 <- as.data.frame(conteo2) #Convierte el resultado en datframe para graficar
colnames(conteo_df2) <- c("Intervalo", "Cantidad") #Agrega nombre a las columnas
conteo_df2$IntervaloGrafico <- c("Vencidos", "Proximos a Vencer (0 a 1 mes)", "1 mes a 1 año", "1 año a 2 años", "2 años o Más")#Crea los textos de intervalos para la grafica
conteo_df2$IntervaloGrafico <- factor(conteo_df2$IntervaloGrafico, levels = conteo_df2$IntervaloGrafico)#Variable categorica a factor para graficar

vencidos = fechasVSclpr %>% filter(resta <= 0) #filtro por vencimiento
conteo_df3 <- as.data.frame(table(vencidos$clpr_nombre))  #hace un conteo de vencimiento por clase de prestador y lo convierte en dataframe
colnames(conteo_df3) <- c("Clase", "Cantidad")#Agrega nombre a las columnas
conteo_df3$Clase <- clpr$clpr_nombre <- c("IPS", "Objeto Social", "Independiente", "Transporte Especial")#Cambia los valores de la columna
conteo_df3$porcentaje <- conteo_df3$Cantidad / sum(conteo_df3$Cantidad) * 100 #saca un porcentaje dentro de los vencidos
#Visualizacion de datos--------------------------------------------------------------------------------------------------

#Grafica de Cantidad de prestadores por Region en Colombia
#png("Grafica1PrestadoresporRegion.png")
ggplot(regionVSPresta, aes(x = Region, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "skyblue") +  
  labs(title = "Cantidad de Prestadores por Region en Colombia", x = "Region", y = "Cantidad de Prestadores")
#dev.off()

#Grafica de los 10 mejores municipios por cantidad de prestadores
#png("Grafica2_10municipiosmasprestadores.png")
ggplot(muniVSPresta[1:10, ], aes(x = Municipio, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Top 10 Municipios con mayor cantidad de prestadores") +
  xlab("Municipio") +
  ylab("Cantidad de Prestadores") + 
  geom_smooth(method = "lm", aes(group = 1), se = FALSE, color="red")
#dev.off()

#Grafica de conteo de municpios por cantidad de prestadores
#png("Grafica3Cantidadprestadorespormunicipio.png")
ggplot(conteo_df, aes(x = IntervaloGrafico, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Conteo de Municipios por Cantidad de Prestadores") +
  xlab("Cantidad de prestadores") +
  ylab("Cantidad de Municipios")
#dev.off()

#Grafica Cantidad de Clase de Prestadores en Colombia
#png("Grafica4Cantidadclasesdeprestadores.png")
ggplot(clpr, aes(x = "", y = Cantidad, fill = clpr_nombre)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 90) +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Cantidad de Clase de Prestadores en Colombia", fill="Clases") +
  geom_text(aes(label = paste0(round(porcentaje), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("skyblue","lightgreen","pink","yellow"))
#dev.off()

#Grafica Cantidad de Caracter dentro de los prestadores publicos
#png("Grafica5CantidadCaracterprestadorespublicos.png")
ggplot(caracter, aes(x = "", y = Cantidad, fill = caracter)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Cantidad de Caracter de Prestadores Publicos en Colombia", fill="Caracter") +
  geom_text(aes(label = paste0(round(porcentaje), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("lightgreen","gray","pink","skyblue","yellow"))
#dev.off()

#Grafica de los 10 municipios mas poblados
#png("Grafica6_10municipiosmaspoblados.png")
ggplot(pobVSCant[1:10, ], aes(x = Municipio, y = Poblacion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Top 10 Municipios con mayor Poblacion") +
  xlab("Municipio") +
  ylab("Poblacion") + 
  geom_smooth(method = "lm", aes(group = 1), se = FALSE, color="red")
#dev.off()

#Grafica con los prestadores y su rango de vencimiento
#png("Grafica7Prestadoresysuvencimiento.png")
ggplot(conteo_df2, aes(x = IntervaloGrafico, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Conteo de Prestadores por Vencimiento") +
  xlab("Tiempo de vencimiento") +
  ylab("Cantidad de prestadores")
#dev.off()

#Grafica Clases de prestadores Vencidos
#png("Grafica8ConteodeclasesporVencimiento.png")
ggplot(conteo_df3, aes(x = "", y = Cantidad, fill = Clase)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 90) +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Conteo de Clases de Prestadores Vencidos en Colombia", fill="Clases") +
  geom_text(aes(label = paste0(round(porcentaje), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("skyblue","lightgreen","pink","yellow"))
#dev.off()

#Desconexión de la base de datos---------------------------------------------------------------------------------------
dbDisconnect(basededatos)



