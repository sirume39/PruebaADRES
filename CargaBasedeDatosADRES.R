
#Hola soy Simón, Gracias por esta oportunidad!
#Cualquier duda o inquietud te dejo mi correo:
#simonrume3911@gmail.com

#LIBRERIAS-------------------------------------------------------------------------------------------------
# install.packages("dplyr")
# install.packages("data.table")
#install.packages("DBI")
#install.packages("RSQLite")

library(dplyr)
library(ggplot2)
library(data.table)
library(DBI)
library(RSQLite)

#Directorio de trabajo-------------------------------------------------------------------------------------
setwd("C:/SIMON/PruebaADRES")

#Lectura de los Datos--------------------------------------------------------------------------------------
dfMunicipios <- fread("MunicipiosCSV.csv")
dfPrestadores <- fread("PrestadoresCSV.csv")

#Exploracion de datos----------------------------------------------------------------------------------

head(dfMunicipios)#Permite analizar los primeros 5 datos del dataset
head(dfPrestadores)

tail(dfMunicipios)#Permite analizar los ultimos 5 datos del dataset
tail(dfPrestadores)

str(dfMunicipios)#Permite una visualizacion de los tipos de datos
str(dfPrestadores)

summary(dfMunicipios)#Da nun resumen de las columnas, para analizar cada una 
summary(dfPrestadores)

table(dfMunicipios$Departamento)#muestra las diferentes etiquetas en una columna
table(dfMunicipios$Municipio)

#Se eliminan caracteres diferentes a los estipulados en las comillas y se trabajan en minusculas
dfMunicipios$Departamento <- gsub("[^a-zñáéíóú.]", "", tolower(dfMunicipios$Departamento)) 
dfMunicipios$Municipio <- gsub("[^a-zñáéíóú.]", "", tolower(dfMunicipios$Municipio))
dfMunicipios$Municipio <- ifelse(dfMunicipios$Municipio == "bogotádc", "bogotá", dfMunicipios$Municipio) #Ajustar nombre de Municipio

dfPrestadores$depa_nombre <- gsub("[. ]", "", tolower(dfPrestadores$depa_nombre))
dfPrestadores$muni_nombre <- gsub("[. ]", "", tolower(dfPrestadores$muni_nombre))


#Se camnbian los valores vacios por valores NA
for (columna in names(dfPrestadores)) {
  dfPrestadores[[columna]][dfPrestadores[[columna]] == ""] <- NA
}

#Grafica de datos faltantes en Municipios
existing_data_proportions <- 1 - apply(is.na(dfMunicipios), 2, mean)
existing_data <- data.frame(Columna = names(existing_data_proportions), Proporcion = existing_data_proportions)
etiquetas <- round(existing_data_proportions*100,2)

#png("Grafica0DatosFaltantesMunicipios.png")
ggplot(data = existing_data, aes(x = reorder(Columna, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(x = "Columna", y = "Porcentaje de Datos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_hline(yintercept = 1, linetype = "longdash", color = "red") +
  geom_text(aes(label = etiquetas), vjust = -0.5, size = 3) 
#dev.off()

#Grafica de datos faltantes en Prestadores
existing_data_proportions <- 1 - apply(is.na(dfPrestadores), 2, mean)
existing_data <- data.frame(Columna = names(existing_data_proportions), Proporcion = existing_data_proportions)
etiquetas <- round(existing_data_proportions*100,1)

#png("Grafica0DatosFaltantesPrestadores.png")
ggplot(data = existing_data, aes(x = reorder(Columna, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(x = "Columna", y = "Porcentaje de Datos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_hline(yintercept = 1, linetype = "longdash", color = "red") +
  geom_text(aes(label = etiquetas), vjust = -0.5, size = 3) 
#dev.off()

#Rectificación de valores en columnas con el 100% de datos
table(dfPrestadores$habilitado)
table(dfPrestadores$fecha_corte_REPS)

#Carga en la Base de datos---------------------------------------------------------------------------------
basededatos <- dbConnect(SQLite(), dbname = "BasededatosADRES.sqlite")

dbWriteTable(basededatos,"Municipios", dfMunicipios) #se carga en la tabla Municipios
dbWriteTable(basededatos,"Prestadores", dfPrestadores)#se carga en la tabla Prestadores

#Desconexión de la base de datos---------------------------------------------------------------------------------------
dbDisconnect(basededatos)












