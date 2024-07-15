
library(dplyr)
library(readxl)
library(tidyverse)

#IMPORTAR Y UNIR ARCHIVOS
#Colocar el link del destino de los archivos de excel
file='/Users/matmedina/Documents/2024/tesis/Faena/'
#Crear un array de los archivos en el destino
lista_archivos = list.files(file)
lista_archivos
#Leer el primer archivo
df = read.csv(paste0(file,lista_archivos[1]))
#Leer y unir todos los otros acumulativamente
for(nro in 2:length(lista_archivos)){
df = rbind(df,read.csv(paste0(file,lista_archivos[nro])))
}
head(df)
nrow(df)





#DETECTAR VALORES POR CADA COLUMNA
#1
#2,3,4 son fechas 
conteo_ano <- df %>%
  group_by(year) %>%
  summarize(animales_faneados = n())
print(conteo_ano)






#5-establecimientos
conteo_frigorificos <- df %>%
  group_by(establecimiento) %>%
  summarize(animales_faneados = n())
conteo_frigorificos$establecimiento <- factor(conteo_frigorificos$establecimiento, levels = unique(conteo_frigorificos$establecimiento[order(conteo_frigorificos$animales_faneados)]))
print(conteo_frigorificos)
grafico_frigorificos = conteo_frigorificos %>%ggplot(aes(x = establecimiento,y = animales_faneados, fill = establecimiento)) +
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
print(grafico_frigorificos)
vacios = df$establecimiento == ""
vacio = df[vacios,]
vacio = vacio
nrow(vacio)
vacio






#6-idpropietario
conteo_propietarios <- df %>%
  group_by(idpropietario) %>%
  summarize(animales_faneados = n())
print(conteo_propietarios)
nrow(conteo_propietarios)




#7-departamento
#CAMBIAMOS
conteo_departamento <- df %>%
  group_by(departamento) %>%
  summarize(animales_faneados = n())
conteo_departamento_correcto <- df1 %>%
  group_by(departamento) %>%
  summarize(animales_faneados = n())

tabla_combinada <- merge(conteo_departamento, conteo_departamento_correcto, by = "departamento", suffixes = c("_1", "_2"))
tabla_combinada$Division <- tabla_combinada$animales_faneados_2 / tabla_combinada$animales_faneados_1

conteo_departamento$departamento <- factor(conteo_departamento$departamento, levels = unique(conteo_departamento$departamento[order(conteo_departamento$animales_faneados)]))
conteo_departamento_correcto$departamento <- factor(conteo_departamento_correcto$departamento, levels = unique(conteo_departamento_correcto$departamento[order(conteo_departamento_correcto$animales_faneados)]))

print(conteo_departamento)
print(conteo_departamento_correcto)

grafico_frigorificos = conteo_departamento %>%ggplot(aes(x = departamento,y = animales_faneados, fill = departamento)) +
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
print(grafico_frigorificos)

grafico_frigorificos_correcto = conteo_departamento_correcto %>%ggplot(aes(x = departamento,y = animales_faneados, fill = departamento)) +
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
print(grafico_frigorificos_correcto)




inv1 = df$departamento == ""
inv1 = df[inv1,]





#8-Origen
conteo_origen <- df %>%
  group_by(departamento, origen) %>%
  summarize(animales_faneados = n())


#9-Clase PERFECTO
conteo_clase <- df %>%
  group_by(clase) %>%
  summarize(animales_faneados = n())
print(conteo_clase)




#10-SExo
conteo_sexo <- df %>%
  group_by(sexo) %>%
  summarize(animales_faneados = n())
print(conteo_sexo)


#11-Denticion
conteo_denticion <- df %>%
  group_by(denticion) %>%
  summarize(animales_faneados = n())
print(conteo_denticion)




#12-Raza
conteo_raza <- df %>%
  group_by(raza) %>%
  summarize(animales_faneados = n())
print(conteo_raza)




#13-conformacion
conteo_conformacion <- df %>%
  group_by(conformacion) %>%
  summarize(animales_faneados = n())
print(conteo_conformacion)




#14-Cobertura
conteo_cobertura <- df %>%
  group_by(cobertura) %>%
  summarize(animales_faneados = n())
print(conteo_cobertura)




#15-Contusión

conteo_contusion <- df %>%
  group_by(contusion) %>%
  summarize(animales_faneados = n())
print(conteo_contusion)





#16-peso
#LIMPIAR LA COLUMNA DE PESO
#A continuación se pueden observar los valores sucios en gráficos
conteo_peso <- df %>%
  group_by(peso) %>%
  summarize(animales_faneados = n())
print(conteo_peso)
plot(prop.table(table(df$peso)), xlab = "Peso en kg", ylab = "Proporción")
boxplot(df$peso,ylab = "Peso [Kg]")
plot(density(df$peso), xlab = "Peso en kg" ,ylab = "Densidad",main = "")
#A continuación vemos los valores del peso sucio
summary(df$peso)
average = mean(df$peso)
SD = sd(df$peso)
c(average,SD)



#DATASET ELIMINANDO LOS VALORES
df_delete = df$peso >= 110 & df$peso <= 400
nrow(df)-sum(df_delete)
(nrow(df) - sum(df_delete))/nrow(df)
df_limpio = df[df_delete,]
nrow(df_limpio)
head(df_limpio)
#Ploteamos el dataset tras eliminar los valores
plot(prop.table(table(df_limpio$peso)), xlab = "Peso en kg", ylab = "Proporción")
boxplot(df_limpio$peso,ylab = "Peso [Kg]")
d = plot(density(df_limpio$peso))
polygon(d, col="red", border="black")
#A continuación vemos los valores del peso limpio
summary(df_limpio$peso)
average = mean(df_limpio$peso)
SD = sd(df_limpio$peso)
c(average,SD)


#DATASET REEMPLAZANDO LOS VALORES POR LA MEDIA
df_reemplazo = df
df_reemplazo$bol = df_delete
df_reemplazo$peso = ifelse(df_reemplazo$bol == FALSE,mean(df_limpio$peso),df_reemplazo$peso)
df_reemplazo = df_reemplazo %>% select(!c("bol"))
head(df_reemplazo)
nrow(df_reemplazo)
#Ploteamos el dataset tras eliminar los valores
plot(prop.table(table(df_reemplazo$peso)), xlab = "Peso en kg", ylab = "Proporción")
boxplot(df_reemplazo$peso,ylab = "Peso [Kg]")
d = plot(density(df_reemplazo$peso))
polygon(d, col="red", border="black")
#A continuación vemos los valores del peso limpio
summary(df_reemplazo$peso)
average = mean(df_limpio$peso)
SD = sd(df_reemplazo$peso)
c(average,SD)



#INVESTIGACION DE 249
unicos = unique(df_limpio$peso)
porcentaje_unicos = c()
cantidad = c()
copia = c()
for(num in 1:length(unicos)){
inv = df$peso == unicos[num]
inv = df[inv,]
nrow(inv)
num_filas_unicas <- select(inv, -1) %>%
  distinct() %>%
  nrow()
porcentaje = num_filas_unicas/nrow(inv)
cantidad = c(cantidad, nrow(inv))
porcentaje_unicos = c(porcentaje_unicos,porcentaje)
copiado = nrow(inv)-num_filas_unicas
copia = c(copia, copiado)
}
calculo = data.frame(Unico = unicos)
calculo$porcentaje_unicos = porcentaje_unicos
calculo$cantidad = cantidad
calculo$copia = copia

plot(calculo$Unico,calculo$porcentaje_unicos)
boxplot(calculo$porcentaje_unico)
mean(calculo$porcentaje_unicos)
inv = df$peso == 249
inv = df[inv,]
nrow(inv)
num_filas_unicas <- select(inv, -1) %>%
  distinct() %>%
  nrow()
print(num_filas_unicas)
print(num_filas_unicas/nrow(inv))

#Ver los datos vacios y ver cuales son 249
vacios = df$establecimiento == ""
vacio = df[vacios,]
x = vacio$peso == 249
vacio = vacio[x,] 
vacio
nrow(vacio)

#PESO POR FRIGORIFICO
conteo_frigorificos <- df %>%
  group_by(establecimiento) %>%
  summarize(animales_faneados = n(), animales_peso_entero = sum(round(peso) == peso),porcentaje = 100*animales_peso_entero/animales_faneados, peso249 = sum(peso == 249))
print(conteo_frigorificos)

solo_249 = df$peso == 249.0  & df$establecimiento == "FRIGOCHACO" & df$year == 2022
solo_249 = df[solo_249,]
sum(solo_249)

enteros = round(df$peso) != df$peso
sum(enteros)

df_delete = df$peso >= 220 & df$peso <= 240 & df$establecimiento != "FRIGOCHACO" & df$establecimiento != ""

anterior = sum(df$peso==248)
siguiente = sum(df$peso==250)
media = (anterior + siguiente)/2
porcentaje = media/sum(df$peso==249)


df_delete = df$peso >= 110 & df$peso <= 400
df_limpio = df[df_delete,]

limpiar_peso <- df_limpio[df_limpio$establecimiento == "FRIGOCHACO" & df_limpio$peso == 249, ]
limpiar_peso <- limpiar_peso %>%
  sample_frac(porcentaje)  # Mantener el 70% de las filas aleatoriamente
# Actualizar el dataframe original df_limpio
df_limpio <- df_limpio[!(df_limpio$establecimiento == "FRIGOCHACO" & df_limpio$peso == 249), ]
df_limpio <- rbind(df_limpio, limpiar_peso)





#Ploteamos el dataset tras eliminar los valores
plot(prop.table(table(df$peso)), xlab = "Peso en kg", ylab = "Proporción")
boxplot(df_limpio$peso,ylab = "Peso [Kg]")
d = plot(density(df_limpio$peso))
polygon(d, col="red", border="black")
head(df1)










