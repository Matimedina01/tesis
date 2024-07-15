library(dplyr)
library("xlsx")
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

#REEMPLAZAR VALORES POR CADA COLUMNA
#REEMPLAZAR NOMBRES DE COLUMNAS
#Cambiar el nombre de las clumnas que requieran
colnames(df)[1] <- "idres"
colnames(df)[2] <- "year"
colnames(df)[3] <- "month"
colnames(df)[4] <- "day"
#Cambiamos el nombre de valores categoricos para facilitar lectura del código
df$departamento = ifelse(df$departamento == "Ãfâ???~EEMBUCU","NEEMBUCU",df$departamento)


#ELIMINAMOS VALORES
#En primer lugar 3668 valores vacios de ano a origen
x = !is.na(df$year)
df = df[x,]

#Eliminamos los 44.978 de departamento y origen
depa_logical = df$departamento != ""
df = df[depa_logical,]
df$origen = ifelse(df$origen == "'-",NA,df$origen)
df$origen = ifelse(df$origen == "Completar Origen...",NA,df$origen)

df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "CAMPO BOREL","CAMPO BOREAL",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN A.PICO","FORTIN AMERICO PICO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN AMERICO","FORTIN AMERICO PICO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN A. PICO","FORTIN AMERICO PICO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FTIN. AMERICO PICO","FORTIN AMERICO PICO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FTN. TTE. MARTINEZ","FORTIN TENIENTE MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "TTE MARTINEZ","FORTIN TENIENTE MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "TTE. MARTINEZ","FORTIN TENIENTE MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN C. ANTONIO","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN C.ANTONIO","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN CARLOS A","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN CARLOS A.","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN CARLOS ANTONIO","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FORTIN CARLOS A. LOPEZ","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "FTIN. CARLOS ANTONIO","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "F.C.A. LOPEZ","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "LA VISTORIA","LA VICTORIA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "MAYAR PABLO LAG","MAYOR PABLO LAGERENZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "MAYOR P LAGERENZA","MAYOR PABLO LAGERENZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "MAYOR P.LAGERENZA","MAYOR PABLO LAGERENZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "MAYOR PABLO LA GERENSA","MAYOR PABLO LAGERENZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "MAYOR PABLO LAG","MAYOR PABLO LAGERENZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "MAYOR PABLO LAGERE","MAYOR PABLO LAGERENZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "LA ESPERANZA","PUERTO LA EZPERANZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "PTO LA ESPERANZA","PUERTO LA EZPERANZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "PTO LA  ESPERANZA","PUERTO LA EZPERANZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "PTO. LA ESPERANZA","PUERTO LA EZPERANZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "PTO.LA ESPERANZA","PUERTO LA EZPERANZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "PUERTO LA ESPERANZA","PUERTO LA EZPERANZA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "PTO. GUARANI","PUERTO GUARANI",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "PTO.GUARANI","PUERTO GUARANI",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "SANTO DOMIGO","SANTO DOMINGO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "STO. DOMINGO","SANTO DOMINGO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "COL. MENNO","COLONIA MENNO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "COL.MENNO","COLONIA MENNO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "TENIENTE MARTINEZ","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "TTE.MARTINEZ","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "MCAL. ESTIGARRIBIA","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARAGUAY" & df$origen == "TTE. MONTANIA","TENIENTE MONTANIA",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARANA" & df$origen == "AGAGUIGO","AGA GUYJHO",df$origen)
df$origen = ifelse(df$departamento == "ALTO PARANA" & df$origen == "FORTIN CARLOS A","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "CAP. BADO","CAPITAN BADO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "CAP.BADO","CAPITAN BADO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "CAPITÃN BADO","CAPITAN BADO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "PEDRO J. CABALLERO","PEDRO JUAN CABALLERO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "PEDRO JUAN C.","PEDRO JUAN CABALLERO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "PEDRP JUAN CABALLERO","PEDRO JUAN CABALLERO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "BELLA BISTA","BELLA VISTA",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "COL. FRIESLAND","COLONIA FRIESLAND",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "COL. MENNO","COLONIA MENNO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "FORTIN A. PICO","FORTIN AMERICO PICO",df$origen)
df$origen = ifelse(df$departamento == "AMAMBAY" & df$origen == "FTN. TTE. MARTINEZ","FORTIN TENIENTE MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "AYALA VELASQUES","AYALA VELAZQUEZ",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "AYALA VELASQUEZ","AYALA VELAZQUEZ",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL FERNHEIM","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL. FERNHEIM","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL. FERNHEIN","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL.FERNHEIM","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COLONIA FERNHEIN","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL NEULAND","COLONIA NEULAND",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL. NEULAND","COLONIA NEULAND",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COLONIA  NEULAND","COLONIA NEULAND",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COLONIA NCOLUND","COLONIA NEULAND",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COLONIA NEULAND","COLONIA NEULAND",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL. MENNO","COLONIA MENNO",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL.MENO","COLONIA MENNO",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "FORTIN N. ASUNCION","FORTIN NUEVA ASUNCION",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "FORTIN NUEVA AS","FORTIN NUEVA ASUNCION",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "LA","LA PATRIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "MARICAL ESTIGARRIB","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "MARICAL ESTIGARRIBIA","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "MCAL ESTIGARRIBIA","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "MCAL. ESTIGARRIBIA","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "MCAL.ESTIGARRIBIA","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "MRCAL. ESTIGARRIBIA","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "PEDRO P PEÃ'A","PEDRO P.PEÃ'A",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "PEDRO P. PEÃ'A","PEDRO P.PEÃ'A",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "PRATIS","PRATTS GILL",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "PRATT GILL","PRATTS GILL",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "PRATTS  GILL","PRATTS GILL",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "PTATTS GILL","PRATTS GILL",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TENIENTE MONTAN","TENIENTE MONTANIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TTE. MONTANIA","TENIENTE MONTANIA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TTE. ENCIISO","TENIENTE ENCISO",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TTE. ENCISO","TENIENTE ENCISO",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TTE.ENCISO","TENIENTE ENCISO",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TTE. MARTINEZ","TENIENTE MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TTE ACOSTA","TENIENTE ACOSTA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TTE. ACOSTA","TENIENTE ACOSTA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "ZONA PARAGUAYAL","ZONA PARAGUAYA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "ZONA PYA","ZONA PARAGUAYA",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "A CONFIRMAR",NA,df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL.FERNHEIN","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL.MENNO","COLONIA MENNO",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "COL.NEULAND","COLONIA NEULAND",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "FTN. TTE. MARTINEZ","FORTIN TENIENTE MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "PTO.PINASCO","PUERTO PINASCO",df$origen)
df$origen = ifelse(df$departamento == "BOQUERON" & df$origen == "TENIENTE MARTINEZ","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "COL. SOMMERFELD","COLONIA SOMMERFELD",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "COL. SOMMERFFEL","COLONIA SOMMERFELD",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "COL.SEMMERFELD","COLONIA SOMMERFELD",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "COLONIA SOMMERFERLD","COLONIA SOMMERFELD",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "COLONIA SONMERFEL","COLONIA SOMMERFELD",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "DR JUAN MANUEL FRUTOS","DR. JUAN MANUEL FRUTOS",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "DR. JUAN M. FRUTOS","DR. JUAN MANUEL FRUTOS",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "DR. MANUEL FRUTOS","DR. JUAN MANUEL FRUTOS",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "J EULOGIO ESTIGARRIBIA","JOSE EULOGIO ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "J. EULOGIO ESTIGARRIBIA","JOSE EULOGIO ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "JOSE E. ESTIGARRIBIA","JOSE EULOGIO ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "RAUL A. OVIEDO","RAUL ARSENIO OVIEDO",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "RAUL A.OVIEDO","RAUL ARSENIO OVIEDO",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "SANTA DEL MBUTUY","SANTA ROSA DEL MBUTUY",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "SANTA ROSA DE MBUTUY","SANTA ROSA DEL MBUTUY",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "STA ROSA DEL MBUTUY","SANTA ROSA DEL MBUTUY",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "STA. ROSA DEL MBUTUY","SANTA ROSA DEL MBUTUY",df$origen)
df$origen = ifelse(df$departamento == "CAAGUAZU" & df$origen == "GRAL. H. MORINIGO","GRAL. HIGINIO MORINIGO",df$origen)
df$origen = ifelse(df$departamento == "CAAZAPA" & df$origen == "MOISES S. BERTONI","MOISES BERTONI",df$origen)
df$origen = ifelse(df$departamento == "CAAZAPA" & df$origen == "GRAL. H. MORINIGO","GRAL. HIGINIO MORINIGO",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "CORPUS CHRISTI","CORPUS CRISTI",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "CURUGAUTY","CURUGUATY",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "CURUGUATYcan","CURUGUATY",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "FCO. CABALLERO","FRANCISCO CABALLERO",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "ITANATA","ITANARA",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "SALTO DE GUAIRA","SALTOS DEL GUAIRA",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "SALTO DEL GUAIRA","SALTOS DEL GUAIRA",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "SALTO DEL GUAVIRA","SALTOS DEL GUAIRA",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "SALTOS DEL GUAITA","SALTOS DEL GUAIRA",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "SALTOS DEL GUIRA","SALTOS DEL GUAIRA",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "YASY CANY","YASY CAÃ'Y",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "YPEJHU","YPE JHU",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "SAN ESTANISLAO","SAN ESTANISLAO",df$origen)
df$origen = ifelse(df$departamento == "CANINDEYU" & df$origen == "TTE. ENCISO","TENIENTE ENCISO",df$origen)
df$origen = ifelse(df$departamento == "CENTRAL" & df$origen == "MARIANO R. ALONZO","MARIANO ROQUE ALONZO",df$origen)
df$origen = ifelse(df$departamento == "CONCEPCION" & df$origen == "SAN CARLO","SAN CARLOS",df$origen)
df$origen = ifelse(df$departamento == "CONCEPCION" & df$origen == "con","CONCEPCION",df$origen)
df$origen = ifelse(df$departamento == "CORDILLERA" & df$origen == "ARROYO Y ESTERO","ARROYOS Y ESTEROS",df$origen)
df$origen = ifelse(df$departamento == "CORDILLERA" & df$origen == "ARROYOS Y ESTERO","ARROYOS Y ESTEROS",df$origen)
df$origen = ifelse(df$departamento == "CORDILLERA" & df$origen == "ITACURUBI DE LAS CORDILLERAS","ITACURUBI DE LA CORDILLERA",df$origen)
df$origen = ifelse(df$departamento == "CORDILLERA" & df$origen == "PIRIBEBEUY","PIRIBEBUY",df$origen)
df$origen = ifelse(df$departamento == "GUAIRA" & df$origen == "FELIX P. CARDOZO","FELIX PEREZ CARDOZO",df$origen)
df$origen = ifelse(df$departamento == "GUAIRA" & df$origen == "VILLA RICA","VILLARRICA",df$origen)
df$origen = ifelse(df$departamento == "GUAIRA" & df$origen == "VILLARICA","VILLARRICA",df$origen)
df$origen = ifelse(df$departamento == "GUAIRA" & df$origen == "COL. INDEPENDENCIA","COLONIA INDEPENDENCIA",df$origen)
df$origen = ifelse(df$departamento == "ITAPUA" & df$origen == "GENERAL DELGADO","GRAL. DELGADO",df$origen)
df$origen = ifelse(df$departamento == "ITAPUA" & df$origen == "SAN RAFAEL","SAN RAFAEL DEL PARANA",df$origen)
df$origen = ifelse(df$departamento == "ITAPUA" & df$origen == "SAN RAFAEL DEL P.","SAN RAFAEL DEL PARANA",df$origen)
df$origen = ifelse(df$departamento == "ITAPUA" & df$origen == "TOMAS R. PEREIRA","TOMAS ROMERO",df$origen)
df$origen = ifelse(df$departamento == "ITAPUA" & df$origen == "CARLOS A, LOPEZ","CARLOS A. LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "MISIONES" & df$origen == "BELLA VISTA (MUNICIPIO)","BELLA VISTA",df$origen)
df$origen = ifelse(df$departamento == "MISIONES" & df$origen == "CAPITAN BADO (MUNICIPIO)","CAPITAN BADO",df$origen)
df$origen = ifelse(df$departamento == "NEEMBUCU" & df$origen == "SAN JUAN BAUTISTA DEL Ã'EEMBUCU","SAN JUAN BAUTISTA",df$origen)
df$origen = ifelse(df$departamento == "NEEMBUCU" & df$origen == "SAN JUAN BTA DEL Ã'EEMBUCU","SAN JUAN BAUTISTA",df$origen)
df$origen = ifelse(df$departamento == "NEEMBUCU" & df$origen == "SAN JUAN  BAUTISTA","SAN JUAN BAUTISTA",df$origen)
df$origen = ifelse(df$departamento == "NEEMBUCU" & df$origen == "TTE.ESTEBAN MARTINEZ","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "NEEMBUCU" & df$origen == "GRAL.DIAZ","GENERAL DIAZ",df$origen)
df$origen = ifelse(df$departamento == "PARAGUARI" & df$origen == "COL.FERNHEIN","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "PARAGUARI" & df$origen == "COL.MENNO","COLONIA MENNO",df$origen)
df$origen = ifelse(df$departamento == "PARAGUARI" & df$origen == "COL.NEULAND","COLONIA NEULAND",df$origen)
df$origen = ifelse(df$departamento == "PARAGUARI" & df$origen == "TTE.ACOSTA","TENIENTE ACOSTA",df$origen)
df$origen = ifelse(df$departamento == "PARAGUARI" & df$origen == "TTE.MONTANIA (KM.220)","TENIENTE MONTANIA",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "10 LEGUA","10 LEGUAS",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "10LEGUAS","10 LEGUAS",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "AVALOS SANCHES","AVALOS SANCHEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "AVALOS SANHEZ","AVALOS SANCHEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "CADETE PANDO","CADETE PASTOR PANDO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "CADETE P. PANDO","CADETE PASTOR PANDO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "CAMPOP ACEVAL","CAMPO ACEVAL",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "COL. FALCON","COLONIA FALCON",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "COLONIA CEIB0","COLONIA CEIBO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "COL.CEIBO","COLONIA CEIBO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FORTIN C","FORTIN C. ANTONIO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FORTIN CAMPO VI","FORTIN CAMPO VIA",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FORTIN RIO VERD","FORTIN RIO VERDE",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FOTIN RIO VERDE","FORTIN RIO VERDE",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FTIN. RIO VERDE","FORTIN RIO VERDE",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FOTIN CABALLERO","FORTIN CABALLERO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FTIN. ZALAZAR","FORTIN ZALAZAR",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "GRAL BRUGUEZ","GENERAL BRUGUEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "GRAL. BRUGUEZ","GENERAL BRUGUEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "GRAL. BURGUEZ","GENERAL BRUGUEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "GRAL DIAZ","GENERAL DIAZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "GRAL. DIAZ","GENERAL DIAZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "GRAL.DIAZ","GENERAL DIAZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "IRALA  FERNANDEZ","IRALA FERNANDEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "IRALA FERNANDE","IRALA FERNANDEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "LOLI","LOLITA",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "MADREJONCITO","MADREJON",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "POZO COLORADO.","POZO COLORADO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "PTO. CASADO","PUERTO CASADO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "PTO.CASADO","PUERTO CASADO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "PTO.PINASCO","PUERTO PINASCO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "PURTO PINASCO","PUERTO PINASCO",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "TIEGUE","TIEGE",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "TIGUE","TIEGE",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "TTE ESTEBAN M.","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "TTE MARTINEZ","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "TTE. MARTINEZ","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "TTE.MARTINEZ","TTE. ESTEBAN MARTINEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "COL. FERNHEIN","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "FORTIN C. ANTONIO","FORTIN CARLOS ANTONIO LOPEZ",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "MARICAL ESTIGARRIB","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "MCAL. ESTIGARRIBIA","MARISCAL ESTIGARRIBIA",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "PTO.GUARANI","PUERTO GUARANI",df$origen)
df$origen = ifelse(df$departamento == "PTE. HAYES" & df$origen == "STO.DOMINGO","SANTO DOMINGO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "CAPIBARY","CAPIIBARY",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "CAPI IBARY","CAPIIBARY",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL. MANITOVA","COLONIA MANITOVA",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL.MANITOVA","COLONIA MANITOVA",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL. SANTA CLARA","COLONIA SANTA CLARA",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL.SANTA CLARA","COLONIA SANTA CLARA",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL.STA CLARA","COLONIA SANTA CLARA",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL.STA ROSA","COL. STA ROSA",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL. FERNHEIN","COLONIA FERNHEIM",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL. FRIESLAND","COLONIA FRIESLAND",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COLONIA VOLEDAN","COLONIA VOLENDAN",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COL. VOLENDAN","COLONIA VOLENDAN",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "COLONIA VOLENDON","COLONIA VOLENDAN",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "GRAL AQUINO","GENERAL AQUINO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "GRAL. RESQUIN","GENERAL RESQUIN",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "GRAL.RESQUIN","GENERAL RESQUIN",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "GRAL RESQUIN","GENERAL RESQUIN",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "GUAYAIBY","GUAYAYBY",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "GUAYAYBI","GUAYAYBY",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "ITAC. DEL ROSARIO","ITACURUBI DEL ROSARIO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "ITAKURUBI DEL ROSARIO","ITACURUBI DEL ROSARIO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "PTO. LA ESPERANZA","PUERTO LA ESPERANZA",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "PUERTO YBAPOVO","PUERTO YBAPOBO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "PTO.YBAPOBO","PUERTO YBAPOBO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "PTO. YBAPOBO","PUERTO YBAPOBO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "SAN ESTANILAO","SAN ESTANISLAO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "SAN ESTANISLADO","SAN ESTANISLAO",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "SAN P.DE COCUERE","SAN PABLO DE COCUERE",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "SANTA DEL AGUARAY","SANTA ROSA DEL AGUARAY",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "STA.ROSA DEL AGUARAY","SANTA ROSA DEL AGUARAY",df$origen)
df$origen = ifelse(df$departamento == "SAN PEDRO" & df$origen == "YASY KAÃ'Y","YASY CAÃ'Y",df$origen)


depas='/Users/matmedina/Documents/2024/tesis/depas.csv'
depas = read.csv(depas, sep=";")
depas = depas %>% select(-animales_faneados)

for(nros in 1:nrow(depas)){
df$departamento = ifelse(df$departamento == depas$departamento[nros] & df$origen == depas$origen[nros],depas$correcto[nros],df$departamento)
print(nros)
}

df$departamento = ifelse(df$departamento == "ALTO PARAGUAY ","ALTO PARAGUAY",df$departamento)
df$departamento = ifelse(df$departamento == "ALTO PARANA ","ALTO PARANA",df$departamento)
df$departamento = ifelse(df$departamento == "AMAMBAY ","AMAMBAY",df$departamento)
df$departamento = ifelse(df$departamento == "BOQUERON ","BOQUERON",df$departamento)
df$departamento = ifelse(df$departamento == "CAAZAPA ","CAAZAPA",df$departamento)
df$departamento = ifelse(df$departamento == "CANINDEYU ","CANINDEYU",df$departamento)
df$departamento = ifelse(df$departamento == "CENTRAL ","CENTRAL",df$departamento)
df$departamento = ifelse(df$departamento == "CONCEPCION ","CONCEPCION",df$departamento)
df$departamento = ifelse(df$departamento == "ITAPUA ","ITAPUA",df$departamento)
df$departamento = ifelse(df$departamento == "NEEMBUCU ","NEEMBUCU",df$departamento)
df$departamento = ifelse(df$departamento == "PARAGUARI ","PARAGUARI",df$departamento)
df$departamento = ifelse(df$departamento == "PTE. HAYES ","PTE. HAYES",df$departamento)
df$departamento = ifelse(df$departamento == "SAN PEDRO ","SAN PEDRO",df$departamento)

df$sexo = ifelse(df$clase == "VA" & df$sexo == "","Z",df$sexo)
df$sexo = ifelse(df$clase == "NO" & df$sexo == "","X",df$sexo)



logical = df$cobertura != "SD" & df$conformacion != "SD"& df$conformacion != "'-"
df = df[logical,]

df$contusion = ifelse(df$contusion == "'-",0,df$contusion)




peso_logic = df$peso >= 110 & df$peso <= 400
df = df[peso_logic,]

x = !is.na(df$year)
df = df[x,]





anterior = sum(df$peso==248)
siguiente = sum(df$peso==250)
media = (anterior + siguiente)/2
porcentaje = media/sum(df$peso==249)
porcentaje
limpiar_peso <- df[df$peso == 249, ]
limpiar_peso <- limpiar_peso %>%
  sample_frac(porcentaje)  # Mantener el 70% de las filas aleatoriamente
# Actualizar el dataframe original df_limpio
df <- df[!(df$peso == 249), ]
df <- rbind(df, limpiar_peso)

df1 = df
df1$tipificacion <- ifelse(df1$denticion<=2 & df1$clase %in% c("NT", "VQ") & df1$conformacion %in% c("EE","MB","BB") & df1$cobertura == 2 & df1$contusion<=1 ,"PREMIUM", 
                           ifelse(df1$denticion<=4 & df1$clase %in% c("NT", "VQ","NJ","VL") & df1$conformacion %in% c("EE","MB","BB") & df1$cobertura %in% c(1,2) & df1$contusion<=1 |df1$denticion<=2 & df1$clase %in% c("TJ", "TT") & df1$conformacion %in% c("EE","MB","BB") & df1$cobertura %in% c(1,2) & df1$contusion<=1,"SUPERIOR",
                                  ifelse(df1$denticion<=6 & df1$clase %in% c("NT", "VQ","NJ","VL","NO","VA") & df1$conformacion %in% c("EE","MB","BB","RR") & df1$cobertura %in% c(1,2,3) & df1$contusion<=2,"STANDARD",
                                         ifelse(df1$conformacion %in% c("EE","MB","BB","RR","IN") & df1$cobertura %in% c(0,1,2,3) & df1$contusion<=2,"GENERAL","MANUFACTURA"))))
df1$paga_por_peso = ifelse(df1$peso >= 180 & df1$clase %in% c("VQ","VL","VA"),TRUE,
                           ifelse(df1$peso>= 220 & df1$clase %in% c("NT","NJ","NO","TT","TJ","TO"),TRUE,FALSE))







df_final = df1



s =summary(df_final)



