library(readxl)
base_desigualdades <-
  read_excel("Desigualdades y acceso a derechos II (respuestas).xlsx")

names(base_desigualdades)
table(base_desigualdades$`Encuestador/a`)
base_desigualdades$hogar<-1:901
# están cargadas las personas como variables,
# una columnas por cada variable de cada persona
# retengo las columnas que corresponde al bloque individual

personas<-base_desigualdades[,12:111]

# un por uno se reacomoda a long
persona_1<-personas[,1:10]
names(persona_1)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")

persona_2<-personas[,11:20]
names(persona_2)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")

persona_3<-personas[,21:30]
names(persona_3)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")
persona_4<-personas[,31:40]
names(persona_4)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")
persona_5<-personas[,41:50]
names(persona_5)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")
persona_6<-personas[,51:60]
names(persona_6)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")
persona_7<-personas[,61:70]
names(persona_7)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")
persona_8<-personas[,71:80]
names(persona_8)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")
persona_9<-personas[,81:90]
names(persona_9)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")
persona_10<-personas[,91:100]
names(persona_10)<-c("respondente", "sexo", "edad", "parentesco","cobertura_salud",
                    "asiste_educativo", "nivel", "grado_año", "pais_nacimiento",
                    "provincia_nacimiento")

#se unen

personas<-rbind(persona_1, persona_2, persona_3, persona_4, 
                persona_5, persona_6, persona_7, persona_8,
                persona_9, persona_10)

personas$hogar<-rep(1:901, 10)


# vuelan las intermedias
rm(persona_1, persona_2, persona_3, persona_4,
   persona_5, persona_6, persona_7, persona_8, persona_9, persona_10)

# a ver?
library(summarytools)

freq(personas$sexo, cumul = F)
freq(personas$sexo, cumul = F, report.nas = F)
freq(personas$asiste_educativo, cumul = F, report.nas = F)
freq(personas$edad)

personas$edad_num<-ifelse(personas$edad=="1 mes"|
                            personas$edad=="2 meses"|
                            personas$edad=="3 meses"|
                            personas$edad=="4 meses"|
                            personas$edad=="5 meses"|
                            personas$edad=="6 meses"|
                            personas$edad=="7 meses"|
                            personas$edad=="8 meses"|
                            personas$edad=="9 meses"
                            ,"0", ifelse(personas$edad=="Un año",1,
                          personas$edad))

personas$edad_num<-as.numeric(personas$edad_num)
freq(personas$edad_num)
hist(personas$edad_num)

freq(personas$parentesco, cumul = F)
freq(personas$cobertura_salud, cumul = F)
personas$respondente<-ifelse(personas$respondente=="Sí", "Si", 
                             personas$respondente)
freq(personas$respondente)

# Eliminamos las filas ociosas y trabajamos un porquito sobre las cadenas de texto
summary(personas)
personas[,1:10] <- lapply(personas[,1:10], factor)
personas = subset(personas, is.na(edad_num)==F)
personas$edad = NULL
table(personas$grado_año)
summary(personas)
personas$aux_nivel = as.numeric(gsub("([0-9]+).*$", "\\1", personas$nivel))
personas$años_estudios = ifelse(personas$aux_nivel==0, 0,
                                ifelse(personas$aux_nivel==1, as.numeric(gsub("([0-9]+).*$", "\\1", personas$grado_año)),
                                       ifelse(personas$aux_nivel == 2, 6,
                                              ifelse(personas$aux_nivel == 3, 6 + as.numeric(gsub("([0-9]+).*$", "\\1", personas$grado_año)),
                                                     ifelse(personas$aux_nivel==4, 12,
                                                            ifelse(personas$aux_nivel == 5, 12 + as.numeric(gsub("([0-9]+).*$", "\\1", personas$grado_año)),
                                                                   ifelse(personas$aux_nivel==6, 17, NA)))))))
personas$años_estudios = ifelse(personas$años_estudios>17, NA, personas$años_estudios)
personas$aux_nivel = NULL
hist(personas$años_estudios)

personas$pais_nacimiento = tolower(personas$pais_nacimiento)
table(personas$pais_nacimiento)
personas$pais_aux = grepl("tina", personas$pais_nacimiento, fixed = TRUE)
personas$pais_nacimiento = ifelse(personas$pais_aux==T, "argentina", personas$pais_nacimiento)
personas$pais_aux = grepl("arg", personas$pais_nacimiento, fixed = TRUE)
personas$pais_nacimiento = ifelse(personas$pais_aux==T, "argentina", personas$pais_nacimiento)
personas$pais_nacimiento = ifelse(personas$pais_nacimiento == "córdoba", "argentina", personas$pais_nacimiento)
personas$pais_aux = NULL
table(personas$pais_nacimiento)
personas$pais_nacimiento = ifelse(nchar(personas$pais_nacimiento) <= 3, NA, personas$pais_nacimiento)
personas$pais_nacimiento = as.factor(personas$pais_nacimiento)
summary(personas$pais_nacimiento)

# Pego variables específicas del hogar a la base de personas
names(base_desigualdades)
hogar = base_desigualdades
hogar = hogar[,c(4, 5, 7, 8, 9, 10, 11, 112:230)]
class(hogar$hogar)
class(personas$hogar)
library(dplyr)
personas = left_join(personas, hogar, by = "hogar")



