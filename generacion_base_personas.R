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

freq(personas$parentesco, cumul = F)
freq(personas$cobertura_salud, cumul = F)
freq(personas$respondente)
personas$respondente<-ifelse(personas$respondente=="Sí", "Si", 
                             personas$respondente)
