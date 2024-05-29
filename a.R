
data = rio::import(file.choose())

library(tidyverse)
data_filt = data %>% select(region, area, nse, hogar, p9, p10,
                            edad, sexo, ecivil, e1, e6a, e7, e8, e12a,
                            e12b, e12c, e12d, e12e, e13b_1, e13b_2,
                            e13b_3, e13b_4, e13b_5, e13b_6, e13b_7,
                            e13b_8, e13b_9, e13b_10, e13b_11, e14a, e14b,
                            e14c, e14d, e14e,e16, e18, o10, y1)

#nse
#p9 cuántos viven en la casa
#p10 todos comparten presupuesto?
#e1 saben leer/escribir?
#e3 asiste a establecimiento de educación
#e6a nivel educacional más alto al que asistió?
#e7 nombre de carrera o programa
#e8 tipo de institución
#e12a a e12e preguntas si recibieron ayuda en el establecimiento educacional
#e13b_1 a e13b_11 son si recibió beca o no
#o10 horas trabajadas semanalmente 
#y1. Mes pasado. Sueldo o salario líquido en su trabajo principal


library(rio)
library(dplyr)

# cargar base

datitos <- import("casen_filtrada.csv")

str(datitos)
View(datitos)

a <- datitos %>% 
  select(y1,e6a) %>% 
  na.omit()

plot(a$e6a,a$y1)

b <- datitos %>% 
  select(y1,edad,sexo,e1,region,e6a,e8, nse) %>% 
  na.omit()

b$e1 <- ifelse(b$e1 != 1, 0, 1)
b$e6a <- ifelse(b$e6a == 7,"basica",
                ifelse(b$e6a == 9 | b$e6a == 11,"media",
                       ifelse(b$e6a == 12 | b$e6a == 13,"tec-uni",
                              ifelse(b$e6a == 14 | b$e6a == 15,"post",0))))

b$region <- ifelse(b$region == 13,"RM","otra")

GGally::ggcorr(b)

b$sexo <- as.factor(b$sexo)
b$e1 <- as.factor(b$e1)
b$region <- as.factor(b$region)
b$e6a <- as.factor(b$e6a)
b$e8 <- as.factor(b$e8)

table(b$nse, b$e6a)
table(datitos$e6a, datitos$nse)
prop.table(table(datitos$e6a, datitos$nse), 2)

c = datitos %>% select(y1,nse, e6a) %>% na.omit() 

c$e6a = as.factor(c$e6a)
c$nse = as.factor(c$nse)


summary(lm(y1 ~ e6a, c))

table(b$nse)
table(b$edad)
table(b$sexo)
table(b$e1)
table(b$region)
table(b$e6a)
table(b$e8)

b <- b %>%
  select(-e1) 

modelo <- lm(y1 ~ .,b) #no

summary(modelo)

lm(nse~e6a, b) #no

# Limpiando la base

datos_finales <- datitos %>% 
  select(y1,nse,edad,e1,e6a) %>% 
  mutate(leer_escribir = ifelse(e1 != 1, 0, 1)) %>% 
  filter(y1 != 0) %>% 
  mutate(nse_2 = ifelse(nse == 1,"bajo",
                        ifelse(nse == 2,"medio",
                               ifelse(nse == 3,"alto",0)))) %>% 
  filter(nse_2 != 0) %>% 
  mutate(nivel_educ = ifelse(e6a == 6 | e6a == 7,"basica",
                             ifelse(e6a == 8 | e6a == 9 | 
                                      e6a == 10 | e6a == 11,"media",
                                    ifelse(e6a == 12 | e6a == 13,"tec-uni",
                             ifelse(e6a == 14 | e6a == 15,"post",0))))) %>%
  filter(nivel_educ != 0) %>% 
  mutate(log_sueldo = log(y1)) %>% 
  select(y1,log_sueldo,leer_escribir,nse_2,edad,nivel_educ) %>% 
  na.omit() 

datos_finales$nse_2 <- factor(datos_finales$nse_2, 
                              levels = c("bajo","medio","alto"))
datos_finales$nivel_educ <- factor(datos_finales$nivel_educ, 
                              levels = c("basica","media","tec-uni","post"))
datos_finales$leer_escribir<- factor(datos_finales$leer_escribir,
                                     levels = c(0,1))

tablita <- table(datos_finales$nivel_educ,datos_finales$nse_2)

prop.table(tablita,1) * 100 
prop.table(tablita,2) * 100

chisq.test(tablita) 
chisq.test(prop.table(tablita,1) * 100 )

# no hay independencia 
# significa que existe relacion entre el nivel educacional y el nse

# como existe relacion buscaremos predecir el sueldo en base algunas variables 

modelo <- lm(y1 ~., datos_finales)

modelo <- lm(y1 ~.-log_sueldo-leer_escribir-edad, datos_finales)

modelo <- lm(y1 ~ nivel_educ, datos_finales) # bueno
modelo <- lm(log(y1) ~ nivel_educ, datos_finales) # mejor (creo)

summary(modelo)
anova(modelo)

plot(modelo)

