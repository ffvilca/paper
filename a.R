
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
library(lme4)

# cargar base

datitos <- import("casen_filtrada.csv")


datos_finales <- datitos %>% 
  select(y1,nse,edad,e1,e6a, sexo) %>% 
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
  select(y1,log_sueldo,leer_escribir,nse_2,edad,nivel_educ, sexo) %>% 
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

chisq.test(prop.table(tablita,1) * 100 )

# no hay independencia 
# significa que existe relacion entre el nivel educacional y el nse

# como existe relacion buscaremos predecir el sueldo en base algunas variables 

modelo1 <- lm(y1 ~., datos_finales)
modelo2 <- lm(y1 ~.-log_sueldo-leer_escribir-edad, datos_finales)
modelo3 <- lm(y1 ~ nivel_educ, datos_finales) # bueno
modelo4 <- lm(log(y1) ~ nivel_educ, datos_finales) # mejor (creo)
modelo5 <- lm(y1 ~.-leer_escribir-edad, datos_finales)
modelo6 = lmer(log(y1) ~ nivel_educ + (1 | sexo), data = datos_finales) #sin interacción
modelo7 = lmer(log(y1) ~ nivel_educ*sexo + (1 | sexo), data = datos_finales) #modelo con sexo como interacción y efecto aleatorio
modelo8 = lm(log(y1) ~ nivel_educ*sexo, data = datos_finales) #solo interacción, no efecto aleatorio

#no sé si va con log o no la vdd
#efecto aleatorio: Esto significa que cada nivel de sexo tendrá su propio intercepto, captura variabilidad no explicada entre sexo

summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo6) #vale un poco pico
summary(modelo7) 
summary(modelo8)


anova(modelo1)
anova(modelo2)
anova(modelo4)
anova(modelo6)
anova(modelo7)
anova(modelo8)



# analisis modelo 4 -------------------------------------------------------

plot(modelo4, which = 1) #residuos
plot(modelo4, which = 1) #residuos estandarizados
hist(residuals(modelo4)) #densidad residuos
qqnorm(residuals(modelo4))
qqline(residuals(modelo4))

shapiro.test(residuals(modelo4)) #no corre

lmtest::bptest(modelo4) #varianza no cte, hetero


plot(modelo8, which = 1) #residuos
plot(modelo8, which = 1) #residuos estandarizados
hist(residuals(modelo8)) #densidad residuos
qqnorm(residuals(modelo8), col="pink")
qqline(residuals(modelo8), col="pink")

lmtest::bptest(modelo8) #varianza no cte, hetero

plot(modelo7, which = 1) #residuos
plot(modelo7, which = 2) #residuos estandarizados

qqnorm(residuals(modelo7), col="pink")
qqline(residuals(modelo7), col="pink")

lmtest::bptest(modelo7)
shapiro.test(residuals(modelo7))
