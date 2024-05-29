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
  select(y1,edad,sexo,e1,region,e6a,e8) %>% 
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

table(b$edad)
table(b$sexo)
table(b$e1)
table(b$region)
table(b$e6a)
table(b$e8)

b <- b %>% 
  filter(e8 != -99 & e8 != -88) %>%
  select(-e8)

modelo <- lm(y1 ~ .,b)

summary(modelo)
