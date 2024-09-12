source("strong_leaders/scripts/0_packages.R")

options(scipen=999)

#load data

mepop_raw <-read_spss("strong_leaders/Data/raw_data/Encuesta+MEPOP_data final spss.sav")

#filter, transform and create new DF

mepop_raw$Duration__in_seconds_ <-as.numeric(mepop_raw$Duration__in_seconds_)
mepop_raw$Progress <-as.numeric(mepop_raw$Progress)
mepop_raw$Q4 <-haven::as_factor(mepop_raw$Q4)
mepop_raw$Q4 <-as.numeric(mepop_raw$Q4)


desviacion <-sd(mepop_raw$Duration__in_seconds_)
multiplicador <-desviacion*2


mepop_short <-mepop_raw%>%
  dplyr::filter( Progress == 100 & Q4 == 1 & Duration__in_seconds_ < multiplicador)%>%
  dplyr::select(Duration__in_seconds_,Q172, Q191, Q11_1:Q11_9,Q15_1:Q15_7,Q141_1:Q141_10, Q142_1:Q142_9, Q147_1, Q156_1, Q161_1:Q161_3, Q168_1, Q168_3,Q170_8, Q144_1:Q144_9, Q192_1:Q192_5,Q196, Q173, Q174, NSE)%>%
  na.omit(TRUE)
                

#Primera disacusión: cómo vamos a limpiar la base de datos en términos de cuánto se demoraron en responder?

mean(mepop_short$Duration__in_seconds_)
sd(mepop_short$Duration__in_seconds_)
range(mepop_short$Duration__in_seconds_)


# mepop raw: mean=5260,22 (87,6 min); sd=23363,94 (389min); range: 3 - 493162 (3 segundos a ... 8219 segundos! 136,98 horas, 5,7 días)
# mepop_short con 3 desviaciones estandar: mean=2856,71; sd=4889,16, range= 260 - 68619
# mepop short con 2 desviaciones estandar: mean=2685.425; sd=3781.382 ; range= 260 44583
# mepop short con 1 desviación estandar: mean= 2377.78; sd= 2154.222 ; range= 260 21593  


#create binary response to strong leader ("good" or "bad")

lider_fuerte <-table(mepop_short$Q161_1)
round(prop.table(lider_fuerte)*100,2)


mepop_short$lider_fuerte <-ifelse(mepop_short$Q161_1 == 1 | mepop_short$Q161_1 == 2,1,0)

# transform variables

mepop_short <-mepop_short%>%
  dplyr::mutate(echo = Q144_1 + Q144_2 + Q144_3 + Q144_4 + Q144_5 +
                  Q144_6 + Q144_7 + Q144_8 + Q144_9)%>%
  dplyr::mutate(std_echo = round(((echo - 9)/(45 - 9)),3))
  #dplyr::mutate(ideologia = case_when(
  #  Q156_1 == c(0, 1, 2, 3, 4) ~ 1,
  #  Q156_1 == 1 ~ 1,
  #  Q156_1 == 2 ~ 1,
  #  Q156_1 ==
  #  Q156_1 == 5 ~ 2,
  #  Q156_1 == c(6, 7, 8, 9, 10) ~ 3))


### CREATE BASE VARIABLES

#mepop_short$Q173 <-relevel(factor(mepop_short$Q173), ref = 10)




saveRDS(mepop_short, file = "strong_leaders/Data/intermediate_data/mepop_short.RDS")


reg1 <-glm(lider_fuerte ~ Q174 + Q173 + Q191 + Q156_1 + Q172 + Q156_1*echo, family = binomial(link = "logit"), data = mepop_short)

summary(reg1)
predict(reg1)

############################ Varianzas de variables de interés#######################

table(mepop_raw$Q156_1) #Identificación política
table(mepop_raw$Q173) #Educación
table(mepop_raw$Q195) #Genero
table(mepop_raw$Q183) #Religión
table(mepop_raw$Q171_2) #Aborto
table(mepop_raw$Q174) #Ingreso

#Estas 4 pueden ser una "escala de pesimismo"

table(mepop_raw$Q164) #SITUACIÓN ECONÓMICA ACTUAL DEL PAÍS
table(mepop_raw$Q165) #SITUACIÓN ECONÓMICA DEL PAÍS EN 12 MESES
table(mepop_raw$Q166) #TU SITUACIÓN ACTUAL
table(mepop_raw$Q167) #TU SITUACIÓN EN 12 MESES

#Conspiración

table(mepop_raw$Q192_4) #Sociedades secretas en el mundo
table(mepop_raw$Q192_5) #Sociedades secretas en política

#Lider fuerte

table(mepop_raw$Q161_1)

#Partidos políticos

table(mepop_raw$Q157)

