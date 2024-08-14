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
multiplicador <-desviacion*3


mepop_short <-mepop_raw%>%
  dplyr::filter( Progress == 100 & Q4 == 1 & Duration__in_seconds_ < multiplicador)%>%
  dplyr::select(Q172, Q191, Q11_1:Q11_9,Q141_1:Q141_10, Q142_1:Q142_9, Q147_1, Q156_1, Q161_1:Q161_3, Q144_1:Q144_9, Q196, Q173, Q174, NSE)%>%
  na.omit(TRUE)
                

#Primera disacusión: cómo vamos a limpiar la base de datos en términos de cuánto se demoraron en responder


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
