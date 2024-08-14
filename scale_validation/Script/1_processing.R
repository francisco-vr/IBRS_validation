## filter variables and create IRBS-9 df

source("scale_validation/Script/0_packages.R")


mepop_raw <-read_spss("scale_validation/Data/Encuesta+MEPOP_data final spss.sav")

# filter just finiesh surveys and select IRBS-9 questions

desviacion <-sd(mepop_raw$Duration__in_seconds_)
multiplicador <-desviacion*3

IBRS_9 <-mepop_raw%>%
  dplyr::filter(mepop_raw$Finished == 1 & Duration__in_seconds_ < multiplicador)%>%
  dplyr::select(Q144_1, Q144_2,Q144_3,Q144_4,Q144_5,Q144_6,Q144_7,Q144_8,Q144_9)%>%
  na.omit(TRUE)

IBRS_6 <-mepop_raw%>%
  dplyr::filter(mepop_raw$Finished == 1 & Duration__in_seconds_ < multiplicador)%>%
  dplyr::select(Q144_1, Q144_2,Q144_3,Q144_4,Q144_5,Q144_6)%>%
  na.omit(TRUE)


#mepop_intermediate$EchSum <- mepop_intermediate$Q144_1 + mepop_intermediate$Q144_2 + mepop_intermediate$Q144_3 +
#  mepop_intermediate$Q144_4 + mepop_intermediate$Q144_5 + mepop_intermediate$Q144_6 +
#  mepop_intermediate$Q144_7 + mepop_intermediate$Q144_8 + mepop_intermediate$Q144_9


##first look

#ggplot(data = IBRS_9, mapping = aes(x = EchSum)) +
#  geom_histogram()


#range(mepop_intermediate$EchSum)

#table(mepop_intermediate$EchSum)

#mean(mepop_intermediate$EchSum)
#sd(mepop_intermediate$EchSum)

## Kaiser-meyes olkin test

KMO(IBRS_9)

## crombach alpha

alfa <-psych::alpha(IBRS_9)

kable(alfa[["alpha.drop"]])


## Barlett test

cortest.bartlett(IBRS_9)

## create two random samples for exploratory factorial analysis and confoormatory factorial a nalysis

set.seed(42)
n_rows <- nrow(IBRS_9)
sample_size <- round(0.5 * n_rows)

IBRS_9_EFA <- IBRS_9[sample(n_rows, size = sample_size, replace = FALSE), ]
IBRS_9_CFA <- setdiff(IBRS_9, IBRS_9_EFA)

## correlation matrix

round(cor(IBRS_9_EFA[,1:9]),2)

## number of factors
FACTORABILITY(IBRS_9_EFA)

RAWPAR(IBRS_9_EFA, factormodel = "PCA", Ndatasets = 10000, percentile = 95)

RAWPAR(IBRS_9_EFA, factormodel = "PAF", Ndatasets = 10000, percentile = 95)

MAP(IBRS_9_EFA)

SCREE_PLOT(IBRS_9_EFA) # 2 o 3 facctores?

SMT(IBRS_9_EFA)

EMPKC(IBRS_9_EFA)

EFA(IBRS_9_EFA, corkind="pearson", Nfactors = 3, iterpaf = 50, rotation = "PROMAX")

## seems to be three factors

## CONFIRMATORY FACTORIAL ANALYSIS


