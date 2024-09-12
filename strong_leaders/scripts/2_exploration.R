#ESTA ESCALA NO DA CONVERGENCIA CON OTRAS ESCALAS

source("strong_leaders/scripts/0_packages.R")

df <- readRDS("strong_leaders/Data/intermediate_data/mepop_short.RDS")

conspiracy <- dplyr::select(df, Q192_1:Q192_5) #Conspiracy Mentality Questionnaire

#####Distribución de las respuestas

library(reshape2)

# Prepare your data
scale_items <- conspiracy
scale_items_long <- melt(scale_items, variable.name = "Item", value.name = "Score")

# Create the 2x3 histogram chart
ggplot(scale_items_long, aes(x = Score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Item, nrow = 2, ncol = 3) +  # Specify 2 rows and 3 columns
  labs(title = "Histograms of Each Scale Item", x = "Score", y = "Count") +
  theme_minimal()

###############

cor(conspiracy)

ev <- plot(eigen(cor(conspiracy))$values, type="b",
           ylab="Value", xlab="Number of Eigenvalues")
abline(h = 1, col = "red", lwd = 2, lty = 2) 

library(psych)

# Run parallel analysis
fa.parallel(conspiracy, 
            fa = "both", 
            n.iter = 100, 
            main = "Parallel Analysis Scree Plots", 
            show.legend = TRUE)

library(GPArotation)
cor_mat <- polychoric(conspiracy)$rho
efa <- fa(cor_mat, nfactors = 2, rotate = "oblimin")
print(efa)

loadings <- efa$loadings

loadings_matrix <- as.data.frame(unclass(loadings))

communalities <- as.data.frame(unclass(efa$communality))


#Cronbach

psych::alpha(conspiracy)


#CFA

library(lavaan)

modelo_1_factor <- 'factor1 =~ Q192_1+Q192_2+Q192_3+Q192_4+Q192_5'
modelo_2_factor <- 'factor1 =~ Q192_1+Q192_2
                    factor2 =~ Q192_3+Q192_4+Q192_5'

cfa_model <- cfa(modelo_1_factor, data = conspiracy, ordered = TRUE)
summary(cfa_model, fit.measures = TRUE)


cfa_model <- cfa(modelo_2_factor, data = conspiracy, ordered = TRUE)
summary(cfa_model, fit.measures = TRUE)


###### Normalidad de los datos

# Step 1: Install and load the MVN package (if not already installed)
install.packages("MVN")
library(MVN)

# Mardia’s Multivariate Normality Test
result_mardia <- mvn(data = conspiracy, mvnTest = "mardia")

# Henze-Zirkler’s Multivariate Normality Test
result_hz <- mvn(data = conspiracy, mvnTest = "hz")

# Royston’s Multivariate Normality Test
result_royston <- mvn(data = conspiracy, mvnTest = "royston")

# Step 4: View the results
print("Mardia's Test:")
print(result_mardia)

print("Henze-Zirkler's Test:")
print(result_hz)

print("Royston's Test:")
print(result_royston)


#Composite scores

conspiracy_1 <- conspiracy$Q192_1+conspiracy$Q192_2
conspiracy_2 <- conspiracy$Q192_3+conspiracy$Q192_4+conspiracy$Q192_5



#Análisis Convergente

convergencia <- cbind(conspiracy,dplyr::select(df,Q15_1:Q15_7))

cor(convergencia)

convergencia[1:5,6:12]

convergencia<- cbind(conspiracy,dplyr::select(df,Q168_1, Q168_3,Q170_8))

cor(convergencia)
