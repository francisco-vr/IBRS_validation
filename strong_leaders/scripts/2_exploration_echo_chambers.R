source("strong_leaders/scripts/0_packages.R")

df <- readRDS("strong_leaders/Data/intermediate_data/mepop_short.RDS")

#conspiracy <- dplyr::select(df, Q144_1:Q144_9) #Echo Chamber Scale
conspiracy <- dplyr::select(df, Q144_1:Q144_6) #Echo Chamber Scale Short

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
efa <- fa(cor_mat, nfactors = 3, rotate = "oblimin")
print(efa)

loadings <- efa$loadings

loadings_matrix <- as.data.frame(unclass(loadings))

communalities <- as.data.frame(unclass(efa$communality))


#Cronbach

psych::alpha(conspiracy)


#CFA

library(lavaan)

modelo_3_factor_full_scale <- 'factor1 =~ Q144_1+Q144_2+Q144_7
                    factor2 =~ Q144_5+Q144_6+Q144_9
                    factor3 =~ Q144_3+Q144_4+Q144_8'

modelo_3_factor_short_scale <- 'factor1 =~ Q144_1+Q144_2
                    factor2 =~ Q144_5+Q144_6
                    factor3 =~ Q144_3+Q144_4'

cfa_model <- cfa(modelo_3_factor_short_scale, data = conspiracy, ordered = TRUE)
summary(cfa_model, fit.measures = TRUE)




