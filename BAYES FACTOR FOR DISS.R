library(BayesFactor)
library(tidyverse)

DISS_DATA <- read.csv("./2.csv")

vars <- c("SELF_SEXUALISATION_TOTAL",
          "SOBBS_TOTAL", "FNE_TOTAL",
          "ESS_TOTAL", "PASSIVE", "ACTIVE_TOTAL")
#cleaning
DISS_DATA <- DISS_DATA[complete.cases(DISS_DATA[, vars]), ]



# creating a data frame
df <- data.frame(Age=DISS_DATA$Age,
                  S_S=DISS_DATA$SELF_SEXUALISATION_TOTAL,
                  SOBBS=DISS_DATA$SOBBS_TOTAL,
                 FNE=DISS_DATA$FNE_TOTAL, ESS=DISS_DATA$ESS_TOTAL,
                 PASSIVE=DISS_DATA$PASSIVE, ACTIVE = DISS_DATA$ACTIVE_TOTAL)

print(df)

# descriptives
print("summarised dataframe")
df %>%
  summarise_at(c("Age","S_S","SOBBS","FNE","ESS","PASSIVE","ACTIVE"), list(mean=mean,sd=sd))

#set up regression model
lm <- lm(SELF_SEXUALISATION_TOTAL ~ SOBBS_TOTAL + FNE_TOTAL + ESS_TOTAL + PASSIVE + ACTIVE_TOTAL, data = DISS_DATA)
summary(lm)

#correlations
cor(DISS_DATA[,vars])

#test of assumptions for a multiple regression
shapiro.test(residuals(lm)) #normal
hist(residuals(lm))
qqnorm(residuals(lm))
plot(residuals(lm)) #homoscedasticity
length(which(cooks.distance(lm)<1))
length(which(cooks.distance(lm)>1)) #no cook's distance value below 1, suggesting no influential cases


#bayes factor 
lm_bf <- lmBF(
  SELF_SEXUALISATION_TOTAL ~ SOBBS_TOTAL + FNE_TOTAL + ESS_TOTAL + PASSIVE + ACTIVE_TOTAL,
  data = DISS_DATA
)

summary(lm_bf) #bayes factor 3 - moderate evidence for H1
