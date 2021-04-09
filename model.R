library(readxl)
library(tidyverse)
library(gridExtra)

study1 <- read_excel("Study1 Data Unrounded.xlsx")
study2 <- read_excel("Study2 Data Unrounded.xlsx")

## study 1
study1.mod1 <- glm(sent ~ trust, data = study1, family = "binomial")
summary(study1.mod1)

study1.mod2 <- glm(
  sent ~ trust + zAfro + attract + maturity + zfWHR + glasses + tattoos,
  data = study1, family = "binomial"
  )
summary(study1.mod2)


## study 2
study2.mod1 <- glm(sent ~ trust, data = study2, family = "binomial")
summary(study2.mod1)

study2.mod2 <- glm(
  sent ~ trust + zAfro + attract + maturity + glasses + served,
  data = study2, family = "binomial"
  )
summary(study2.mod2)

# recreating the table for study 2

coef1 <- rownames_to_column(as.data.frame(summary(study2.mod1)$coefficients), var = "Variable")
coef2 <- rownames_to_column(as.data.frame(summary(study2.mod2)$coefficients), var = "Variable")

names <-  data.frame(Variable = c("(Intercept)", "trust", "zAfro", "attract", "maturity", "glasses", "served"),
                     Predictor = c("Intercept", "Trustworthiness", "Afrocentricity", "Attractiveness", "Facial maturity", "Presence of glasses", "Time served"))

table <-
  bind_rows(coef1, coef2) %>% 
  mutate(`Odds Ratio` = round(exp(Estimate),2),
         b = round(Estimate,2),
         or.lower = exp(Estimate - 1.96*`Std. Error`),
         or.upper = exp(Estimate + 1.96*`Std. Error`),
         `OR 95% CI` = paste("[", round(or.lower,2), ", ", round(or.upper,2), "]", sep = ""),
         `Std. Error` = round(`Std. Error`, 2),
         `Pr(>|z|)` = round(`Pr(>|z|)`, 3)) %>% 
  left_join(names) %>% 
  select(Predictor, b, `Pr(>|z|)`, `Std. Error`, `Odds Ratio`, `OR 95% CI`)

rownames <- rep("", nrow(table))
rownames[table$Predictor == "Intercept"] <- c("Model 1", "Model 2")

table$`OR 95% CI`[table$Predictor == "Intercept"] <- NA

png("table2.png", height = 250, width = 500)
grid.table(table, rows = rownames)
dev.off()