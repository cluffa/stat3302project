library(readxl)

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
  sent ~ trust + zAfro + attract + maturity + glasses,
  data = study2, family = "binomial"
  )
summary(study2.mod2)