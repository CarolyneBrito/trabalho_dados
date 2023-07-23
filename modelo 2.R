# Pacotes ----
pacman::p_load(readxl, tidyverse, dplyr,gridExtra, ggview, MASS, 
               stats, car, aod, glmtoolbox, ResourceSelection, leaps,
               bestglm)

# Selecionando amostra ---

set.seed(2023.1)

amostra <- dados[sample(1:nrow(dados),100,F),]

# Arrumando o banco da amostra ----

mod <- amostra %>%
  mutate(y = case_when(poupanca == "Sim" ~ 1,
                       poupanca == "Não" ~ 0),
         status_superior = case_when(status == "Superior" ~ 1,
                                     status != "Superior" ~ 0),
         status_medio = case_when(status == "Médio" ~ 1,
                                  status != "Médio" ~ 0),
         casa_sim = case_when(casa == "Sim e quitada" ~ 1,
                              casa != "Sim e quitada" ~ 0),
         setor_b = case_when(setor == "Setor B" ~ 1,
                             status != "Setor B" ~ 0))


ids <- amostra$ID
teste <- dados %>%
  filter(!ID %in% ids) %>%
  mutate(y = case_when(poupanca == "Sim" ~ 1,
                       poupanca == "Não" ~ 0),
         status_superior = case_when(status == "Superior" ~ 1,
                                     status != "Superior" ~ 0),
         status_medio = case_when(status == "Médio" ~ 1,
                                  status != "Médio" ~ 0),
         casa_sim = case_when(casa == "Sim e quitada" ~ 1,
                              casa != "Sim e quitada" ~ 0),
         setor_b = case_when(setor == "Setor B" ~ 1,
                             status != "Setor B" ~ 0))

# Modelo completo ---
mod_completo = glm(y ~ idade + status_superior + status_medio + casa_sim + setor_b, data=mod, family='binomial') 
summary(mod_completo)

mod_nulo <- glm(y ~ 1, family=binomial, data=mod)
summary(mod_nulo)

# Seleção de variáveis ----

step(mod_nulo, direction="forward", scope=formula(mod_completo))
step(mod_completo, direction="backward")
step(mod_completo, direction="both")


# todos os métodos indicam que o melhor modelo é: y ~ idade + status_superior + status_medio + casa_sim

#__ Modelo:  y ~ idade + status_superior + casa_sim ----

mod_ajs <- glm(y ~ idade + status_superior + casa_sim, data = mod, family = binomial)
mod_rs1 <- glm(y ~ idade + status_superior + status_medio + casa_sim, data = mod, family = binomial)
summary(mod_rs1)
summary(mod_ajs)


#__ Análise resíduos ----
# Wald

wald.test(Sigma = vcov(mod_ajs), b = coef(mod_ajs), Terms = 2)
wald.test(Sigma = vcov(mod_rs1), b = coef(mod_rs1), Terms = 2)
wald.test(Sigma = vcov(mod_completo), b = coef(mod_completo), Terms = 2)

# Verossimilhança

lrtest(mod_ajs, mod_nulo)
lrtest(mod_completo, mod_nulo)
lrtest(mod_rs1, mod_nulo)
lrtest(mod_completo, mod_ajs)

lrtest(mod_completo, mod_rs1)
lrtest(mod_completo, mod_ajs)
lrtest(mod_rs1, mod_ajs)

# Lemeshow
hoslem.test(mod_completo$y, fitted(mod_completo))
hoslem.test(mod_ajs$y, fitted(mod_ajs))
hoslem.test(mod_rs1$y, fitted(mod_rs1))
# VIF

mean(vif(mod_completo))
mean(vif(mod_rs1))
mean(vif(mod_ajs))

# Bic

BIC(mod_completo)
BIC(mod_rs1)
BIC(mod_ajs)

# AIC

AIC(mod_completo)
AIC(mod_rs1)
AIC(mod_ajs)


# betas ---
coef(mod_ajs); confint(mod_ajs)

# odds ----
odds = exp(coef(mod_ajs)); odds

s = summary(mod_ajs)$coefficients[,2]
li = exp(coef(mod_ajs) - 1.96*s); ls = exp(coef(mod_ajs) + 1.96*s)
round(data.frame(li,ls),3)

# Validação do modelo ----

library(caret)

aModel <- train(as.factor(y) ~ idade + status_superior + casa_sim, 
                data = mod, method="glm", family = "binomial")

predicoes <- predict(aModel, newdata = teste )

confusionMatrix(data=predicoes, as.factor(teste$y))