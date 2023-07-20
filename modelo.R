# Modelo
pacman::p_load(aod, glmtoolbox, ResourceSelection)

# Regressao Logistica Binaria
set.seed(500)
amostra <- dados[sample(1:nrow(dados),100,F),]

mod <- amostra %>%
  mutate(y = case_when(poupanca == "Sim" ~ 1,
                       poupanca == "Não" ~ 0))

mod1 = glm(y ~ idade + status + casa + setor, data=mod, family='binomial') # SM SS
summary(mod1)
mod1 = glm(y ~ idade + status + casa + setor + status*casa, data=mod,
           family='binomial') # SS CS
summary(mod1)
mod1 = glm(y ~ idade + status + casa + setor + status*setor, data=mod,
           family='binomial') # nenhum
summary(mod1)
mod1 = glm(y ~ idade + status + casa + setor + status*setor + status*casa, data=mod,
           family='binomial') # SS
summary(mod1)
mod1 = glm(y ~ idade + status + casa + setor + casa*setor, data=mod,
           family='binomial') # SM SS
summary(mod1)
mod1 = glm(y ~ idade + status + casa + setor + casa*setor + status*casa, 
           data=mod, family='binomial') # SM SS
summary(mod1)
mod1 = glm(y ~ idade + status + casa + setor + casa*setor + status*casa + status*setor, 
           data=mod, family='binomial') # SM SS
summary(mod1)

# Ausencia de regressao

wald.test(Sigma = vcov(mod1), b = coef(mod1), Terms = 2)


# Coeficientes
coef(mod1); confint(mod1)


# Odds ratios
odds = exp(coef(mod1)); odds

s = summary(mod1)$coefficients[,2]
li = exp(coef(mod1) - 1.96*s); ls = exp(coef(mod1) + 1.96*s)
round(data.frame(li,ls),3)


# Teste Hosmer e Lameshow

glmtoolbox::hltest(mod1)

hoslem.test(mod1$y, fitted(mod1))
car::vif(mod1)
