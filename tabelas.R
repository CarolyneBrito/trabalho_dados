# Tabelas:
pacman::p_load(xtable)
names(amostra)

# Amostra
set.seed(123456789)
amostra <- dados[sample(1:nrow(dados),100,F),]

# Quadros de medida resumo ----
# idade
summary(amostra$idade)
sd(amostra$idade)

# Idade + Status
summary(amostra$idade[amostra$status == "Inferior"])
sd(amostra$idade[amostra$status == "Inferior"])
summary(amostra$idade[amostra$status == "Médio"])
sd(amostra$idade[amostra$status == "Médio"])
summary(amostra$idade[amostra$status == "Superior"])
sd(amostra$idade[amostra$status == "Superior"])

# Idade + Casa
summary(amostra$idade[amostra$casa == "Sim e quitada"])
sd(amostra$idade[amostra$casa == "Sim e quitada"])
summary(amostra$idade[amostra$casa == "Não ou não quitada"])
sd(amostra$idade[amostra$casa == "Não ou não quitada"])

# Idade + setor
summary(amostra$idade[amostra$setor == "Setor A"])
sd(amostra$idade[amostra$setor == "Setor A"])
summary(amostra$idade[amostra$setor == "Setor B"])
sd(amostra$idade[amostra$setor == "Setor B"])

# Idade + poupanca
summary(amostra$idade[amostra$poupanca == "Sim"])
sd(amostra$idade[amostra$poupanca == "Sim"])
summary(amostra$idade[amostra$poupanca == "Não"])
sd(amostra$idade[amostra$poupanca == "Não"])

# Tabelas bivariadas ----
table(amostra$status,amostra$poupanca)
table(amostra$casa,amostra$poupanca)
table(amostra$setor,amostra$poupanca)
table(amostra$status,amostra$setor)
table(amostra$status,amostra$casa)
table(amostra$setor,amostra$casa)










