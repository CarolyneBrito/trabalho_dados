pacman::p_load(readxl, tidyverse, dplyr, GGally, ggview)
dados <- read_excel("dados_trabalho.xlsx")
cores <- c("#283D3B","#197278","#E1C7B7","#C44536","#772E25")

# Funções ----
# Definindo função que retorna frequências relativas de um vetor
percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

# Definindo função que retorna banco de dados com frequências
# relativas e absolutas de uma variável categórica
vector_frequencies <- function(vector) {
  frequency <- vector %>%
    table() %>%
    as_tibble() %>%
    mutate(
      rel = n %>%
        percent() %>%
        paste("%", sep = "")
    )
  colnames(frequency) <- c("groups", "absolute", "relative")
  return(frequency)
}

# Limpando banco e gerando amostra ----
names(dados) <- c("ID","idade","status", "casa", "setor", "poupanca")
dados <- dados %>% mutate(status = case_when(status == 1 ~ "Superior",
                                             status == 2 ~ "Médio",
                                             status == 3 ~ "Inferior"),
                          casa = case_when(casa == 1 ~ "Não ou ainda pagando financiamento", 
                                           casa == 2 ~ "Sim e quitada"),
                          setor = case_when(setor == 1 ~ "Setor A",
                                            setor == 0 ~ "Setor B"),
                          poupanca = case_when(poupanca == 0 ~ "Não",
                                               poupanca == 1 ~ "Sim"))

set.seed(123)
amostra <- dados[sample(1:nrow(dados),100,F),]
valid <- anti_join(dados, amostra)

# Descritiva Univariada ----

# Idade
ggplot(amostra, aes(x="", y=idade)) +
  geom_boxplot(fill=cores[2], width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Idade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

# Status
classes <- amostra %>%
  filter(!is.na(status)) %>%
  count(status) %>%
  mutate(freq = n %>% percent()) %>%
  mutate(freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
         label = str_c(n, " (", freq, ")") %>% str_squish())

ggplot(classes,  aes(x = as.factor(status), y = n, label = n))  +
  geom_bar(stat = "identity", fill = cores[2], width = 0.7) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, # hjust = .5,
            size = 3) +
  labs(x = "Status socioeconômico", y = "Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

# Casa
classes <- amostra %>%
  filter(!is.na(casa)) %>%
  count(casa) %>%
  mutate(freq = n %>% percent()) %>%
  mutate(freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
         label = str_c(n, " (", freq, ")") %>% str_squish())

ggplot(classes,  aes(x = as.factor(casa), y = n, label = n))  +
  geom_bar(stat = "identity", fill = cores[2], width = 0.7) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, # hjust = .5,
            size = 3) +
  labs(x = "Posse de casa própria", y = "Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 
# Setor
classes <- amostra %>%
  filter(!is.na(setor)) %>%
  count(setor) %>%
  mutate(freq = n %>% percent()) %>%
  mutate(freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
         label = str_c(n, " (", freq, ")") %>% str_squish())

ggplot(classes,  aes(x = as.factor(setor), y = n, label = n))  +
  geom_bar(stat = "identity", fill = cores[2], width = 0.7) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, # hjust = .5,
            size = 3) +
  labs(x = "Setor da cidade", y = "Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

# Poupanca
classes <- amostra %>%
  filter(!is.na(poupanca)) %>%
  count(poupanca) %>%
  mutate(freq = n %>% percent()) %>%
  mutate(freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
         label = str_c(n, " (", freq, ")") %>% str_squish())

ggplot(classes,  aes(x = as.factor(poupanca), y = n, label = n))  +
  geom_bar(stat = "identity", fill = cores[2], width = 0.7) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, # hjust = .5,
            size = 3) +
  labs(x = "Conta Poupança", y = "Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

# Descritiva Bivariada ----
# Idade - Status
ggplot(amostra) +
  aes(x = status, y = idade) +
  geom_boxplot(fill = cores[2], width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Status Socioeconômico", y = "Idade")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

# Idade - Casa 
ggplot(amostra) +
  aes(x = casa, y = idade) +
  geom_boxplot(fill = cores[2], width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Posse de casa própria", y = "Idade")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

# Idade - Setor
ggplot(amostra) +
  aes(x = setor, y = idade) +
  geom_boxplot(fill = cores[2], width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Setor da cidade", y = "Idade")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

# Idade - Poupança 
ggplot(amostra) +
  aes(x = poupanca, y = idade) +
  geom_boxplot(fill = cores[2], width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Conta Poupança", y = "Idade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

# Status - Casa 
trans_drv <- amostra %>%
  group_by(casa, status) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(x = fct_reorder(casa, freq, .desc = T),y = freq,fill = status,label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9),
           vjust = -0.5, hjust = 0.5,size = 3) +
  labs(x = "Posse de casa própria", y = "Frequência") +
  scale_fill_manual(name="Status Socioceconômico", values=c("#197278","#E1C7B7","#C44536"))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.position = "top") 

# Status - Setor 
trans_drv <- amostra %>%
  group_by(setor, status) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(x = setor,y = freq,fill = status,label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5,size = 3) +
  labs(x = "Setor da cidade", y = "Frequência") +
  scale_fill_manual(name="Status Socioceconômico", values=c("#197278","#E1C7B7","#C44536"))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.position = "top") 

# Status - Poupança 
trans_drv <- amostra %>%
  group_by(status,poupanca) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(x = status,y = freq,fill = poupanca,label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5,size = 3) +
  labs(x = "Status Socioceconômico", y = "Frequência") +
  scale_fill_manual(name="Conta Poupança", values=c("#197278","#C44536"))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.position = "top") 

# Casa - Setor
trans_drv <- amostra %>%
  group_by(setor,casa) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(x = setor,y = freq,fill = casa,label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5,size = 3) +
  labs(x = "Setor da cidade", y = "Frequência") +
  scale_fill_manual(name="Posse de casa própria", values=c("#197278","#C44536"))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.position = "top") 

# Casa - Poupança 
trans_drv <- amostra %>%
  group_by(casa,poupanca) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(x = casa,y = freq,fill = poupanca,label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5,size = 3) +
  labs(x = "Posse de casa própria", y = "Frequência") +
  scale_fill_manual(name="Conta Poupança", values=c("#197278","#C44536"))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.position = "top") 

# Setor - Poupança 
trans_drv <- amostra %>%
  group_by(setor,poupanca) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(x = setor,y = freq,fill = poupanca,label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5,size = 3) +
  labs(x = "Posse de casa própria", y = "Frequência") +
  scale_fill_manual(name="Conta Poupança", values=c("#197278","#C44536"))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.position = "top") 
