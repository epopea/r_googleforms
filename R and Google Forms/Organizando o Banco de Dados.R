# IMPORTANDO E ANALISANDO DADOS DO GOOGLEFORM #
# Tutores: Gilvan Guedes (EPOPEA & Cedeplar); Jéferson Andrade (EPOPEA & ICEx)
# Emails: grguedes@epopea.com.br; jeferson@epopea.com.br
# Local: Zoom Webconference Room
# Data: 09/06/2020
# Horário: 14:00
# Duração: 90 minutos
#We have to change X1(Date), X7(Date), X8(Factor), X9(Very complicated Factor), X11(Factor)
#--------------------------------#
# INICIANDO A SESSÃO DE TRABALHO #
#--------------------------------#

# Limpando todos os objetos atualmente ativos no Ambiente (Environment)
rm(list=ls(all=T))

# Carregando os pacotes necessarios para a aula

if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
if(!require(magrittr)){install.packages("magrittr");require(magrittr)}
if(!require(likert)){install.packages("likert");require(likert)}
if(!require(skimr)){install.packages("skimr");require(skimr)}

#------------------------------------------------#
# PRIMEIROS PASSOS AO IMPORTAR UM BANCO DE DADOS #
#------------------------------------------------#

# Definindo a pasta de trabalho (voce deve mudar para o que quiser
# ou deixar no local onde definiu quando criou o projeto)

setwd("/Library/Developer/CommandLineTools/usr/share/git-core/perl/Git/workshop")

# Carregando os dados salvos em formato RData
getwd() # verificando se o R ja esta na pasta onde esta o banco em formato RData

load("dados.workshop.RData")

# Analisando a estrutura do banco de dados

# 1) Visualizar:
View(dados)

# 2) Descrever a estrutura dos dados:
str(dados)
attach(dados) # uso esse comando para nao precisar toda hora digitar o nome do objeto

# Transformando variaveis com classe incorreta #
dados %<>% as.data.frame() %>% mutate(`X1` = as.Date(as.POSIXct(`X1`)),
                                      `X7` = as.Date(as.POSIXct(`X7`)),
                                      `X8` = as.factor(`X8`),
                                      `X9` = as.factor(`X9`),
                                      `X10` = factor(X10, ordered = TRUE),
                                      `X11` = as.factor(`X11`),
                                      `X12` = factor(X12, ordered = TRUE),
                                      `X13` = factor(X13, ordered = TRUE))

attach(dados) # uso esse comando para nao precisar toda hora digitar o nome do objeto

# Criando uma nova variável: Idade atual
dados$Idade <- year(X1) - year(X7)

# Criando uma nova variável: Dummy para Idade acima da idade mediana
dados$Dummy_Idade <- case_when(
  Idade > median(Idade) ~ 1,
  TRUE ~ 0
)
dados$Dummy_Idade %<>% as.factor()

# Criando uma nova variável: quartil de Idade
dados$Quartil_Idade <- cut(Idade, breaks = quantile(Idade, probs = seq(0, 1, 0.25)), include.lowest = TRUE)

# Criando uma nova variável: Grupo etário quinquenal
dados$grupo_etario <- cut(Idade, breaks = seq(0, 80, 5), include.lowest = TRUE, right = FALSE)

# Criando uma nova variável: Coorte
dados$coorte <- as.factor(case_when(
  X6 < 2010 ~ "<2010",
  between(X6, 2010, 2014) ~ "2010-2014",
  between(X6, 2015, 2019) ~ "2015-2019",
  TRUE ~ "2020"
  ))

# Excluindo uma variavel do banco de dados

dados$temp = NA # criei uma variavel temporaria
dados=subset(dados, select = -temp) #agora eu excluo a variavel que criei

# Forma alternativa de fazer
dados$temp <- NA
dados %<>% select(-temp)


# Como criamos duas novas variaveis no banco de dados, vamos arrumar o dicionario
Age = c("Idade","Qual a sua idade atual?")
Median_Age = c("Dummy_Idade", "Sua idade é maior que a mediana de todas as idades?")
quartil = c("Quartil_Idade", "Em qual quartil sua idade pertence?")
Age_group <- c("grupo_etario", "Grupo etário quinquenal")
Cohorts <- c("coorte", "Coorte de entrada na PG")
dicionario=rbind(dicionario,Age, Median_Age,
                 quartil, Age_group, Cohorts)
dicionario=as.data.frame(dicionario)

save(dicionario,file="dicionario.RData")
save(dados,file="dados.RData")

#------------------------------------#
# Lembrando as classes das variaveis #
#------------------------------------#

# Qualitativas:
# - nominal (sexo, cor dos olhos): class(var) -> "factor"
# - ordinal (classe social, grau de instrucao): class(var) -> "ordered" "factor"

# Quantitativas:
# - continua (peso, altura, salario, idade): class(var) -> "numeric"
# - discreta (numero de filhos, numero de carros): class(var) -> "integer"

# Descritiva do banco de dados
skimr::skim(dados)

# Descritiva de variaveis selecionadas #

# Descrevendo variaveis qualitativas nominais

# Tabela de Frequencia Absoluta de Cor/Etnia
table(X11) # tabulacao simples
table(X11, useNA="no") #useNA tem 3 opcoes: no, ifany, always
table(X11, useNA="ifany")
table(X11, useNA="always")
addmargins(table(X11)) #acrescenta o total ao final

# Tabela de Frequencia Relativa de Cor/Etnia
x=table(X11)
prop.table(x) # esse comando calcula as proporcoes relativas
addmargins(prop.table(x))

# Agora vamos combinar tudo, construindo uma tabela completa
tabs = function(x){
  z = cbind(Fabs=addmargins(table(x)),
            Frel=round(addmargins(prop.table(table(x))),digits=2),
            FAcum=cumsum(round(addmargins(prop.table(table(x))),digits=2)))
  z[nrow(z),ncol(z)] <- NA
  return(z)
}
tabs(dados$Dummy_Idade)


# Analisando uma variavel qualitativa ordinal

dicionario %>% filter(`Codigo das variaveis` == "X10") %>% select(2)
class(dados$X10)
dados$X10

# Probabilidade de area onde mora ser atingido por desastre
likert_X10 <- c("Discordo completamente",
                "Discordo",
                "Indiferente",
                "Concordo",
                "Concordo completamente")

dados <- dados %>% 
  mutate(X10 = factor(dados$X10,
                        levels = likert_X10)) 

colsummary <- likert(as.data.frame(dados[,10]))
summary(colsummary)
plot(colsummary)

# Criando os rotulos das categorias das variaveis com escala Likert para varias variaveis

likert_1 <- c("Muito pouco provável",
              "Pouco provável",
              "Igualmente provável",
              "Provável",
              "Muito provável")

likert_2 <- c("Não projete",
              "Pouco efetivo",
              "Mais ou menos efetivo",
              "Muito efetivo",
              "Totalmente efetivo")

likert_3 <- c("Nenhuma outra",
              "Pouquíssimas",
              "Poucas",
              "Algumas",
              "Muitas")

likert_4 <- c("Sem custo",
              "Muito barato",
              "Preço razoável",
              "Caro",
              "Caríssimo")

likert_5 <- c("Nenhum",
              "Pouquíssimo tempo",
              "Tempo razoável",
              "Muito tempo",
              "Muitíssimo tempo")

likert_6 <- c("Nenhum",
              "Pouquíssimo esforço",
              "Esforço razoável",
              "Muito esforço",
              "Esforço excessivo")

likert_7 <- c("Facílimo",
              "Fácil",
              "Nem fácil nem difícil",
              "Difícil",
              "Dificílimo")

likert_8 <- c("Nenhuma",
              "Pouca",
              "Nem pouca nem muita",
              "Muita",
              "Toda a ajuda possível")

# Transformando as questoes Likert em fatores (variaveis X33 ate X37)

temp = dados[,c(33:37)]

temp <- temp %>% 
  mutate(`X33` = factor(`X33`,
                        ordered = TRUE,
                        levels = likert_1),
         `X34` = factor(`X34`,
                        ordered = TRUE,                        
                        levels = likert_1),
         `X35` = factor(`X35`,
                        ordered = TRUE,
                        levels = likert_1),
         `X36` = factor(`X36`,
                        ordered = TRUE,
                        levels = likert_1),
         `X37` = factor(`X37`,
                        ordered = TRUE,
                        levels = likert_1))   

# Conferindo as classes das novas variaveis (todas tem que ser fator)
str(temp)
temp=as.data.frame(temp)
names(temp)
colnames(temp) = c("Probabilidade de desastre",
                   "Dano ao patrimômino público",
                   "Dano a sua casa e pertences",
                   "Ameaça a sua vida e família",
                   "Vida afetada por muito tempo")
colsummary <- likert(temp)
summary(colsummary) # Descrevendo os resultados
plot(colsummary) # Criando um grafico de barra dos resultados

# Tabelas Cruzadas de duas variáveis qualitativas

# Jeito mais simples
x = table(dados$cor, dados$sexo) # valores absolutos

prop.table(x) # valores relativos

# Usando a funcao CrossTable (do pacote "gmodels")
if(!require(gmodels)){install.packages("gmodels");library(gmodels)}

# Tabela com proporcoes em relacao ao total da amostra
CrossTable(dados$cor, dados$sexo, # as duas variaveis de analise
           prop.t=TRUE, # se a soma é no total da amostra
           prop.chisq=FALSE, # para nao mostrar teste de hipotese
           prop.r=FALSE, # se total da soma é na linha
           prop.c=FALSE, # se total da soma é na coluna
           format=c("SPSS"), # tipo de formato de apresentacao
           digits=2) # numero de casas decimais para percentual

# Tabela com proporcoes em relacao ao total da linha
CrossTable(dados$cor, dados$sexo, # as duas variaveis de analise
           prop.t=FALSE, # se a soma é no total da amostra
           prop.chisq=FALSE, # para nao mostrar teste de hipotese
           prop.r=TRUE, # se total da soma é na linha
           prop.c=FALSE, # se total da soma é na coluna
           format=c("SPSS"), # tipo de formato de apresentacao
           digits=2) # numero de casas decimais para percentual

# Tabela com proporcoes em relacao ao total da coluna
CrossTable(dados$cor, dados$sexo, # as duas variaveis de analise
           prop.t=FALSE, # se a soma é no total da amostra
           prop.chisq=FALSE, # para nao mostrar teste de hipotese
           prop.r=FALSE, # se total da soma é na linha
           prop.c=TRUE, # se total da soma é na coluna
           format=c("SPSS"), # tipo de formato de apresentacao
           digits=2) # numero de casas decimais para percentual

# Salvando novamente os dados depois de tudo que fizemos
save.image(file="dados.desastres.RData")

