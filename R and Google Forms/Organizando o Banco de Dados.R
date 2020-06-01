# IMPORTANDO E ANALISANDO DADOS DO GOOGLEFORM #
# Tutores: Gilvan Guedes (EPOPEA & Cedeplar); Jéferson Andrade (EPOPEA & ICEx)
# Emails: grguedes@epopea.com.br; jeferson@epopea.com.br
# Local: Zoom Webconference Room
# Data: 09/06/2020
# Horário: 14:00
# Duração: 90 minutos

#--------------------------------#
# INICIANDO A SESSÃO DE TRABALHO #
#--------------------------------#

# Limpando todos os objetos atualmente ativos no Ambiente (Environment)
rm(list=ls(all=T))

# Carregando os pacotes necessarios para a aula

if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
if(!require(magrittr)){install.packages("magrittr");require(magrittr)}
if(!require(likert)){install.packages("likert");require(likert)}

#------------------------------------------------#
# PRIMEIROS PASSOS AO IMPORTAR UM BANCO DE DADOS #
#------------------------------------------------#

# Definindo a pasta de trabalho

setwd("/Users/")

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

# Idade (de texto para inteiro)
X3
dados$X3 = as.integer(dados$X3) # transformando idade em inteiro
dados$X3

# Cor/etnia (de texto para fator)
X4
table(dados$X4)

# Criando um vetor texto para os rotulos (labels) das categorias de cor/etnia
etnia = c("Branca","Preta","Amarela","Parda","Indígena")

# Forma 1: usando funcao mutate do pacote dplyr
class(dados$X4)
dados <- dados %>% 
  mutate(`X4` = factor(`X4`,
                       levels = etnia))  
class(dados$X4) #checando se deu certo
table(dados$X4)

# Forma 2: usando funcao ifelse (criando outra variavel)
dados$cor = ifelse(dados$X4=="Branca",1,
                   ifelse(dados$X4=="Preta",2,
                          ifelse(dados$X4=="Amarela",3,
                                 ifelse(dados$X4=="Parda",4,5))))

table(dados$cor) # acabamos de criar uma coluna nova no banco
class(dados$cor) # a nova variavel e numerica, vamos passar para fator

dados$cor = factor(dados$cor,
                   levels=c(1,2,3,4,5),
                   labels=c("Branca","Preta","Amarela","Parda","Indigena"))
class(dados$cor) #checando se deu certo

table(dados$cor)

# Sexo ao nascer

class(dados$X5) # verificando a classe da variavel
X5
table(X5) # vendo a quantidade de casos em cada categoria

dados$sexo = ifelse(dados$X5=="Feminino",1,0)
dados <- dados %>%
  mutate(`sexo` = factor(`sexo`,
                         levels=c(0,1),
                         labels=c("Masculino","Feminino")))
class(dados$sexo) # conferindo se deu certo

# Vamos criar uma variavel para tempo de residencia em BH

dados$tresid = NA
dados$tresid=ifelse(is.na(dados$X8),dados$X3,dados$tresid)
View(dados[,c("X3","X6","X7","X8","tresid")]) # verificando se deu certo
# o resto temos que fazer a mao
dados[c(1,36),"tresid"] = 7 # adicionei o valor 7 para as linhas 1 e 36 na coluna tresid
dados[c(3,9),"tresid"] = 3
dados[4,"tresid"] = 55
dados[c(6,13,48),"tresid"] = 12
dados[8,"tresid"] = 9
dados[10,"tresid"] = 2
dados[11,"tresid"] = 4
dados[c(17,20,24,27,32,43,45,51),"tresid"] = 0
dados[c(25,29),"tresid"] = 6
dados[c(28,37),"tresid"] = 5
dados[34,"tresid"] = 18
dados[39,"tresid"] = 20
dados[42,"tresid"] = 11
dados[53,"tresid"] = 10
View(dados[,c("X3","X6","X7","X8","tresid")]) # verificando se deu certo

# Excluindo uma variavel do banco de dados

dados$temp = NA # criei uma variavel temporaria
dados=subset(dados, select = -temp) #agora eu excluo a variavel que criei
# Forma alternativa de fazer
#dados$temp = NA
#dados=dados[-122]

# Como criamos duas novas variaveis no banco de dados, vamos arrumar o dicionario
cor = c("cor","Qual das opções abaixo melhor descreve como se vê em termos de etnia/cor?")
tresid = c("tresid", "Anos de residencia em Belo Horizonte")
sexo = c("sexo", "Sexo ao nascer")
dicionario=rbind(dicionario,cor)
dicionario=rbind(dicionario,tresid)
dicionario=rbind(dicionario,sexo)
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
describe(dados)

# Descritiva de variaveis selecionadas #

# Descrevendo variaveis qualitativas nominais

# Tabela de Frequencia Absoluta de Cor/Etnia
table(dados$cor) # tabulacao simples
table(dados$cor, useNA="no") #useNA tem 3 opcoes: no, ifany, always
table(dados$cor, useNA="always")
addmargins(table(dados$cor)) #acrescenta o total ao final

# Tabela de Frequencia Relativa de Cor/Etnia
x=table(dados$cor)
prop.table(x) # esse comando calcula as proporcoes relativas
addmargins(prop.table(x))

# Agora vamos combinar tudo, construindo uma tabela completa
#tabs = function(x){
#  z = cbind(Fabs=addmargins(table(x)),
#            Frel=round(addmargins(prop.table(table(x))),digits=2),
#            FAcum=cumsum(round(addmargins(prop.table(table(x))),digits=2)))
#  z[nrow(z),ncol(z)] <- NA
#  return(z)
#}

z = cbind(Fabs=addmargins(x),
          Frel=round(addmargins(prop.table(x)),2),
          FAcum=round(cumsum(addmargins(prop.table(x))),2))
z[6,3] <- NA
z

# Analisando uma variavel qualitativa ordinal

dicionario["X33",2]
class(dados$X33)
dados$X33

# Probabilidade de area onde mora ser atingido por desastre
likert_X33 <- c("Muito pouco provável",
                "Pouco provável",
                "Igualmente právavel",
                "Provável",
                "Muito provável")

dados <- dados %>% 
  mutate(`X33` = factor(`X33`,
                        levels = likert_X33)) 

colsummary <- likert(as.data.frame(dados[,33]))
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

