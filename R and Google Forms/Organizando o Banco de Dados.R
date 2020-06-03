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
if(!require(skimr)){install.packages("skimr");require(skimr)}
if(!require(ggplot2)){install.packages("ggplot2");require(ggplot2)}

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

# Criando uma nova variável: Idade atual
dados$Idade <- year(X1) - year(X7)

# Criando uma nova variável: Dummy para Idade acima da idade mediana
dados$Dummy_Idade <- case_when(
  dados$Idade > median(dados$Idade) ~ 1,
  TRUE ~ 0
)
dados$Dummy_Idade %<>% as.factor()

# Criando uma nova variável: quartil de Idade
dados$Quartil_Idade <- cut(dados$Idade, breaks = quantile(dados$Idade, probs = seq(0, 1, 0.25)), include.lowest = TRUE)

# Criando uma nova variável: Grupo etário quinquenal
dados$grupo_etario <- cut(dados$Idade, breaks = seq(0, 80, 5), include.lowest = TRUE, right = FALSE)

# Criando uma nova variável: Coorte
dados$coorte <- as.factor(case_when(
  dados$X6 < 2010 ~ "<2010",
  between(dados$X6, 2010, 2014) ~ "2010-2014",
  between(dados$X6, 2015, 2019) ~ "2015-2019",
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

# Tabela de Frequencia Relativa de Tipo de Disciplina a ser Dada Online
tabs(dados$X9)

x=unlist(strsplit(dados$X9,","))
tabs(x)

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
  mutate(X10 = factor(X10,
                      levels = c(1:5),
                      ordered = TRUE,
                      labels=likert_X10)) 

colsummary <- likert(as.data.frame(dados[,10]))
summary(colsummary)
plot(colsummary)

# Criando os rotulos das categorias das variaveis com escala Likert para varias variaveis

likert_X12 <- c("Completamente vulneravel",
                "Vulneravel",
                "Indiferente",
                "Protegido",
                "Completamente protegido")

likert_X13 <- c("Muito ruim",
                "Ruim",
                "Nem bom nem ruim",
                "Bom",
                "Muito bom")

dados <- dados %>% 
  mutate(X12 = factor(dados$X12,
                      levels = c(1:5),
                      ordered = TRUE,
                      labels=likert_X12),
         X13 = factor(dados$X13,
                      levels = c(1:5),
                      ordered = TRUE,
                      labels=likert_X13)) 

# Salvando um subconjunto de dados com as variaveis tipo Likert

temp = subset(dados,select=c(X10,X12,X13))

# Conferindo as classes das novas variaveis (todas tem que ser fator)
str(temp)
temp=as.data.frame(temp)
names(temp)
colnames(temp) = c("Aulas online x presenciais",
                   "Vulnerabilidade a Covid-19",
                   "Expertise em codigo-aberto")
colsummary <- likert(temp)
summary(colsummary) # Descrevendo os resultados
plot(colsummary) # Criando um grafico de barra dos resultados

# Tabelas Cruzadas de duas variáveis qualitativas

# Jeito mais simples
x = table(dados$X12, dados$X8) # valores absolutos

prop.table(x) # valores relativos

# Usando a funcao CrossTable (do pacote "gmodels")
if(!require(gmodels)){install.packages("gmodels");require(gmodels)}

# Tabela com proporcoes em relacao ao total da amostra
CrossTable(dados$X12, dados$Dummy_Idade, # as duas variaveis de analise
           prop.t=TRUE, # se a soma é no total da amostra
           prop.chisq=FALSE, # para nao mostrar teste de hipotese
           prop.r=FALSE, # se total da soma é na linha
           prop.c=FALSE, # se total da soma é na coluna
           format=c("SPSS"), # tipo de formato de apresentacao
           digits=2) # numero de casas decimais para percentual

# Tabela com proporcoes em relacao ao total da linha
CrossTable(dados$X12, dados$Dummy_Idade, # as duas variaveis de analise
           prop.t=FALSE, # se a soma é no total da amostra
           prop.chisq=FALSE, # para nao mostrar teste de hipotese
           prop.r=TRUE, # se total da soma é na linha
           prop.c=FALSE, # se total da soma é na coluna
           format=c("SPSS"), # tipo de formato de apresentacao
           digits=2) # numero de casas decimais para percentual

# Tabela com proporcoes em relacao ao total da coluna
CrossTable(dados$X12, dados$Dummy_Idade, # as duas variaveis de analise
           prop.t=FALSE, # se a soma é no total da amostra
           prop.chisq=FALSE, # para nao mostrar teste de hipotese
           prop.r=FALSE, # se total da soma é na linha
           prop.c=TRUE, # se total da soma é na coluna
           format=c("SPSS"), # tipo de formato de apresentacao
           digits=2) # numero de casas decimais para percentual

#-----------------------#
# Representacao Grafica #
#-----------------------#

# Representando variaveis nominais com grafico de barras

ggplot(dados,aes(x=X8,fill=X8)) + 
  geom_bar() +
  labs(title = "Grafico de Barras",
       x = "Sexo ao nascer",
       y = "Contagem") +
  geom_text(stat='count',
            aes(label=..count..),
            vjust=-0.5) +
  scale_x_discrete(labels=c("Light","Heavy")) +
  #scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("Sexo ao nascer",
                    labels = c("Light", "Heavy"),
                    values = c("green", "red"))

barX=barplot(main="Grafico de Barras",table(dados$X8),xlab="Sexo ao Nascer", ylab="Contagem", col=c("green","red"), names.arg=c("Feminino","Masculino"))
text(cex=1, pos=3, x=barX, y=table(dados$X8), xpd=TRUE) 
legend("topleft",legend=c("Feminino","Masculino"),fill=c("green","red"))

# Boxplot

with(dados,boxplot(Idade ~ coorte))

ggplot(dados,aes(y=Idade,x=coorte)) +
  geom_boxplot()

# Scatterplot

plot(dados$X6,dados$Idade,pch=19,col="blue")

ggplot(dados,aes(x=X6,y=Idade))+
  geom_point() + geom_smooth(method="loess")

# Line Plot

plot(dados$X6,dados$Idade,frame=F, pch=19)
lines(sort(dados$X6),sort(dados$Idade,decreasing=T),type = "l")

ggplot(dados,aes(x=X6,y=Idade)) +
  geom_line() +
  geom_point()

# Pie Plot

pie(table(dados$X9),col=c("red","green","blue"))
legend("bottomleft",legend=levels(dados$X9),
       fill=c("red","green","blue"),
       cex=0.6)

ggplot(dados, aes(x=factor(1), fill=X9))+
  geom_bar(width = 1)+
  coord_polar("y")

rm(barX,temp,colsummary,x)
rm(list = ls.str(mode = 'character'))
rm(list = lsf.str(all=T))
# Salvando novamente os dados depois de tudo que fizemos
save.image(file="dados.workshop.modificado.RData")

