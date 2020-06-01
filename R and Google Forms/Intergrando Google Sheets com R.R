# IMPORTANDO E ANALISANDO DADOS DO GOOGLEFORM #
# Tutores: Gilvan Guedes (EPOPEA & Cedeplar); Jéferson Andrade (EPOPEA & ICEx)
# Emails: grguedes@epopea.com.br; jeferson@epopea.com.br
# Local: Zoom Webconference Room
# Data: 09/06/2020
# Horário: 14:00
# Duração: 90 minutos

#------------------------------------#
# IMPORTANDO OS DADOS DO GOOGLE FORM #
#------------------------------------#

# Limpando todos os objetos atualmente ativos no Ambiente (Environment)
rm(list=ls(all=T)) # apaga funções e o Global Environment
rm(list = setdiff(ls(), lsf.str())) # Se quiser apagar apenas objetos do Global Environment
rm(list=lsf.str(all=T)) # quando quiser apagar apenas as funções, e não o Global Environment
#rstudioapi::executeCommand("clearHistory", quiet = TRUE)  # Limpando o histórico de comandos (History)

# Instalando os pacotes necessarios para manipular os dados do Google Forms

#meusPacotes = c("googlesheets", "magrittr", "dplyr", "likert")
#install.packages(meusPacotes, dependencies=T)
#require(googlesheets);require('magrittr');require("dplyr")

# Carregando os pacotes instalados

if(!require(googlesheets4)){install.packages("googlesheets4");require(googlesheets4)}
if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
if(!require(magrittr)){install.packages("magrittr");require(magrittr)}
if(!require(likert)){install.packages("likert");require(likert)}
if(!require(googledrive)){install.packages("googledrive");require(googledrive)}
if(!require(httpuv)){install.packages("httpuv");require(httpuv)}

#Verificando se os pacotes acima estão carregados corretamente
search()

# Para usar o comando seguinte, você deve se assegurar de que há uma conta ativa
# no Google (email), e que essa foi a conta usada para criar o Google Form que vai baixar.
# Aqui estamos dando autorização para o R usar os dados da sua conta Google.
drive_auth(email = "workshop.epeopa@gmail.com")

#Verificando se o Usuário cadastrado é o correto
drive_user()
# Acessando o Google Drive do email autorizado acima e baixando o dado em csv
# Primeiro, garantir que a planilha "dados" tenha sido gerada no Google Forms (Aba -> Respostas)
# o comando abaixo cria um objeto do tipo tibble (uma estrutura de dados similar a um dataframe).
# Ele tem uma coluna para cada questao e outra coluna para o horario da resposta ao questionario
# alem do email do respondente (a menos que voce programou o formulario para ser anonimo).
dados <- googledrive::drive_get("dados") %>%
  read_sheet()

# Leia os dados acessados em formato de data frame
dados1 <- googledrive::drive_get("dados") %>%
  read_sheet(sheet = 1,
             col_names = FALSE, # fazendo com que o texto da pergunta nao seja lido como col_names
             skip=0) # Se eu quiser eliminar o texto da pergunta da primeira linha -> skip=1


# Visualizando os dados
View(dados1)

dados1 %<>% as.data.frame()

#---------------------------------#
# ORGANIZANDO OS DADOS IMPORTADOS #
#---------------------------------#

# Salvando a primeira linha como dicionario de dados
dicionario <- dados1[1,] %>%
  mutate_all(as.character) # transformando todas as 

# Colocando o dicionario em forma de coluna
dicionario=t(dicionario) # o comando t() muda o objeto para matriz

# Juntando o nome das colunas do banco de dados com o nome das variaveis do dicionario
# e mudando o nome das colunas do dicionario com bind_cols 
dicionario=bind_cols("Codigo das variaveis"=colnames(dados1),
                     "Nome das variaveis"=dicionario)


# Verificando a classe de cada variavel do dicionario
str(dicionario)

# Visualizando para ver se fizemos tudo certo
View(dados1)

# Salvando os objetos criados ate agora na sua pasta

setwd("/Volumes/users/epopea/Dropbox/Workshops/") # define onde vai ser salvo

getwd() # checa se o diretorio mudou corretamente

save.image(file="dados.workshop.RData")

# Salvando objetos separados

save(data = dados,
     dictionary = dicionario,
     file = "dados.RData")

# Se quiser remover um objeto esquecifico

rm(dados1)

# Remover todos
rm(list=ls(all=T))

