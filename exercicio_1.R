
# Entre no seguinte link:
# https://pt.wikipedia.org/wiki/Eleição_presidencial_no_Brasil_em_2002
# Vá até o tópico RESUMO DAS ELEICOES
# Crie um vetor com o nome dos seis candidatos a presidência

candidatos <- c("Luiz Inácio Lula da Silva", "José Serra", "Anthony Garotinho", "Ciro Gomes", "José Maria de Almeida", "Rui Costa Pimenta")
candidatos

# Crie um vetor com a sigla do partido de cada candidato

partido <- c("PT", "PSDB", "PSB", "PPS", "PSTU", "PCO")
partido

# Crie um vetor com o total de votos de cada candidato
  
votos_candidatos <- c(39455233, 19705445, 15180097, 10170882, 402236, 38619)
votos_candidatos

# Crie um objeto calculando a soma do votos dos candidatos no 1o turno
  
total_votos <- sum(votos_candidatos)
total_votos

# Crie um vetor com a porcentagem de votos de cada candidato
# fazendo uma operação aritmética entre o objeto votos_candidatos
# e o objeto total_votos

porcentagem_votos <- (votos_candidatos/total_votos)*100
porcentagem_votos

# Crie uma matriz que conste uma coluna com o total de votos de cada candidato
# e outra com a porcentagem dos votos de cada candidato

matriz_votos <- matrix(c(votos_candidatos, porcentagem_votos), byrow = FALSE, nrow = 6)
matriz_votos

# Nomeie as linhas da matriz com o nome dos candidatos

row.names(matriz_votos) <- candidatos
row.names(matriz_votos)

# Nomeie também as colunas

colnames(matriz_votos) <- c("votos_candidatos", "porcentagem_votos")
matriz_votos

# Crie um dataframe com o nome dos candidatos, o partido,
# a quantidade de votos e o percentual

frame_votos <- data.frame (candidatos, partido, votos_candidatos, porcentagem_votos)
frame_votos

# Crie um vetor lógico, indicado TRUE ou FALSE, com a informacao se
# o candidato foi para o segundo turno

segundo_turno <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
segundo_turno

# Adicione esta coluna no dataframe

frame_votos <- cbind(frame_votos, segundo_turno)
frame_votos

# Calcule a soma da porcentagem dos dois candidatos que obtiveram mais votos

frame_votos[1,4] + frame_votos[2,4]


# Exiba as informações do dataframe dos dois candidatos com mais votos

nova_variavel <- frame_votos$segundo_turno == "TRUE"
frame_votos[nova_variavel, ]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# [1] 24 18 31

q <- c(47, 24, 18, 33, 31, 15)
q[c(2, 3, 5)]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# Out Nov
#  24   2

x <- c(5, 4, 24, 2)
y <- c("Ago", "Set", "Out", "Nov")
names(x) <- y

x[c(3:4)]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# 'data.frame':	2 obs. of  2 variables:
# $ x: Factor w/ 2 levels "d","e": 1 2
# $ y: num  1 4

df <- data.frame(x = c("d", "e"), y = c(1,4))

str(df)

###############################################################################

# Crie a seguinte matriz
#
#       [,1] [,2] [,3]
# [1,]   19   22   25
# [2,]   20   23   26
# [3,]   21   24   27


matriz <- matrix(19:27, byrow = FALSE, nrow = 3)

matriz

###############################################################################

# Se Z é uma matriz 4 por 4, qual é o resultado de Z[1,4] ?

matriz <- matrix(1:16, nrow = 4)
matriz

matriz[1,4]

# O primeiro elemento e a quarta coluna da matriz

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#  W3 W4 W1 W2 
#  20 69  5 88 

y <- c(20, 69, 5, 88)
q <- c("W3", "W4", "W1", "W2")
names(q) <- y

str(q)

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#       [,1] [,2]
# [1,]    4    6
# [2,]    3    7
# [3,]    1    8


cbind(c(4, 3, 1), c(6, 7, 8))



###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#       [,1] [,2] [,3] [,4]
# [1,]    1    3   13   15
# [2,]    2    4   14   16

x <- 1:4
y <- 13:16

matrix(c(x, y), nrow = 2, byrow = FALSE)

###############################################################################

# Crie o seguinte dataframe df
#
# df
#    x  y    z
# 1 17  A  Sep
# 2 37  B  Jul
# 3 12  C  Jun
# 4 48  D  Feb
# 5 19  E  Mar

x <- c(17, 37, 12, 48, 19)
y <- c("A", "B", "C", "D", "E")
z <- c("Sep", "Jul", "Jun", "Feb", "Mar")

df <- data.frame(x, y, z)
df

# Ainda utilizando o dataframe df,
# qual código produziria o seguinte resultado?
#
#    x  y
# 1 17  A
# 2 37  B
# 3 12  C

df[1:3, (c("x", "y"))]


###############################################################################

# Responder o exercício teórico abaixo

###############################################################################

# Elaborar uma explicação causal teórica

Quanto maior o nível de desenvolvimento econômico de um país, melhor o desempenho ambiental desse país

# Elaborar Hipóteses

A hipótese central é que em países com maior desenvolvimento econômico, as instituições são mais robustas, 
detendo de uma melhor capacidade de monitoramento, coerção e sanção entre os atores envolvidos (sociedade civil, indústrias).
além de que em países desenvolvidos economicamente há um maior capital cívico entre os indivíduos dessa comunidade.

# Pensar em como operacionalizar os conceitos teóricos em variáveis empíricas 

Desenvolvimento econômico = PIB per capita, IDH, curva de Gini

Desempenho ambiental = OHI (ocean health index), GHG Emissions (Green House Gases Emission), Municipal Waste OECD Data, etc

#Estabelecer o tipo de relação entre as variáveis operacionalizadas 

Associação linear (correlação linear), quando um aumenta, a outra também aumenta. 
