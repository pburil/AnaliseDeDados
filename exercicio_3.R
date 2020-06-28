# Utilizando o banco world do pacote poliscidata, faça um  
# histograma que também indique a média  e um boxplot 
# da variável gini10
# Descreva o que você pode observar a partir deles.

library(tidyverse)
library(poliscidata)
banco <- world

ggplot(banco, aes(gini10)) + 
  geom_histogram(aes(y=..density..),
                 binwidth = 5) +
  geom_density() +
  geom_vline(aes(xintercept = mean(gini10, na.rm = T)))

ggplot (banco, aes(x = gini10)) +
  geom_boxplot()
  
ggplot (banco, aes(x= gini10, y = "")) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

A média do histograma é 40 e a moda é entre 35 e 48, mais ou menos. Enquanto que a mediana no boxplot é um pouco 
menor do que 40. Separando em quintis, vemos que a curva de gini se concentra no segundo quintil (50%).
  
# Utilizando as funções de manipulação de dados da aula passada,
# faça uma tabela que sumarize a media (função mean), 
# mediana (funcao median) e o desvio padrão (fundao sd) da 
# renda per capta (variável gdppcap08), agrupada por tipo de regime 
# (variável democ).
# Explique a diferença entre valores das médias e medianas.
# Ilustre com a explicação com gráfico de boxplot.
# Os dados corroboram a hipótese da relação entre democracia
# e desempenho economico?

banco %>%
  group_by(democ) %>%
  summarise(mean(gdppcap08, na.rm = TRUE), + median(gdppcap08, na.rm = TRUE), + sd(gdppcap08, na.rm = TRUE))

ggplot(banco, aes(x = democ, y = gdppcap08)) +
  geom_boxplot() +


A Média é o valor médio de um determinado conjunto de casos, o formato aritmético é: o somatório de n / n, onde n é o número de casos.
A Mediana é o valor do centro de um conjunto de dados (se esse conjunto for par). Caso seja ímpar: (a + b)/ 2, onde a e b sejam os casos centrais.

No nosso exemplo, percebemos que a renda per capta em países democráticos é maior do que em países não democráticos.
A média de renda per capta dos países democráticos é bem maior do que a renda per capta de países não democráticos. Isso também serve para as medianas.
Também vemos alguns casos outliers. 

Por fim, os dados corroboram com a hipótese de que em sistema democráticos há um melhor desempenho econômico.


# Carregue o banco states que está no pacote poliscidata 
# Mantenha apenas as variáveis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

library(poliscidata)
banco <- states
banco_selecionado <- banco %>%
  select(obama2012, conpct_m, hs_or_more, prcapinc, blkpct10, south, religiosity3, state)


# Carregue o banco nes que está no pacote poliscidata
# Mantenha apenas as variáveis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state

library(poliscidata)
banco <- nes
banco_selecionado_2 <- banco %>%
  select(obama_vote, ftgr_cons, dem_educ3, income5, black, south, relig_imp, sample_state)

# As variáveis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educação, renda, cor, norte-sul, 
# religiosidade e estado. A diferença é que o nes é um banco de
# dados com surveys individuais e o states é um banco de dados
# com informações por estado
#
# Faça um gráfico para cada banco representando o nível de
# conservadorismo. Comente as conclusões que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, vocês podem ter mais informações sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descrição das
# variáveis

?states

ggplot(banco_selecionado, aes(x = conpct_m)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 2.5) +
  geom_vline(aes(xintercept = mean(conpct_m, na.rm = T)))


ggplot(banco_selecionado_2, aes(x = ftgr_cons)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 10) +
  geom_density() +
  geom_vline(aes(xintercept = mean(ftgr_cons, na.rm = T)))

Percebemos no 1º gráfico que os americanos se concentram majoritamente no nível 35 de conservadorismo. A média fica muito próxima desse valor.
No 2º gráfico percebemos que a moda de conservadorismo é o 50.

# Qual é o tipo de gráfico apropriado para descrever a variável
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gráficos

## Para o Banco States: ## 

head(banco_selecionado)
tail(banco_selecionado)
str(banco_selecionado)
install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(banco_selecionado, aes(obama2012)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=2.5) +
  geom_density() +
  geom_vline(aes(xintercept = mean(obama2012, na.rm = T))) +
  labs(title = "Votos em Obama por Estado", x = "Votos em Obama", y = "Densidade")

ggplot(banco_selecionado, aes(obama2012, state)) +
  geom_point ()

## Achei interessante utilizar o histograma para o banco states, pois states é um banco de dados com informações por estado ##
## Cada caso no eixo y representaria a densidade dos estados que tiveram determinado valor x de votos em Obama ##
## já no eixo x, seria a quantidade de votos para Obama ##
## Diminui o bins para que pudessemos ver a votação de mais estados separadamente. ##
## A média dos votos para Obama por estado ficou um pouco abaixo dos 50 e a moda de votos foi mais ou menos 55% ##
## Conforme conversado, tentei relacionar a variável obama2012 com os estados. Por isso também utilizei o comando geom_beeswarm.

## Para o Banco NES ## 

head(banco_selecionado_2)
tail(banco_selecionado_2)
str(banco_selecionado_2)

ggplot(banco_selecionado_2, aes(obama_vote)) +
  geom_bar()

## Achei interessante utilizar o gráfico de barras, pois como a variável obama_vote é uma variável binária, ##
## cada coluna mostra o caso 0 ou o caso 1 para o eleitorado americano. ##


# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com não-negros. A partir disso, faça
# dois gráficos com a proporção de votos no obama.
# O que você pode afirmar a partir dos gráficos?
# Você diria que existe uma relação entre voto em Obama e cor?

?nes

negros <- nes %>%
transmute(obama_vote, black) %>%
  filter(black == "YES") 

ggplot(negros, aes(obama_vote, ..count../sum(..count..))) +
geom_bar() + 
scale_y_continuous(labels = percent)

library(poliscidata)
brancos <- nes %>%
  transmute(obama_vote, black) %>%
  filter(black == "NO") 
  
ggplot(banco2, aes(obama_vote, ..count../sum(..count..))) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

#Percebemos que há diferença entre a cor da pele e os votos em Obama. No caso dos negros, há um maior percentual de vontantes.


# A partir do banco de dados states, faça uma comparação semelhante.
# Faça um gráfico com as porcentagens de votos em Obama para estados
# que estão acima da mediana da porcentagem de população negra nos estados,
# e outro gráfico com as porcentagens de votos em Obama para os estados
# que estão abaixo da mediana de população negra.
# O que você pode afirmar a partir dos gráficos?
# Podemos chegar a mesma conclusão anterior?

?states
votos_negros <- states 

votos_negros %>%
transmute(obama2012, blkpct10, state) %>%
  filter(blkpct10 >= median(blkpct10))

ggplot(votos_negros, aes(blkpct10)) +
  geom_density()

ggplot(votos_negros, aes(y = blkpct10)) + 
  geom_boxplot()

votos_negros2 <- states

votos_negros2 %>%
  transmute(obama2012, blkpct10, state) %>%
  filter(blkpct10 < median(blkpct10))

ggplot(votos_negros2, aes(blkpct10)) +
  geom_density()

ggplot(votos_negros2, aes(y = blkpct10)) +
  geom_boxplot()


## Não consegui perceber uma diferença significativa entre os dois gráficos. Ao meu ver, não podemos chegar na mesma conclusão
## do exercício anterior ##



# A partir da varíavel X do banco df abaixo
df <- data.frame(x = cos(seq(-50,50,0.5)))
# Faça os tipos de gráficos que representem esse tipo de variável
# comentando as diferenças entre elas e qual seria a mais adequada

df <- data.frame(x = cos(seq(-50,50,0.5)))

ggplot(df, aes(x)) +
  geom_bar()

Esse é o gráfico de barras traidicional. Bota uma variável no Eixo X e outra no eixo Y, como não especificamos, será definido como count

ggplot(df, aes(x, ..count../sum(..count..))) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

ggplot(df, aes(x)) +
  geom_histogram()

ggplot(df, aes(x)) +
  geom_density()

Gráfico de densidade. Quantos casos aparecem por caso de X

ggplot(df, aes(x)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 5) +
  geom_density()

ggplot(df, aes(x)) +
  geom_boxplot()

Gráfico de pontos. Determina o mínimo, máximo e a média dos casos para a variável x.

ggplot(df, aes(x, y = "")) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

Aqui separa por quintis de renda ou separa os casos em quintis.

ggplot(df, aes("", x)) +
  geom_beeswarm()

# responsa as questões teóricas abaixo

1)

Variável independente  ------------------------------------------------------------------------------------------------> Variável dependente 
(Desempenho econômico)    países com melhor desempenho econômico tem melhor capacidade de monitoramento                 (Saúde dos oceanos)





                                    países com melhor desempenho econômico tem melhores oceanos 
Variável independente  ---------------------------------------------------------------------------------------> Variável dependente
(IDH, pib per capita, índice de gini)                                                            (Ocean Health Index, seus goals e sub goals)

2)
Os dados estão amplamente disponíveis. Tanto os da variável independete (IDH, pib per capita, gini), quanto o OHI (Ocean Health Index) 
ou alguma outra base de dados ambiental que possa surgir no decorrer do artigo.

3) O OHI fornecem um quadro robusto para avaliar a saúde dos oceanos e motivar uma melhor recolha de dados
para reforçar as futuras interações do índice.
Ao desenvolver o índice, os autores abordam seis grandes desafios: 
(1) identificar um modesto número de objetivos amplamente aceites para avaliar a saúde e os benefícios dos oceanos em
qualquer escala; 
(2) desenvolver modelos que meçam, com razoável precisão como cada objetivo é alcançado; 
(3) definir pontos de referência robustos para cada modelo; 
(4) incorporar a sustentabilidade no índice; 
(5) assegurar que o índice responde a diferenças e mudanças reais no oceano saúde e benefíciarios;
(6) permitir flexibilidade para se adaptar às restrições (ou melhorias futuras) da disponibilidade, qualidade e quantidade de dados.
Embora o índice possa ser implementado em qualquer escala, aqui concentramo-nos
em escalas de zona económica global e exclusiva (ZEE).

Em relação as variáveis independentes: O IDH, Gini e Pib per capita são amplamente usadas em trabalhos econômicos.
Apesar do IDH ser um índice composto por uma média entre variáveis multisetoriais, ele detém de amplo uso e capacidade de replicação

4) Acredito que o método atual seja um método robusto pra operacionalizar as variáveis. O OHI é um banco de dados colaborativo e global,
onde diversos pesquisadores o atualizam anualmente. Ele também é um índice robusto, composto por 10 goals e múltiplos sub-goals para chegar 
na conclusão do valor da saúde dos oceanos. Os goals são:
  
  -Food provision
  -Artisanal fishing opportunity 
  -Natural products
  -Carbon storage
  -Coastal protection
  -Tourism and recreation
  -Coastal livelihoods and economies
  -Sense of place
  -Clean waters
  -Biodiversity 

Vários desses goals são compostos por sub-goals para calcular os seus valores. 

## Para entender melhor sobre o Índice acessar: http://www.oceanhealthindex.org/methodology/goals ##

Como as váriaveis econômicas são amplamente difundidas, foquei mais no OHI.




