
## Faça todos os gráficos utilizando um tema que você ache mais adequado
## e nomeie os eixos x e y da maneira adequada

ggplot(banco, aes(fct_infreq(democ_regime08))) +
geom_bar() +
  theme_classic() +
  labs(title = "Frequência por tipo de regime", subtitle = "Regimes Democráticos e não democráticos", x = "Tipos de Regime", y = "Quantidade")

ggplot(banco, aes(democ_regime08)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
labs(title = "Frequência por tipo de regime", subtitle = "Regimes Democráticos e não democráticos", x = "Tipos de Regime", y = "Densidade")

ggplot(banco, aes(y = democ_regime08)) +
  theme_minimal() +
  labs(title = "Frequência por tipo de regime", subtitle = "Regimes Democráticos e não democráticos", x = "Tipos de Regime", y = "Densidade") +
  geom_boxplot()

install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(banco, aes("", democ_regime08)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_minimal() +
  geom_beeswarm() +
  labs(title = "Frequência por tipo de regime", subtitle = "Regimes Democráticos e não democráticos", x = "Tipos de Regime", y = "Densidade")


## Carregue o banco world do pacote poliscidata

library(tidyverse)
library(poliscidata)

banco <- world

## Observe o banco de dados com as funções adequadas

head(banco)
tail(banco)
str(banco)
summary(banco)
glimpse(banco)

## A variável democ_regime08 indica se um país é democrático.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta variável 
## graficamente

banco %>%
  count(democ_regime08)

ggplot(banco, aes(x = democ_regime08)) +
  geom_bar() +
  theme_tufte() +
  labs(title = "Sistema Político dos Países", x = "Tipos de Regime", y = "Quantidade")


## Teste a relação entre a variável democ_regime08 e a variável
## muslim (que indica se um país é muçulmano ou não). E represente
## visualmente as variáveis para visualizar se esta religião
## aumenta ou diminui a chance de um país ser democrático
## Qual seria sua conclusão com relação a associação destas duas
## variáveis?

banco2 <- world %>%
  filter(!is.na(democ_regime08),
         !is.na(muslim))


tabela <- table(banco2$muslim, banco2$democ_regime08)
prop.table(tabela)
chisq.test(tabela)

library(graphics)
mosaicplot(tabela, shade = TRUE)

library(vcd)
assoc(tabela, shade = TRUE)
  
Podemos perceber que quando os países não são mulçumanos há mais países democráticos, enquanto que 
quando os países são mulçumanos, eles são menos democráticos 
Quando o país é mulçumano, tem menos chance dele ser democrático.
O resultado do p-valor corrobora com essa hipótese


## A variável gdppcap08 possui informação sobre o PIB per capta
## dos países. Faça uma representação gráfica desta variável

banco3 <- world %>%
  filter(!is.na(gdppcap08)) 


ggplot(banco3, aes(gdppcap08)) +
  geom_density(adjust = 0.5)  

ggplot(banco3, aes(x = "", y = gdppcap08)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


Nesses 2 gráficos vemos que a concentração de países com um PIB per capita elevado tende a diminuir. É mais comum
encontrar países na faixa do segundo e terceiro quartis.

  
## Faça um sumario com a média, mediana e desvio padrão do pib per capta
## para cada tipo de regime politico, represente a associação destas
## variáveis graficamente, e faça o teste estatístico adequado para
## chegar a uma conclusão. Existe associaçào entre as variáveis?

banco %>%
group_by(democ_regime08) %>%
  summarise(mean(gdppcap08, na.rm = TRUE),
            median(gdppcap08, na.rm = TRUE),
             sd(gdppcap08, na.rm = TRUE),
            n = n())

t.test(gdppcap08 ~ democ_regime08, data = banco)

ggplot(banco, aes(democ_regime08, gdppcap08)) +
  theme_minimal() +
  geom_boxplot()


O intervalo de confiança não inclui o 0, isso seria um sinal de que há significância estatistica no modelo.
Também temos um p-valor baixo. Isso seria um sinal de que há significância estatistica. 
A diferença entre as médias também é uma informação importante para afirmar de que há relação entre o tipo do regime
 e o desempenho econômico

## Por fim, ao invés de utilizar uma variável binária de democracia,
## utilize a variável dem_score14 para avaliar se existe uma associação
## entre regime político e desenvolvimento econômico. Represente
## a associação graficamente, faça o teste estatístico e explica sua
## conclusão

cor.test(banco$dem_score14, banco$gdppcap08)

ggplot(banco, aes(dem_score14, gdppcap08)) +
  theme_minimal() +
  geom_point(alpha = 0.8)



O valor 0 não está presente no intervalo de confiança da correlação, isso significaria que há significância estatística 
O valor da correlação entre as variáveis é de 0.505, isso seria um valor razoável para a correlação entre as variáveis
O valor do p-valor é bastante baixo, reforçando a ideia de correlação entre as variáveis

## Teste a associação entre renda perca capta e religiao (com a variável
## muslim) e represente graficamente. Qual é sua conclusão? 

banco %>%
  group_by(muslim) %>%
  summarise(mean(gdppcap08, na.rm = TRUE),
            median(gdppcap08, na.rm = TRUE),
            sd(gdppcap08, na.rm = TRUE),
            n = n())

t.test(gdppcap08 ~ muslim, data = banco)

ggplot(banco, aes(muslim, gdppcap08)) +
  theme_classic() +
  geom_boxplot()

ggplot(banco, aes(muslim, gdppcap08)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_classic()


O valor do p-valor foi bem baixo, reforçando a ideia de que há significância estátistica
O intervalo de confiança não têm o valor 0, reforçando a ideia de que há significância estátistica
A média dos grupos mostra que a densidade dos países mulçumanos se concentra mais no quintis de baixo pib per capita,
Enquanto que os países não-mulçumanos estão em outros quintis de renda

## Comparando suas conclusões anteriores, é possível afirmar qual
## das duas variáveis possui maior impacto no desenvolvimento economico?
## Por que? 

De acordo com os valores do p-valor e do intervalo de confiança, ao meu ver, aparenta que um sistema político influência
mais no desempenho econômico do que a religião. Os valores do sistema político foram mais relevantes estatisticamente do que
os valores da religião. Também temos que destacar que alguns países mulçumanos não são democráticos e que alguns países 
democráticos são majoritariamente cristãos. Isso poderia ser um grau a ser melhor analisado.


##########################################################################

## Exercício teórico
## Levando em consideração as variáveis de seu trabalho final,
## qual dos 3 testes estatísticos utilizados seria adequado utilizar?

Como irei utilizar variáveis continuas que medem desenvolvimento (PIB per capita e IDH) e utilizarei o Ocean Health Index, 
que utiliza uma variedade de goals e subgoals de natureza distintas, mas sua pontuação global vai de 0 a 100, acredito que utilizar
teste de duas variáveis contínuas (teste de correlação) seria o mais adequado de utilizar
