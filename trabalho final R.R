
  title: "A Democracia é boa para o desempenho ambiental? Uma análise de correlação entre o Ocean Health Index e o V-dem"
author: "Pedro Buril Saraiva Lins"
output: r_document

## 1. Introdução

Mais democracia resulta em melhor qualidade ambiental? Este trabalho busca compreender a relação entre a democracia e o meio ambiente em suas várias dimensões e aspectos. Uma pergunta central dessa pesquisa é: há correlação estatística na conciliação entre dois ideais normativos amplamente aceitos, a sustentabilidade ambiental e os valores e práticas democráticas?  (Goodin, 1992).
Democracia e qualidade ambiental são temas que  se tornam cada vez mais indissociáveis nas agendas de pesquisa das Ciências Humanas e Sociais e Sociais Aplicadas, mas também são pautas recorrentes nas  ruas, mídias impressas e digitais. Esses debates têm  fomentado diversas agendas de pesquisa, mais ou menos interdependentes entre si. No campo da Ciência Política, em especial na Política Comparada, nos últimos vinte anos vêm sendo construída uma (entre diversas) agenda de pesquisa cujo intuito não é apenas mensurar qual o grau de maior ou menor governabilidade democrática entre os países, mas verificar se um melhor desempenho democrático resulta também em melhor qualidade ambiental (Rodrigues, 2015).
Apesar de todos os esforços na tentativa de identificação de inferências, ainda há na literatura bastante divergência entre as correntes de pesquisa. De acordo com Rodrigues (2014), Democracia não é um regime político perfeito para salvaguardar bens e serviços ambientais e  ecossistêmicos, mas é o modelo de regime que permite  maior abertura política para uma sociedade e economias mais sustentáveis ambientalmente. Entretanto, para Lijphart (1971), o regime democrático majoritário ou consociativo por si só não garante melhores resultados de desempenho ambiental para os países. Pickering (2020), entretanto, argumenta que esses dois ideais são freqüentemente concebidos como estando em conflito. Se os cidadãos atribuem baixa prioridade aos valores ecológicos, os esforços para fortalecer a proteção ambiental e a sustentabilidade por meio de processos democráticos podem falhar. Por outro lado, garantir valores ambientais por meio de regras autoritárias tem um alto preço democrático. Por fim, Eckersley (2019) elabora uma crítica contra o pano de fundo do otimismo pós-Guerra Fria sobre a democracia entre muitos teóricos políticos. Esses teóricos muitas vezes viam a democracia discursiva ou deliberativa como uma forma promissora de fortalecer os resultados ecológicos por causa do potencial para um diálogo inclusivo e respeitoso para priorizar interesses compartilhados de longo prazo sobre os interesses privados de curto prazo. Na década de 2000, os estudiosos continuaram a explorar as conexões entre a proteção ambiental e a democracia deliberativa (por exemplo, Baber & Bartlett, 2005; Smith, 2003). Muitos dos primeiros trabalhos sobre o nexo democracia-ambiente prescreveram governança participativa e descentralizada, cidadania e movimentos sociais de base como antídotos para o mal-estar ambiental (Mitchell, 2006). Outros (por exemplo, Goodin, 1992; Jasanoff, 1996) enfatizaram a dificuldade de resolver as tensões entre a ciência ambiental e os resultados "verdes" de um lado e os processos democráticos do outro.
Dessa forma, este trabalho busca acrescentar a literatura sobre o impacto da Democracia nos índices ambientais. O desenho de pesquisa desse artigo se baseia na comparação de dois índices, um de caráter político -V-dem- e outro ambiental -Ocean Health Index-.  Utilizaremos da estatística descritiva e estatística inferencial para analisar a associação entre desempenho democrático e qualidade ambiental. Para cumprir com os nossos objetivos, esse trabalho divide-se em quatro etapas: a  primeira buscou apresentar os pressupostos teóricos e  metodológicos que reforçaram a necessidade de uma  agenda de pesquisa envolvendo qualidade democrática  e desempenho ambiental; na segunda e terceira etapas,  foram apresentados o desenho de pesquisa, bem como  os índices operacionalizados, V-dem (Varieties of Democracy) e o Ocean Health Index (OHI), respectivamente; na quarta etapa foram apresentados os resultados da análise.

## 2. Materiais e Métodos 

Regimes mais democráticos têm melhor desempenho ambiental? Qual o efeito do nível de democracia sobre a preservação dos recursos naturais? Nosso desenho de pesquisa foi elaborado para responder a essas perguntas. Para isso, examinamos a relação entre dois índices: I) o V-dem e suas múltiplas dimensões que operacionalizam a Democracia; II) o Ocean Health Index (Índice da Saúde dos Oceanos, no acrônimo em Português). O primeiro dimensiona a qualidade democrática dos regimes políticos e o segundo visa mensurar a saúde dos oceanos nos países costeiros. A tabela 1 sumariza nosso desenho de pesquisa. 

```{r, echo = FALSE, include = FALSE}

library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dotwhisker)
library(lmtest)
library(car)
library(GGally)
library(knitr)
library(scales)

vetor_populacao <- c("114 países")
vetor_variaveldependente <- c("Índice de Saúde dos Oceanos (OHI) - Média dos anos 2012 - 2019")
vetor_variavelindependente <- c("V-dem (2019)")
vetor_variavelcontrole <- c("Índice de Desenvolvimento Humano (IDH) - 2019")
vetor_hipotese <- c("A democracia exerce um efeito positivo e significativo sobre o desempenho ambiental")
vetor_tecnica <- c("Estatística descritiva; Regressão Linear; Regressão multivariada")

tabela1 <- matrix(c(vetor_populacao, vetor_variaveldependente, vetor_variavelindependente, vetor_variavelcontrole, vetor_hipotese, vetor_tecnica), byrow = TRUE, nrow = 6)

rownames(tabela1) <- c("População", "Variável Dependente", "Variável Independente", "Variáveis de Controle", "Hipótese", "Técnicas")

knitr::kable(tabela1, format = "html", caption = "Tabela 1 - Desenho de pesquisa: indicadores selecionados e técnicas aplicadas")
```

Nosso recorte temporal, na dimensão política, é composto pelos dados mais recentes do Varieties of Democracy, no caso, para o ano de 2019. Para a dimensão ambiental (aqui operacionalizada pelo Índice de Saúde dos Oceanos), é uma média entre todos os anos em que o índice foi calculado (2012-2019). Nosso recorte amostral (n) é composto por 114 casos, esses casos compartilham a mesma característica geográfica de serem países costeiros. Vale a pena destacar a limitação amostral inerente ao desenho de pesquisa elaborado. Como nossa operacionalização da dimensão ambiental envolve a saúde dos oceanos, a semelhança geográfica aqui necessária -ser país costeiro- limita a quantidade amostral. Como solução a esse problema para futuros desenhos de pesquisa, sugere-se a utilização das microrregiões dos países. Por fim, as variáveis utilizadas aqui são contínuas e todas variam em escala de 0 a 1. 
Utilizamos o método dos mínimos ordinários (MMO). A técnica aplicada foi a de regressão simples e regressão multivariada. A regressão é uma técnica que estuda a relação entre duas ou mais variáveis. Em outras palavras, a análise de regressão avalia a amplitude da variação de uma variável, decorrente da variação de outra variável. Iremos analisar o coeficiente de correlação de Pearson (varia de -1 a 1, quanto mais forte dos extremos, mais forte é a correlação). 
Também iremos observar o p-valor, que é uma medida de significância de hipótese. Em outras palavras, o p-valor é a probabilidade de se obter resultados a partir de teste, sob a suposição de que a hipótese nula está correta. Um p-valor muito pequeno significa que tal resultado observado seria muito improvável sob a hipótese nula. Relatar os valores-p de testes estatísticos é uma prática comum em publicações acadêmicas de muitos campos quantitativos. Como o significado preciso do p-valor é difícil de entender, o uso indevido é generalizado tem sido um tópico importante entre os métodos quantitativos. 

## 2.1 Varieties of democracy (V-dem) 

Varieties of Democracy (V-Dem) é uma abordagem que busca conceituar e medir a democracia. O índice é composto por um conjunto de dados multidimensionais e desagregados que tentam refletir a complexidade do conceito de democracia como um sistema de governo que vai além da simples presença de eleições livres. O V-Dem é atualmente um dos maiores projetos de coleta de dados de ciências sociais sobre democracia. A sede é baseada no V-Dem Institute, o Departamento de Ciência Política da Universidade de Gotemburgo, Suécia. 
Três características distinguem o V-Dem de outros esforços anteriores para medir a democracia e nos motivaram a utilizá-lo como índice operacionalizador da Democracia: I) Porque não existe consenso sobre como conceituar e medir a democracia, eles abordam a democracia como multidimensional. Em vez de impor uma definição que necessariamente omite características da democracia que são importantes para alguns usuários, mede-se múltiplas variedades de democracia e permite que os usuários escolham aquela que reflete seu próprio entendimento do conceito; II) o V-dem coleta informações sobre indicadores relevantes para a democracia em um relatório altamente desagregado e disponibiliza esses dados agregados e desagregados; III) os indicadores são estendidos para cada país ao longo da história moderna até 1900, sempre que possível. Embora outros projetos que tentam medir a democracia possam conter um ou vários desses recursos, nenhum os combina todos.
Metodologicamente, o Varieties of Democracy foi construído em uma escala de zero a um e é composto por mais de 400 indicadores individuais. Embora o consenso sobre um conceito central de democracia permaneça incerto, o V-dem tenta operacionalizar a democracia em cinco princípios ou tradições-chave que oferecem abordagens distintas para defini-la -eleitoral, liberal, participativa, deliberativa e igualitária. Chega-se aos valores dos indicadores a partir de um grupo de especialistas em medição e metodologistas que desenvolveram um modelo avançado para agregar e ponderar classificações de especialistas com base na confiabilidade e para calcular intervalos de confiança.
O componente eleitoral da democracia incorpora o valor central de tornar os governantes receptivos aos cidadãos por meio da competição pela aprovação de um amplo eleitorado durante as eleições periódicas. No esquema conceitual V-Dem, o componente eleitoral é fundamental; sem ela, os autores argumentam que não podem chamar um regime de "democrático" em nenhum sentido. Ao mesmo tempo, reconhecemos que realizar eleições por si só é insuficiente e também que os países podem ter "qualidades democráticas" sem serem democracias. A "democracia eleitoral" do V-dem é, portanto, muito semelhante ao conceito de poliarquia de Robert Dahl.
O componente liberal da democracia incorpora o valor intrínseco de proteger os direitos individuais e das minorias contra uma potencial "tirania dos maioria." Isso é alcançado por meio de liberdades civis protegidas constitucionalmente, um Estado de Direito forte e controles e contrapesos eficazes que limitam o uso do poder executivo.     O componente participativo incorpora os valores do governo direto e da participação ativa dos cidadãos em todos os processos políticos; enfatiza formas não eleitorais de participação política, como por meio de organizações da sociedade civil e mecanismos de democracia direta. 
O componente deliberativo consagra o valor central de que as decisões políticas em busca do bem público devem ser informadas por um diálogo respeitoso e razoável em todos os níveis, em vez de por apelos emocionais, apegos solidários, interesses paroquiais ou coerção. 
O componente igualitário sustenta que as desigualdades materiais e imateriais inibem o exercício real dos direitos e liberdades formais; portanto, uma distribuição mais igualitária de recursos, educação e saúde entre os grupos socioeconômicos deve aumentar a igualdade política. 
Esses cinco componentes são projetados para serem conceitualmente distintos, sem sobreposição, embora possam ser empiricamente correlacionados. Além disso, os formuladores do V-dem desenvolveram versões "densas" de cada um desses conceitos que incluem um elemento sobreposto - ou seja, o componente eleitoral, pois há a percepção que nenhum regime deve ser chamado de "democracia" de qualquer tipo, a menos que haja a presença desse componente (Lindberg, Coppedge, Gerring, et al. 2014). Coletivamente, essas versões densas dos cinco conceitos são o que chamamos de "variedades de democracia". Os autores argumentam que, em conjunto, eles oferecem uma explicação bastante abrangente do conceito de democracia. Assim, enquanto a maioria dos índices se concentra apenas nos elementos eleitorais e liberais da democracia, o V-Dem busca medir uma gama mais ampla de atributos associados ao conceito de democracia. 
Existem dezenove subcomponentes que compõe cada um dos cinco componentes da Democracia. É importante destacar que os valores essenciais consagrados nos vários componentes, subcomponentes e indicadores de democracia às vezes entram em conflito uns com os outros, algo natural à Democracia. Os indicadores que utilizamos neste trabalho caem em três categorias principais: 1) dados factuais coletados de outros conjuntos de dados ou fontes originais; 2) indicadores de avaliação codificados por especialistas de vários países; e 3) variáveis agregadas. 
Representamos graficamente e testamos as 5 dimensões da democracia a seguir:
  
  ```{r}

#url3 <- "https://drive.google.com/file/d/1rYP4-uQQRvNxNexb1X-nirwb-zgZzer0/view?usp=sharing"

vdem <- read_csv("V-Dem-CY-Full+Others-v10.csv")

vdem_select <- vdem %>%
  filter(year == 2019) %>%
  select(Country = country_name, year,
         Electoral = v2x_polyarchy, 
         Liberal = v2x_libdem, 
         Participatory = v2x_partipdem, 
         Deliberative = v2x_delibdem,
         Egalitarian = v2x_egaldem) 
names(vdem_select)[1:8]

vdem_select2 <- vdem %>%
  select(Country = country_name, iso3c = country_text_id, COWcode,
         Year = year,
         Electoral = v2x_polyarchy,
         Liberal = v2x_libdem,
         Participatory = v2x_partipdem,
         Deliberative = v2x_delibdem,
         Egalitarian = v2x_egaldem) 

vdemgather <- vdem_select2 %>%
  gather(Dimension, score, -Country, -iso3c, -COWcode, -Year)


ggplot(vdem_select, aes(Electoral)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(Electoral, na.rm = T))) +
  labs(title = "Figura 1",
       subtitle = "Componente Eleitoral", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(vdem_select, aes(Liberal)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(Liberal, na.rm = T))) +
  labs(title = "Figura 2",
       subtitle = "Componente Liberal", 
       caption = "Elaboração própria", 
       fig.align = "center")


ggplot(vdem_select, aes(Participatory)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(Participatory, na.rm = T))) +
  labs(title = "Figura 3",
       subtitle = "Componente Participatório", 
       caption = "Elaboração própria", 
       fig.align = "center")


ggplot(vdem_select, aes(Deliberative)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(Deliberative, na.rm = T))) +
  labs(title = "Figura 4",
       subtitle = "Componente Deliberativo", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(vdem_select, aes(Egalitarian)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(Egalitarian, na.rm = T))) +
  labs(title = "Figura 5",
       subtitle = "Componente Igualitário", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(filter(vdemgather, Year == max(Year)),
       aes(x = score, color = Dimension, fill = Dimension)) +
  geom_density(alpha = 0.4) +
  labs(title = "Figura 6",
       subtitle = "Comparação entre os componentes da Democracia",
       caption = "Elaboração própria",
       fig.align = "center")

ggplot(vdemgather, aes(x = Dimension, y = score, group = Dimension, color = Dimension)) + 
  geom_boxplot() +
  expand_limits(y = c(0,1)) +
  labs(title = "Figura 7",
       subtitle = "Boxplot entre os componentes",
       caption = "Elaboração própria",
       fig.align = "center")

vdemgather %>%
  group_by(Dimension) %>%
  summarise(media = mean(score, na.rm = TRUE), 
            mediana = median(score, na.rm = TRUE), 
            minimo = min(score, na.rm = TRUE),
            maximo = max(score, na.rm = TRUE),
            n = n())

```
Aqui representamos graficamente todas as dimensões e também a média, mediana, mínimo e máximo. Vemos que a dimensão Igualitária tem uma densidade grande próxima do score 0.25 (sugerindo que muitos países não são democraticamente igualitário). Em relação a dimensão eleitoral, vemos que a maior densidade se localiza próximo ao score 75 (justifico isso pelo fato de que a dimensão essencial para ser considerado democrático é a dimensão eleitoral). A dimensão deliberativa é a que tem a moda no pior score, sugerindo que poucos países tem processos deliberativos (conselhos, outros processos de deliberação, etc). A dimensão que tem o maior máximo é a dimensão eleitoral, detendo da melhor pontuação na média também. A dimensão participatória tem a moda próximo do score 0.1, sugerindo que muito países não tem outros processos participatória fora a eleição em si. A dimensão com o menor mínimo é a dimensão deliberativa.

Nesta figura podemos observar a distribuição das dimensões de democracia. Uma das vantagens do boxplot é ver a amplitude dos valores, o que é bem interessante quando estamos comparando as dimensões (anteriormente analisamos cada uma individualmente). Por exemplo, vemos que o intervalo da dimensão eleitoral é maior do que o intervalo da dimensão participatória. Também podemos ver uma grande amplitude dos scores da dimensão deliberativa da democracia. 

## 2.2 Ocean Health Index (OHI) 

Embora existam diversos índices para a avaliação da sustentabilidade ambiental de um país, como o Enviromental Perfomance Index (Índice de Perfomance Ambiental, no acrônimo em Português) ou o Índice de Desenvolvimento Sustentável (Enviromental Sustainability Index), entre outros, procurou-se, neste artigo, focar especialmente a dimensão ambiental oceânica dos países através de um índice que possibilita ao pesquisador reunir dados quantitativos integrais de caráter sistêmico e com grau de confiabilidade elevada. Esse índice, que colaborará com a reflexão interdisciplinar proposta neste artigo, é o Ocean Health Index (OHI). 
O OHI é composto por dez objetivos (e oito sub-metas). Os dez objetivos que compõe o OHI são: I) Provisão de Alimento (Pesca e Maricultura); II) Oportunidade de pesca artesanal; III) Produtos Naturais; IV) Armazenamento de Carbono; V) Proteção Costeira; VI) Turismo e Recreação; VII) Condições costeiras e Economia (Condições costeiras e Economia); VIII) Senso de Pertencimento ao Espaço (Espécies icônicas e Últimos espaços especiais); IX) Águas Limpas; X) Biodiversidade (Habitats e Espécies).
Cada um desses objetivos pode ser considerado separadamente ou agregado em uma pontuação geral para uma região, país ou todo o oceano global. De acordo com Halpern (2012), existem centenas de indicadores específicos para medir os vários aspectos que envolvem a saúde dos oceanos. Entretanto, o diferencial central do OHI é o fato de ser um índice abrangente, utilizando métricas amplamente díspares, permitindo um avaliação integrada de mudanças, partindo da biodiversidade, por exemplo, até a quantidade de empregos costeiros em determinada região.
De acordo com os formuladores do OHI, os países desenvolvidos tendem a pontuar mais alto do que os países em desenvolvimento; O Ocean Health Index é significativamente correlacionado com o Índice de Desenvolvimento Humano (IDH) (r = 0.57, P-value < 0.0001, n = 114). Isso ocorre porque os países desenvolvidos tendem a ter economias mais fortes, melhores regulamentações e infraestrutura para administrar pressões e maior capacidade de buscar o uso sustentável dos recursos. No entanto, alguns países desenvolvidos, como a Polônia e Cingapura, pontuaram mal (índice de pontuação de 0,42 e 0,48, respectivamente), com pontuações baixas em vários goals, enquanto países em desenvolvimento como Suriname (0,69) e Seychelles (0,73) pontuaram relativamente bem porque muitos as metas tiveram pontuações muito altas (Halpern, B. S., Longo, C., Hardy, D., et al. 2012). 
Assim, o artigo busca avaliar as justificativas elaboradas pelos autores de que países mais desenvolvidos pontuam melhor do que os países em desenvolvimentos. Para nós, melhores regulações e busca do uso sustentável dos recursos são dimensões de natureza política, justificando o uso de um índice político como variável independente e um índice econômico como variável de controle. 
Três pontos-chave afetam a interpretação das pontuações do índice. Em primeiro lugar, os resultados para objetivos individuais podem parecer contra-intuitivos, porque avaliamos a saúde do oceano através das lentes de sistemas humanos-naturais. Isso significa que algumas pontuações isoladamente não significam diretamente em melhor sustentabilidade. Por exemplo, as metas extrativas, como "produtos naturais", têm melhor pontuação quando os níveis de colheita são altos, mas sustentáveis, com impactos inerentessobre outras metas. Além disso, uma imagem integrada da saúde do oceano em vários objetivos pode não corresponder às expectativas com base no status de um objetivo individual. Muitos objetivos tiveram baixa pontuação globalmente, em particular 'fornecimento de alimentos', 'produtos naturais' e 'turismo e recreação', enquanto outros objetivos tiveram pontuação mais alta (0,75), incluindo 'armazenamento de carbono', 'águas limpas' e 'biodiversidade'. As conclusões baseadas em uma única escolha se desviarão daquelas derivadas da avaliação geral do índice. Por exemplo, a Rússia teve uma pontuação muito baixa para "fornecimento de alimentos" e "produtos naturais" e muito alta para "águas limpas" e "biodiversidade", e teve uma pontuação de índice geral de 67. A partir desse ponto justificamos a utilização do score geral em nosso desenho de pesquisa.
Em segundo lugar, o índice representa o desempenho de um sistema humano-natural de forma mimética. Em outras palavras, este índice inclui metas que tendem a ser mais valorizadas por ambientalistas e usuários não extrativistase, mas também aqueles mais valorizados por usuários extrativos - fornecer alimentos e recursos naturais, por exemplo. Fundamentando os oceanos como abordagem da dimensão ambiental, mas também como dimensão política, social e econômica.
Em terceiro lugar, o índice permite uma avaliação transparente de como os valores sociais influenciam as perspectivas sobre a saúde dos oceanos. Embora os autores tenham ponderado as metas igualmente para evitar presumir valores sociais, eles reconhecem que as pessoas valorizam os benefícios do oceano de maneira diferente. Para avaliar as consequências potenciais da ponderação desigual, os autores calcularam as pontuações do índice para quatro esquemas de ponderação potencial que aproximam os conjuntos de valores de uso preservacionista, não extrativo, extrativo e fortemente extrativo. Como os pesos das metas podem influenciar as pontuações do índice, é fundamental determinar os valores sociais (pesos) antes do cálculo do índice (Halpern, B. S., Longo, C., Hardy, D., et al. 2012). A escolha de um único esquema de ponderação desigual para esta análise global não teria sido apropriada, pois esses pesos variam por país, região e comunidade.
Por fim, apesar de haver uma ampla literatura que avalie o impacto da qualidade democrática no desempenho ambiental, não há nenhum estudo que utilize ambos os índices de forma sistemática. Assim, este estudo é inovador ao testar essa hipótese a partir do Varieties of Democracy (V-dem) e o Ocean Health Index (OHI). A seguir realizamos a análise exploratória deste índice:
  
  ```{r}

url <- "https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/yearly_results/global2020/Results/data/index_scores_wide_all_years.csv"

download.file(url, "banco.csv", mode = "wb")

banco <- read_csv("banco.csv")

banco <- banco %>%
  rename(Country = region_name, OHI = average) %>%
  select(Country, OHI, "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

banco %>%
  summarise(media = mean(OHI, na.rm = TRUE), 
            mediana = median(OHI, na.rm = TRUE), 
            minimo = min(OHI, na.rm = TRUE),
            maximo = max(OHI, na.rm = TRUE))

ggplot(banco, aes(OHI)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(OHI, na.rm = T))) +
  labs(title = "Figura 8",
       subtitle = "Ocean Health Index", 
       caption = "Elaboração própria", 
       fig.align = "center")

```
Aqui representamos a media, mediana, o mínimo e o máximo do OHI. A média é de 69,96, a mediana 70,3, o mínimo 46 e o máximo 93. Também representamos a partir do gráfico de histograma. Pelo gráfico a moda aparenta ter um score de mais ou menos 75.

### 2.3 Índice de Desenvolvimento Humano (IDH) ###

Por fim, em nosso desenho de pesquisa utilizamos o Índice de Desenvolvimento Humano como variável de controle. O índice é composto por 3 dimensões: I) expectativa de vida ao nascer; II) educação e III) PIB per capita. O Índice de Desenvolvimento Humano tem sido criticado por uma série de razões, incluindo pela não inclusão de quaisquer considerações de ordem ecológica, focando exclusivamente no desempenho nacional e por não prestar muita atenção ao desenvolvimento de uma perspectiva global. Utilizamos o IDH como variável de controle devido a relevância do índice na literatura. Como já abordamos anteriormente, em relação ao Ocean Health Index, "Países desenvolvidos tendem a pontuar mais alto do que os países em desenvolvimento, o OHI é significativamente correlacionado com o IDH" (Halpern, B. S., Longo, C., Hardy, D., et al. 2012). A seguir realizamos a análise exploratória deste índice: 
  
  ```{r}

url2 <- "https://raw.githubusercontent.com/pburil/analisededados/master/idh.csv"

download.file(url2, "idh.csv", mode = "wb")

idh <- read_csv("idh.csv")

idh <- idh %>%
  rename(IDH = Score)

idh$IDH <- as.numeric(idh$IDH)

ggplot(idh, aes(IDH)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(IDH, na.rm = T))) +
  labs(title = "Figura 9",
       subtitle = "Índice de Desenvolvimento Humano (IDH)", 
       caption = "Elaboração própria", 
       fig.align = "center")

idh %>%
  summarise(media = mean(IDH, na.rm = TRUE), 
            mediana = median(IDH, na.rm = TRUE), 
            minimo = min(IDH, na.rm = TRUE),
            maximo = max(IDH, na.rm = TRUE))


list.files()

```
Em relação ao IDH, temos que o máximo é 0.95 (Noruega), o menor IDH é o 0.364 (Somália), a média é de 0.70. Também o representamos graficamente. A pontuação com maior densidade é o 0.72. 

De forma sucinta, o desenho de pesquisa apresentado aqui ficou da seguinte forma: a variável depedente é o Ocean Health Index, as variáveis independentes foram as dimensões de democracia. Essas dimensões foram operacionalizadas a partir das multifacetas dos índices V-dem (democracia) e do Ocean Health Index (meio ambiente), sendo o IDH a variável de controle (justificado a partir da literatura). A hipótese aqui é de uma relação positiva, onde uma melhor democracia levaria a melhores resultados ambientais.

## 3. Qualidade democática e desempenho ambiental: resultados


```{r}
head(vdem_select)

banco_merged <- inner_join(idh, banco, by = c("Country" = "Country"))

banco_final <- inner_join(banco_merged, vdem_select, by = c("Country" = "Country"))

banco_merged2 <- inner_join(vdemgather, idh, by = c("Country" = "Country"))

banco_final2 <- inner_join(banco_merged2, banco, by = c("Country" = "Country")) %>%
  select(Country, iso3c, COWcode, Year, Dimension, score, IDH, OHI)

```

## 3.1 Gráficos bivariados 

```{r}
ggplot(banco_final, aes(Egalitarian, OHI)) +
  geom_jitter() +
  geom_vline(aes(xintercept = mean(Egalitarian, na.rm = T))) +
  theme_minimal() +
  labs(title = "Figura 10",
       subtitle = "Gráfico de dispersão OHI - Egalitarian", 
       caption = "Elaboração própria", 
       fig.align = "center")
```

Neste gráfico de dispersão entre o OHI e a dimensão igualitária da democracia, podemos perceber que os casos que pontuam bem na dimensão egalitarian tem um melhor desempenho ambiental. O gráfico aparenta acompanhar a curva de regressão, posteriormente iremos avaliar esse pressuposto. No gráfico também representamos a média do Igualitário no intercepto do eixo X.


```{r}
ggplot(banco_final, aes(Participatory, OHI)) +
  geom_jitter() +
  geom_vline(aes(xintercept = mean(Participatory, na.rm = T))) +
  theme_minimal() +
  labs(title = "Figura 11",
       subtitle = "Gráfico de dispersão OHI - Participatory", 
       caption = "Elaboração própria", 
       fig.align = "center")
```
Em relação a dimensão participatória, também há uma tendência de maiores scores no OHI, virem acompanhados de maiores scores no participatório. Há algumas excessões a esse padrão, mas iremos analisar mais aprofundadamente ao decorrer do artigo.

```{r}
ggplot(banco_final, aes(Liberal, OHI)) +
  geom_jitter() +
  geom_vline(aes(xintercept = mean(Liberal, na.rm = T))) +
  theme_minimal() +
  labs(title = "Figura 12",
       subtitle = "Gráfico de dispersão OHI - Liberal", 
       caption = "Elaboração própria", 
       fig.align = "center")
```
Como podemos ver, a tendência é contínua para todas as dimensões da democracia, com algumas excessões de casos que iremos analisar posteriormente em nosso artigo. Os gráficos bivariados das outras dimensões estão no apêndice.  

```{r}

ggplot(banco_final, aes(IDH, OHI)) +
  geom_jitter() +
  theme_minimal() +
  labs(title = "Figura 15",
       subtitle = "Gráfico de dispersão OHI - IDH", 
       caption = "Elaboração própria", 
       fig.align = "center")
```
Para a relação entre OHI e IDH, vemos que os casos se assemelham bastante com o comportamento dos casos de uma reta de regressão. 


```{r}
vdem_dimensions_last_year <- banco_final2 %>%
  filter(Year == max(Year)) %>%
  spread(Dimension, score) %>%
  select(-Country, -iso3c, -COWcode, -Year)

vdem_dimensions_last_year

ggpairs(vdem_dimensions_last_year) +
  labs(title = "Figura 17",
       subtitle = "Gráfico GGPairs OHI/V-dem ano", 
       caption = "Elaboração própria", 
       fig.align = "center")

```
O comando ggpairs é interessante pois ele cria uma matriz de plots. Temos 6 density plots (cada um representando uma variável do nosso modelo). Também temos alguns scatter plots, o primeiro scatterplot é o IDH em relação ao OHI, o IDH com a dimensão deliberativa e consecutivamente. O último é a dimensão liberal com a dimensão participatória. Também temos o coeficiente de correlação, o primeiro coeficiente de correlação corresponde ao IDH. Podemos perceber que temos uma grande correlação entre todas as variáveis (***). 


## 3.2 Gráficos Multivariados

```{r}
ggplot(banco_final, aes(Electoral, OHI, color = IDH, fill = IDH)) +
  geom_jitter(size = 3) +
  theme_minimal() +
  labs(title = "Figura 18",
       subtitle = "Gráfico Electoral/OHI por IDH", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(banco_final, aes(Liberal, OHI, color = IDH, fill = IDH)) +
  geom_jitter(size = 3) +
  theme_minimal() +
  labs(title = "Figura 19",
       subtitle = "Gráfico Liberal/OHI por IDH", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(banco_final, aes(Participatory, OHI, color = IDH, fill = IDH)) +
  geom_jitter(size = 3) +
  theme_minimal()+
  labs(title = "Figura 20",
       subtitle = "Gráfico Participatory/OHI por IDH", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(banco_final, aes(Deliberative, OHI, color = IDH, fill = IDH)) +
  geom_jitter(size = 3) +
  theme_minimal() +
  labs(title = "Figura 21",
       subtitle = "Gráfico Deliberative/OHI por IDH", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(banco_final, aes(Egalitarian, OHI, color = IDH, fill = IDH)) +
  geom_jitter(size = 3) +
  theme_minimal() +
  labs(title = "Figura 22",
       subtitle = "Gráfico Egalitarian/OHI por IDH", 
       caption = "Elaboração própria", 
       fig.align = "center")

graficos_multivariados <- banco_final2 %>%
  filter(Year == max(Year)) %>%
  select(-Country, -iso3c, -COWcode, -Year)

ggplot(graficos_multivariados, aes(score, OHI, color = Dimension)) +
  geom_jitter(size = 3) +
  theme_minimal() +
  labs(title = "Figura 23",
       subtitle = "Gráfico OHI/score por Dimension", 
       caption = "Elaboração própria", 
       fig.align = "center")

```

Nos gráficos multivariados, analisamos as dimensões da democracia com o score do OHI, destacando o IDH. Ressalto que os países que pontuam bem na dimensão democrática e no IDH se concentram no quadrante superior direito, com algumas excessões a esse padrão. Os gráficos multivariados reforçam nossa hipótese de que um bom score nas dimensões democráticas, causam um bom score na dimensão ambiental. A hipótese sugerida pelos formuladores do Ocean Health Index de que há correlação na pontuação do IDH com OHI é reforçada visualmente em nossos gráficos. Os casos com uma boa pontuação na dimensão democrática, juntamente com uma boa pontuação no score de desenvolvimento humano, se concentram no eixo superior direito. Em outras palavras, pelo menos graficamente, a hipótese de que uma boa qualidade democrática levaria a um bom desempenho ambiental é válida. Por fim, agrupamos o OHI pelo score e por dimensões, vemos que a dimensão eleitoral (verde) concentra a maior quantidade de casos no setor superior direito. Neste último gráfico há mais casos, pois avaliamos os scores das dimensões da Democracia separadamente. Ou seja, cada país está representado em 5 pontos distintos (um pra cada dimensão), entretanto, apesar de reconhecermos um problema potencial em repetir os casos, esse exercício nos pareceu frutífero, pois pudemos identificar quais dimensões tem uma maior relevância para um bom score na dimensão ambiental.

## 3.3 Testes de hipótese bivariada
```{r}
# Dimensão Electoral
cor.test(banco_final$OHI, banco_final$Electoral)

regressao <- lm(OHI ~ Electoral, data = banco_final)
summary(regressao)
confint(regressao, level = 0.95)


#verificando pressupostos (0)

#lineariedade 
plot(regressao, 1)

#Homocedasticidade 
plot(regressao, 3)
bptest(regressao)
ncvTest(regressao)

#autocorrelação entre casos/resíduos
acf(regressao$residuals)

#Normalidade dos resíduos
plot(regressao, 2)

library(MASS)
sresid <- studres(regressao) 
shapiro.test(sresid)

#Outliers
plot(regressao, 4)
plot(regressao, 5)

ggplot(banco_final, aes(Electoral, OHI)) +
  geom_point() +
  geom_smooth(method = "lm")
```
Em relação ao primeiro teste de associação (Ocean Health Index e a dimensão Eleitoral), houve um p-valor pequeno, mostrando uma boa associação entre as variáveis. O intervalo de confiança não inclui o zero, mostrando que há uma relevância estatística. Em relação a regressão, houve significância estatística entre o OHI e a dimensão eleitoral. Os R² foram pequenos, mas temos que levar em consideração que nosso modelo é bivariável (há outras variáveis que podem influenciar o modelo). O p-valor foi bem pequeno. O intervalo de confiança não contém o 0. O coeficiente é positivo em 12.368. Isso significa que a variação em 1 unidade no OHI, levaria a uma variação em 12 unidades na dimensão eleitoral. O erro padrão foi de 2.87. O erro padrão é uma estatística de desvio da distribuição amostral. 

O pressuposto da linearidade não foi amplamente satisfeito. O pressuposto da linearidade trata sobre uma variação linear -ou proporcional- das variáveis. Na  estimação  do  modelo,  a  linearidade  implica  que  o aumento   de   uma   unidade   em X1 gera   o   mesmo  efeito   sobre   Y, independente dovalor inicial de X1 (Wooldridge, 2009). No nosso exemplo a variação de uma unidade na variável independente não leva a variação em uma unidade na variável dependente. A violação desse pressuposto impede que a estimação por mínimos quadrados ordinários produza o melhor estimador linear não-viesado. 

O pressuposto da Homocedasticidade foi satisfeito. Para o pressuposto da homocedasticidade ser satisfeito, o importante é a distribuição dos pontos. Eles devem ser uniformes ao logo de ambos os eixos. Homoscedasticidade é satisfeita se todas as variáveis do modelo tiverem a mesma variância finita, também é conhecido como homogeneidade de variância. Também fizemos o bptest e o ncvTest para avaliar a homocedasticidade. O p-valor de ambos os testes não foram tão baixos. A hipótese nula é a presença de homocedasticidade, ou seja, que o pressuposto está satisfeito. Dessa forma, o resultado desejado é um p-valor alto, onde não podemos rejeitar a hipótese nula. Logo, em nosso modelo há a presença de homocedasticidade. 

Nosso modelo mostrou ter alguns problemas no teste de autocorrelação entre casos/resíduos. As linhas verticais passaram do intervalo delimitado pelas linhas azuis tracejadas. Isso significa que em nossa regressão alguns resíduos foram depedentes (correlacionados) uns dos outros. Se a suposição de independência for violada, alguns resultados de ajuste do modelo podem não ser confiáveis. Isso pode ser causado por uma correlação temporal ou espacial, mas também pode ser causado por outros motivos. Um erro muito comum é o erro sistemático de codificação do banco de dados. Esse pressuposto é muito negativo para o nosso modelo, pois trata essencialmente sobre uma incosistência nos testes de significância, assim como ocorre quando o pressuposto da homocedasticidade é violado.

Nosso teste de normalidade dos resíduos foi positivo. O termo de erro deve seguir uma distribuição aproximadamente normal para que as estimativas do modelo sejam consistentes (Figueiredo, 2019). Os pontos de nossos teste estão majoritariamente próximos da diagonal (uma distribuição normal. O nosso p-valor foi alto, assim não pudemos rejeitar a hipótese nula para a normalidade. A normalidade diz sobre a consistência das estimativas do nosso modelo, se eles não são consistentes, não podemos confiar nas estimativas do modelo. Uma coisa que pode afetar a normalidade dos resíduos, por exemplo, são os outliers ou problemas de heterocesdasticidade. 
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          No teste de outliers, o modelo identificou 3 outliers. O caso 27 (Árabia Saudita), 28 (Emirados Árabes) e 31 (Qatar). Esses países tiveram um alto score no OHI, mas péssima pontuação na dimensão eleitoral. Quanto maior as retas verticais, maior é a contribuição dos mesmos no enviesamento das estimativas. A distância da reta vermelha no segundo teste, por sua vez, é proporcional às distâncias de Cook, que indicam o impacto que nossos estimadores sofrem quando um determinado caso é excluído da amostra analisada.
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          Os pontos não ficaram tão próximos da reta de regressão, sugerindo que pode haver muita diferença entre os casos (já que a reta corresponde à menor distância possível entre cada ponto plotado e a reta). 
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ```{r}
                                                                                                                                                                                                                                                                          # Dimensão Deliberative (1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          cor.test(banco_final$OHI, banco_final$Deliberative)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          regressao1 <- lm(OHI ~ Deliberative, data = banco_final)
                                                                                                                                                                                                                                                                          summary(regressao1)
                                                                                                                                                                                                                                                                          confint(regressao1, level = 0.95)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #verificando pressupostos (1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #lineariedade 
                                                                                                                                                                                                                                                                          plot(regressao1, 1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Homocedasticidade 
                                                                                                                                                                                                                                                                          plot(regressao1, 3)
                                                                                                                                                                                                                                                                          bptest(regressao1)
                                                                                                                                                                                                                                                                          ncvTest(regressao1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #autocorrelação entre casos/resíduos
                                                                                                                                                                                                                                                                          acf(regressao1$residuals)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Normalidade dos resíduos
                                                                                                                                                                                                                                                                          plot(regressao1, 2)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Outliers
                                                                                                                                                                                                                                                                          plot(regressao1, 4)
                                                                                                                                                                                                                                                                          plot(regressao1, 5)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ggplot(banco_final, aes(Deliberative, OHI)) +
                                                                                                                                                                                                                                                                            geom_point() +
                                                                                                                                                                                                                                                                            geom_smooth(method = "lm")
                                                                                                                                                                                                                                                                          ```
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          Com a dimensão deliberativa não houve muita diferença da dimensão eleitoral. Houve relevância estatística, devido ao baixo p-valor (***) e o intervalo de confiança não conter o 0. Não houve grandes mudanças nos R² e no coeficiente. A magnitude da autocorrelação foi menor, fornecendo mais consistência aos testes de significância. Um outlier mudou: Nicarágua. Para acessar os dados das demais dimensões veja o apêndice.
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ```{r}
                                                                                                                                                                                                                                                                          #Dimensão Egalitarian (4)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          cor.test(banco_final$OHI, banco_final$Egalitarian)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          regressao4 <- lm(OHI ~ Egalitarian, data = banco_final)
                                                                                                                                                                                                                                                                          summary(regressao4)
                                                                                                                                                                                                                                                                          confint(regressao4, level = 0.95)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #verificando pressupostos (4)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #lineariedade 
                                                                                                                                                                                                                                                                          plot(regressao4, 1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Homocedasticidade 
                                                                                                                                                                                                                                                                          plot(regressao4, 3)
                                                                                                                                                                                                                                                                          bptest(regressao4)
                                                                                                                                                                                                                                                                          ncvTest(regressao4)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #autocorrelação entre casos/resíduos
                                                                                                                                                                                                                                                                          acf(regressao4$residuals)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Normalidade dos resíduos
                                                                                                                                                                                                                                                                          plot(regressao4, 2)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Outliers
                                                                                                                                                                                                                                                                          plot(regressao4, 4)
                                                                                                                                                                                                                                                                          plot(regressao4, 5)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ggplot(banco_final, aes(Egalitarian, OHI)) +
                                                                                                                                                                                                                                                                            geom_point() +
                                                                                                                                                                                                                                                                            geom_smooth(method = "lm")
                                                                                                                                                                                                                                                                          ```
                                                                                                                                                                                                                                                                          Nessa dimensão, um caso específico (112) teve um grande impacto no problema de linearidade e de autocorrelação entre os resíduos. Os pontos apareceram melhor ajustado a reta da regressão.
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ```{r}
                                                                                                                                                                                                                                                                          #IDH (5)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          cor.test(banco_final$OHI, banco_final$IDH)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          regressao5 <- lm(OHI ~ IDH, data = banco_final)
                                                                                                                                                                                                                                                                          summary(regressao5)
                                                                                                                                                                                                                                                                          confint(regressao5, level = 0.95)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #verificando pressupostos (5)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #lineariedade 
                                                                                                                                                                                                                                                                          plot(regressao5, 1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Homocedasticidade 
                                                                                                                                                                                                                                                                          plot(regressao5, 3)
                                                                                                                                                                                                                                                                          bptest(regressao5)
                                                                                                                                                                                                                                                                          ncvTest(regressao5)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #autocorrelação entre casos/resíduos
                                                                                                                                                                                                                                                                          acf(regressao5$residuals)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Normalidade dos resíduos
                                                                                                                                                                                                                                                                          plot(regressao5, 2)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Outliers
                                                                                                                                                                                                                                                                          plot(regressao5, 4)
                                                                                                                                                                                                                                                                          plot(regressao5, 5)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ggplot(banco_final, aes(IDH, OHI)) +
                                                                                                                                                                                                                                                                            geom_point() +
                                                                                                                                                                                                                                                                            geom_smooth(method = "lm")
                                                                                                                                                                                                                                                                          ```
                                                                                                                                                                                                                                                                          Por fim, o IDH também foi estatísticamente relevante. O p-valor foi pequeno, mas o valor do coeficiente foi positiva e alta (comparando com as dimensões da democracia). Os R² tiveram um valor maior (0.50), sugerindo que o IDH explica melhor o modelo do que as dimensões da Democracia. O coeficiente de intervalo não contém o 0.
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ## 3.4 Regressões Multivariadas
                                                                                                                                                                                                                                                                          ```{r}
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          regressao_multivariada1 <- lm(OHI ~ Electoral + Liberal + Participatory + Deliberative + Egalitarian + IDH, data = banco_final)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          summary(regressao_multivariada1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          dwplot(regressao_multivariada1, vline = geom_vline(xintercept = 0 , colour = "blue", linetype = 2))
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #verificando pressupostos (1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #lineariedade 
                                                                                                                                                                                                                                                                          plot(regressao_multivariada1, 1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Homocedasticidade 
                                                                                                                                                                                                                                                                          plot(regressao_multivariada1, 3)
                                                                                                                                                                                                                                                                          bptest(regressao_multivariada1)
                                                                                                                                                                                                                                                                          ncvTest(regressao_multivariada1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #autocorrelação entre casos/resíduos
                                                                                                                                                                                                                                                                          acf(regressao_multivariada1$residuals)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Normalidade dos resíduos
                                                                                                                                                                                                                                                                          plot(regressao_multivariada1, 2)
                                                                                                                                                                                                                                                                          library(MASS)
                                                                                                                                                                                                                                                                          sresid1 <- studres(regressao_multivariada1)
                                                                                                                                                                                                                                                                          shapiro.test(sresid1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Multicolinearidade
                                                                                                                                                                                                                                                                          vif(regressao_multivariada1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Outliers
                                                                                                                                                                                                                                                                          plot(regressao_multivariada1, 4)
                                                                                                                                                                                                                                                                          plot(regressao_multivariada1, 5)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ```
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          Os resultados de nossa regressão multivariada foram bem interessantes. As dimensões de Democracia que tiveram alguma relevância estatística na regressão multivariada foram: Eleitoral (p-valor = 0.00342) e Liberal (p-valor = 0.02437). Antes, quando fizemos as regressões bivariadas, todas as dimensões tiveram relevância estatística. Vale a pena destacar que a dimensão Eleitoral teve uma relação negativa com a dimensão ambiental (-42), ou seja, você ser mais eleitoral prejudicaria o meio-ambiente; a dimensão Liberal teve uma relação positiva (29); o IDH também teve uma relação positiva (coef = 27.35, p-valor = < 0.005), confirmando a correlação entre o IDH e o OHI. Os r² foram bons (0.5), sugerindo que o modelo tem algum potencial explicativo. A mediana foi -0.17, localizada no 3º quartil (o ideal de uma distribuição normal dos resíduos é meidana 0, eles devem seguir o mesmo padrão da distribuição dos dados normais). 
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          O nosso gráfico dwplot também representa o que identificamos na regressão. As únicas variáveis que não tiveram o 0 em seu intervalo de confiança foram: Eleitoral, Liberal e IDH. 
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          O primeiro pressuposto que iremos trabalhar é a linearidade dos parâmetros. Deve-se esperar que a relação entre a variável dependente e as variáveis independentes seja representada por uma função linear. Quanto mais a relação se distanciar de uma função linear, maior é a diferença entre os paramêtros estimados e os observados (Figueiredo, et. 2011). Em relação ao nosso modelo, a linha vermelha se aproximou da linha pontilhada que cruza o eixo y no valor 0. O ideal é que a linha vermelha esteja sobreposta a linha pontihada, entretanto o desvio não foi tão alarmante assim.
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          A homocedasticidade foi o segundo pressuposto testado em nosso modelo multivariado. Para que haja homocedasticidade é necessário homogeneidade da variância. Em outras palavras, a diferença entre os resultados observados e os resultados preditos pelo modelo devem variar uniformemente. Se a medida que o valor de Y aumenta, os erros de predição também aumentam, tem-se heterogeneidade na variância, ou seja, heterocedasticidade (Figueiredo, et al. 2011). Em nosso modelo, a distribuição dos pontos foi uniforme para cima e para baixo e também ao longo de todo eixo X. Também fizemos testes formais para verificar esse pressuposto. O nosso p-valor foi alto; ou seja, não podemos rejeitar a hipótese nula de que há presença de homocedasticidade em nosso modelo. 
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          O terceiro teste foi da ausência de autocorrelação entre casos/resíduos. Esse pressuposto se refere à situação em que o valor de uma observação medida em determinado período (t1) não influencia o valor de uma observação medida em um momento t2. Significa dizer que as observações são independentes, ou seja, não existe correlação entre os termos do erro. Em nosso modelo, a ausência de autocorrelação foi satisfeita. Todas as linhas verticais ficaram dentro do intervalo das linhas horizontais azuis. Esse pressuposto tem como intuito aumentar a confiabilidade dos testes de significância e intervalos de confiança (Figueiredo, et al. 2011).
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          O quarto teste foi da normalidade dos resíduos. O método de MQO opera melhor quando os erros são normalmente distribuídos. Erros assimétricos podem comprometer a eficiência dos MQO, tendo em vista que o MQO também é um método de previsão de erros. Em nosso modelo, os pontos ficaram próximos da linha diagonal. O nosso p-valor foi alto, onde não podemos rejeitar a hipótese nula da normalidade dos resíduos. Dessa forma, esse pressuposto sendo satisfeito ele aumenta a certeza quanto a razoabilidade de se estimar a média condicional de y a partir de x. 
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          O pressuposto da multicolinearidade trata da magnitude da variância dos parâmetros estimados. A presença de altos níveis de correlação entre as variáveis independentes impossibilita estimar, com precisão, o efeito de cada variável sobre a variável dependente (Figueiredo, et al. 2011). Em nosso teste, o valor mínimo, quando uma variável não possui multicolinearidade, é 1. Quanto maior o valor, maior a multicolinearidade. Para o nosso modelo, as dimensões de democracia e o IDH tiveram uma multicolinearidade alta. Eis  a essência  do problema:  quanto  maior  a  correlação  entre  as  variáveis independentes, menos   informação   estará   disponível   para   estimar   os coeficientes associados  às  variáveis  explicativas.  Para  Kennedy  (2009), "qualquer estimativa  baseada  em  pouca  informação  não  pode  ser  realizada  com muita confiança -ela terá uma alta variância" (Kennedy, 2009: 194).
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          Por fim, testamos os outliers. Nosso modelo multivariado identificou 3 casos como outliers: 45 (Seychelles), 64 (República Dominicana), 83 (Nicaragua). O caso que teve uma maior influência em nosso teste foi o de Seychelles. Iremos testar o modelo sem nenhum dos outliers indicados. Testamos brevemente a seguir se houve alguma mudança estatística removendo os outliers:
                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                            ## Aplicando sem os outliers
                                                                                                                                                                                                                                                                            ```{r}
                                                                                                                                                                                                                                                                          banco_sem_outlier <- banco_final %>%
                                                                                                                                                                                                                                                                            filter(Country != "Seychelles") %>%
                                                                                                                                                                                                                                                                            filter(Country != "Nicaragua") %>%
                                                                                                                                                                                                                                                                            filter(Country != "Dominican Republic")
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          regressao_multivariada_sem_outlier <- lm(OHI ~ Electoral + Liberal + Participatory + Deliberative + Egalitarian + IDH, data = banco_sem_outlier)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          summary(regressao_multivariada_sem_outlier)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #verificando pressupostos (1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #lineariedade 
                                                                                                                                                                                                                                                                          plot(regressao_multivariada_sem_outlier, 1)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Homocedasticidade 
                                                                                                                                                                                                                                                                          bptest(regressao_multivariada_sem_outlier)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #autocorrelação entre casos/resíduos
                                                                                                                                                                                                                                                                          acf(regressao_multivariada_sem_outlier$residuals)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Normalidade dos resíduos
                                                                                                                                                                                                                                                                          plot(regressao_multivariada_sem_outlier, 2)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Multicolinearidade
                                                                                                                                                                                                                                                                          vif(regressao_multivariada_sem_outlier)
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          #Outliers
                                                                                                                                                                                                                                                                          plot(regressao_multivariada_sem_outlier, 4)
                                                                                                                                                                                                                                                                          plot(regressao_multivariada_sem_outlier, 5)
                                                                                                                                                                                                                                                                          ```
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          Destacamos que houve algumas mudanças relevantes em nosso modelo. A dimensão participatória, que antes não havia tido relevância estatística, começou a ter. O p-valor da dimensão eleitoral ficou ainda menor e a magnitude do coeficiente ainda maior (negativo em -52). Os R² aumentaram (0.65), comparando com o modelo com os outliers. A dimensão participatória que antes tinha um p-valor de 0.75, agora está com um p-valor de 0.001 e a magnitude do coeficente aumentou. Em relação aos testes de pressupostos, a multicolinearidade continuou existindo, surgiram novos outliers, ainda houve normalidade dos resíduos, houve pouco autocorrelação entre os resíduos e a homocedasticidade contínuou presente. 
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          ## Conclusões
                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                          Neste artigo, buscamos testar a hipótese de que qualidade democrática impacta o desempenho ambiental. Teoricamente, o baixo desempenho de indicadores 
                                                                                                                                                                                                                                                                          de governabilidade democrática resulta em menor proteção político-institucional dos recursos naturais bióticos e abióticos, renováveis e não renováveis. Ainda que a nossa
                                                                                                                                                                                                                                                                          análise aponte ao efeito positivo do nível de democracia sobre o desempenho ambiental, chama-nos atenção a reduzida possibilidade de reflexão de  problemas ambientais locais em suas múltiplas dimensões e definições. 
                                                                                                                                                                                                                                                                          As relações entre dimensões políticas, variáveis socioeconômicas e desempenho ambiental são multifacetadas (Rydén, O., Zizka, A., Jagers, S. C., et al. 2019), além de provavelmente também serem multidirecionais. Acreditamos que os indicadores ambientais ainda carecem de uma operacionalização ampla e sistêmica do que se conceitua como Meio Ambiente. De outro lado, as crescentes preocupações sociais com a proteção ambiental provavelmente afetarão os processos políticos e a coleta de dados ambientais. Em geral, uma interpretação causal dos padrões observados nos pareceu uma tarefa árdua, devido a mecanismos indiretos ou obscuros e ao grande número de fatores potencialmente confounding. Ainda que tenhamos percebido essas adversidades, consideramos que a democracia exerce um efeito positivo e significativo sobre a proteção dos recursos 
                                                                                                                                                                                                                                                                          naturais. 
                                                                                                                                                                                                                                                                          Este trabalho dá um passo a mais na literatura existente ao identificar as dimensões democráticas que teriam um maior impacto na conservação ambiental: I) Eleitoral; II) Liberal; III) Participatória. Destacamos o impacto negativo da dimensão eleitoral na conservação ambiental. Esse ponto específico entra em confronto com os argumentos de que as democracias geralmente exibem melhor desempenho ambiental do que não-democracias ou autocracias (Fiorino, 2018; Li & Reuveny, 2006), devido a maior pluralismo, ativismo da sociedade civil, instituições mais fortes e responsabilidade eleitoral em sociedades democráticas que as tornam mais abertas às demandas populares para o fornecimento de bens públicos (Winslow, 2005; ver também Duit, Feindt, & Meadowcroft, 2016). Vale a pena destacar que os argumentos que apóiam a reconciliação entre a democracia e a proteção ambiental há muito vêm recebendo críticas vigorosas. A democracia é percebida como muito lenta, comprometedora, complicada e capturada por grupos de interesse e veto players para gerar a mudança transformadora necessária para a sustentabilidade (Hardin, 1968; Ophuls, 1977). Blühdorn (2013, p. 29) afirma que com a maior ênfase que as sociedades modernas tardias colocam na liberdade individual, "mais democracia" - entendida em termos de maior capacidade de resposta às demandas dos cidadãos - "pode muito bem implicar ainda menos sustentabilidade".
Por fim, identificamos um mecanismo causal convincente que conecta X a Y. Além da ampla literatura existente, verificamos que há uma correlação positiva, moderada e significativa entre as dimensões da Democracia e o score ambiental. A questão que surge não é se existe um mecanismo causal convincente que conecte democracia com sustentabilidade, mas sim, qual conjunto de variáveis independetes operacionalizam de forma sistemática um modelo que é essencialmente multidimensional e multidirecional.     Para nós, ficou claro que a direção do mecanismo causal é de X para Y, ou seja, a democracia que impactaria o Meio Ambiente, não o contrário. Justificamos isso a partir dos argumentos presentes na literatura. A democracia forneceria robustez institucional, arenas de participação, monitoramento dos atores, necessários a um melhor desempenho ambiental. 
Identificamos covariação entre X e Y. Como vimos em nossa regressão, há uma associação entre a nossa VD e as VIs. Destacamos que nossos testes buscam uma covariação linear, então a covariação não-linear pode não ser muito bem identificada ela correlação de Pearson. Afirmamos que há covariação, mas questionamos a natureza desta.
Em nosso modelo, utilizamos como variável de controle o IDH. Justificamos nossa escolha devido a sua grande relevância na literatura específica do tema. Entretanto, reconhemos que há outras variáveis que podem influenciar o modelo, por exemplo: Índice de Gini, taxa de urbanização, capital cívico, entre outros. Como agenda de pesquisa sugerimos que outras variáveis de controle sejam inseridas no modelo, assim como outras dimensões ambientais também.  

## Referências bibliográficas 

Blühdorn, I. (2013). The governance of unsustainability: Ecology and democracy after the post-democratic turn.EnvironmentalPolitics,22(1), 16-36.

Coppedge, Michael., John Gerring., Staffan I Lindber.g, Svend-Erik Skaaning., Jan Teorell., Joshua Krusell., Kyle L Marquardt., et al. 2019. "V-Dem Methodology V9."

Duit, A., Feindt, P. H., & Meadowcroft, J. (2016). Greening Leviathan: The rise of the environmental state?Environmental Politics,25(1), 1-23.

Figueiredo, D.; Nunes, F; Rocha, E. et al. O que Fazer e o que Não Fazercom a Regressão: pressupostos e aplicações do modelo linear de Mínimos Quadrados Ordinários (MQO). Revista Política Hoje, Vol. 20, n.1, 2011.

Fiorino, D. J. (2018).Can democracy handle climate change?Cambridge: Polity

Halpern, B. S., Longo, C., Hardy, D., McLeod, K. L., Samhouri, J. F., Katona, S. K., . Zeller, D. (2012). An index to assess the health and benefits of the global. Revista Nature. Vol 000. 2012. 

Hardin, G. (1968). The tragedy of the commons.Science,162(3859), 1243-1248

Jonathan Pickering, Karin Bäckstrand & David Schlosberg (2020)Between environmental and ecological democracy: theory and practice at the democracy-environment nexus, Journal of Environmental Policy & Planning, 22:1, 1-15, DOI:10.1080/1523908X.2020.1703276

Lindberg, S. I., Coppedge, M., Gerring, J., & Teorell, J. (2014). V-Dem: A New Way to Measure Democracy. Journal of Democracy, 25(3), 159-169.

Li, Q., & Reuveny, R. (2006). Democracy and environmental degradation.International Studies Quarterly,50(4), 935-956

Lijphart, A. Comparative politics and the comparative method. American Political Science Review, 65, 6682-6693, 1971

Mitchel, R.; Bernauer, T. Empirical Research in International Environmental Policy: design qualitative case studies. Journal of Environmental & Development, 7(1), 4-31, 1998. Disponível em: <http://web.stanford.edu/group/IDL/102/lectures/Read-ings/L2R1.pdf>Ophuls, W. (1977).Ecology and the politics of scarcity. San Francisco, CA: WH Freeman

Rodrigues, D.; Silva Júnior, J.; Silva, D. et al. A sustentável leveza da democracia? Os efeitos da qualidade democrática sobre o desempenho ambiental. Revista Desenvolvimento e Meio Ambiente, Vol. 33, Abril, 2015.

Rydén, O., Zizka, A., Jagers, S. C., Lindberg, S. I. & Antonelli, A. Linking democracyand biodiversity conservation: Empirical evidence and research gaps.Ambio15pp (2019).

Winslow, M. (2005). Is democracy good for the environment?Journal of Environmental Planning and Management,48(5), 771-783.


## Apêndice 

```{r}

ggplot(banco_final, aes(Deliberative, OHI)) +
  geom_jitter() +
  geom_vline(aes(xintercept = mean(Deliberative, na.rm = T))) +
  theme_minimal() +
  labs(title = "Figura 13",
       subtitle = "Gráfico de dispersão OHI - Deliberative", 
       caption = "Elaboração própria", 
       fig.align = "center")

ggplot(banco_final, aes(Electoral, OHI)) +
  geom_jitter() +
  geom_vline(aes(xintercept = mean(Electoral, na.rm = T))) +
  theme_minimal() +
  labs(title = "Figura 14",
       subtitle = "Gráfico de dispersão OHI - Electoral", 
       caption = "Elaboração própria", 
       fig.align = "center")

#Dimensão Liberal (2)

cor.test(banco_final$OHI, banco_final$Liberal)

regressao2 <- lm(OHI ~ Liberal, data = banco_final)
summary(regressao2)
confint(regressao2, level = 0.95)

#verificando pressupostos (2)

#lineariedade 
plot(regressao2, 1)

#Homocedasticidade 
plot(regressao2, 3)
bptest(regressao2)
ncvTest(regressao2)

#autocorrelação entre casos/resíduos
acf(regressao2$residuals)

#Normalidade dos resíduos
plot(regressao2, 2)


#Outliers
plot(regressao2, 4)
plot(regressao2, 5)

ggplot(banco_final, aes(OHI, Liberal)) +
  geom_point() +
  geom_smooth(method = "lm")
```


```{r}
#Dimensão Participatory (3)

cor.test(banco_final$OHI, banco_final$Participatory)

regressao3 <- lm(OHI ~ Participatory, data = banco_final)
summary(regressao3)
confint(regressao3, level = 0.95)

#verificando pressupostos (3)

#lineariedade 
plot(regressao3, 1)

#Homocedasticidade 
plot(regressao3, 3)
bptest(regressao3)
ncvTest(regressao3)

#autocorrelação entre casos/resíduos
acf(regressao3$residuals)

#Normalidade dos resíduos
plot(regressao3, 2)


#Outliers
plot(regressao3, 4)
plot(regressao3, 5)

ggplot(banco_final, aes(OHI, Participatory)) +
  geom_point() +
  geom_smooth(method = "lm")
```

Apesar de ter apresentado um novo outlier (Seychelles), essa dimensão também foi semelhante as outras.

```