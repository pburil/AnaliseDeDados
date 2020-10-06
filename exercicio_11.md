Exercicio 11
================

``` r
library(tidyverse)
library(haven)

link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
  mutate(D10 = as_factor(D10)) %>%
  filter(Q1607 < 11, 
         Q18 < 11,
         D9 < 9999998,
         Q1501 < 11)


Outras <- levels(banco$D10)[-c(3,5,13)]

banco <- banco %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião"))
```

### Faça uma regressão linear avaliando em que medida as variáveis independentes utilizadas nos exercícios 7 e 8, idade(D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT (Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião (variável criada no passo anterior) explicam a avaliação de Bolsonaro (Q1607)

``` r
regressao <- lm(Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco)

summary(regressao)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0608 -2.5654  0.4179  2.3268  8.9954 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               6.216e+00  5.365e-01  11.586  < 2e-16 ***
    ## D1A_ID                    1.040e-02  6.234e-03   1.669 0.095376 .  
    ## D3_ESCOLA                -1.116e-01  4.486e-02  -2.487 0.012982 *  
    ## D9                       -3.620e-05  2.764e-05  -1.309 0.190576    
    ## Q1501                    -3.946e-01  2.367e-02 -16.670  < 2e-16 ***
    ## Q18                       3.161e-01  2.603e-02  12.142  < 2e-16 ***
    ## D2_SEXO                  -6.874e-01  1.746e-01  -3.937 8.63e-05 ***
    ## religiaoEvangélica        6.685e-01  1.984e-01   3.370 0.000772 ***
    ## religiaoNão tem religião -7.565e-02  3.485e-01  -0.217 0.828177    
    ## religiaoOutras           -8.326e-01  3.081e-01  -2.702 0.006963 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.296 on 1452 degrees of freedom
    ## Multiple R-squared:  0.3018, Adjusted R-squared:  0.2975 
    ## F-statistic: 69.75 on 9 and 1452 DF,  p-value: < 2.2e-16

### Faça o teste de homoscedasticidade do modelo e corrija as estimações dos coeficientes caso seja necessário.

``` r
library(car)
library(lmtest)
library(sandwich)

plot(regressao, 3)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
plot(regressao, 1)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ncvTest(regressao)
```

    ## Non-constant Variance Score Test 
    ## Variance formula: ~ fitted.values 
    ## Chisquare = 22.48512, Df = 1, p = 2.1178e-06

``` r
bptest(regressao)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  regressao
    ## BP = 65.763, df = 9, p-value = 1.025e-10

``` r
coeftest(regressao, vcov = vcovHC(regressao, type = "HC"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                             Estimate  Std. Error  t value  Pr(>|t|)    
    ## (Intercept)               6.2160e+00  5.4272e-01  11.4536 < 2.2e-16 ***
    ## D1A_ID                    1.0403e-02  6.1863e-03   1.6817 0.0928470 .  
    ## D3_ESCOLA                -1.1159e-01  4.6455e-02  -2.4022 0.0164233 *  
    ## D9                       -3.6198e-05  3.1584e-05  -1.1461 0.2519566    
    ## Q1501                    -3.9464e-01  2.6132e-02 -15.1019 < 2.2e-16 ***
    ## Q18                       3.1608e-01  2.8296e-02  11.1704 < 2.2e-16 ***
    ## D2_SEXO                  -6.8736e-01  1.7793e-01  -3.8631 0.0001169 ***
    ## religiaoEvangélica        6.6854e-01  1.9541e-01   3.4213 0.0006406 ***
    ## religiaoNão tem religião -7.5647e-02  3.7006e-01  -0.2044 0.8380534    
    ## religiaoOutras           -8.3256e-01  3.0275e-01  -2.7500 0.0060340 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(regressao, vcov = vcovHC(regressao, type = "HC0"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                             Estimate  Std. Error  t value  Pr(>|t|)    
    ## (Intercept)               6.2160e+00  5.4272e-01  11.4536 < 2.2e-16 ***
    ## D1A_ID                    1.0403e-02  6.1863e-03   1.6817 0.0928470 .  
    ## D3_ESCOLA                -1.1159e-01  4.6455e-02  -2.4022 0.0164233 *  
    ## D9                       -3.6198e-05  3.1584e-05  -1.1461 0.2519566    
    ## Q1501                    -3.9464e-01  2.6132e-02 -15.1019 < 2.2e-16 ***
    ## Q18                       3.1608e-01  2.8296e-02  11.1704 < 2.2e-16 ***
    ## D2_SEXO                  -6.8736e-01  1.7793e-01  -3.8631 0.0001169 ***
    ## religiaoEvangélica        6.6854e-01  1.9541e-01   3.4213 0.0006406 ***
    ## religiaoNão tem religião -7.5647e-02  3.7006e-01  -0.2044 0.8380534    
    ## religiaoOutras           -8.3256e-01  3.0275e-01  -2.7500 0.0060340 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(regressao, vcov = vcovHC(regressao, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                             Estimate  Std. Error  t value  Pr(>|t|)    
    ## (Intercept)               6.2160e+00  5.4458e-01  11.4143 < 2.2e-16 ***
    ## D1A_ID                    1.0403e-02  6.2075e-03   1.6759 0.0939704 .  
    ## D3_ESCOLA                -1.1159e-01  4.6615e-02  -2.3939 0.0167949 *  
    ## D9                       -3.6198e-05  3.1693e-05  -1.1421 0.2535842    
    ## Q1501                    -3.9464e-01  2.6221e-02 -15.0501 < 2.2e-16 ***
    ## Q18                       3.1608e-01  2.8394e-02  11.1321 < 2.2e-16 ***
    ## D2_SEXO                  -6.8736e-01  1.7854e-01  -3.8499 0.0001233 ***
    ## religiaoEvangélica        6.6854e-01  1.9608e-01   3.4095 0.0006686 ***
    ## religiaoNão tem religião -7.5647e-02  3.7133e-01  -0.2037 0.8386006    
    ## religiaoOutras           -8.3256e-01  3.0379e-01  -2.7406 0.0062088 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(regressao, vcov = vcovHC(regressao, type = "HC2"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                             Estimate  Std. Error  t value  Pr(>|t|)    
    ## (Intercept)               6.2160e+00  5.4492e-01  11.4073 < 2.2e-16 ***
    ## D1A_ID                    1.0403e-02  6.2245e-03   1.6714 0.0948674 .  
    ## D3_ESCOLA                -1.1159e-01  4.6830e-02  -2.3829 0.0173042 *  
    ## D9                       -3.6198e-05  3.3879e-05  -1.0684 0.2855005    
    ## Q1501                    -3.9464e-01  2.6253e-02 -15.0320 < 2.2e-16 ***
    ## Q18                       3.1608e-01  2.8415e-02  11.1239 < 2.2e-16 ***
    ## D2_SEXO                  -6.8736e-01  1.7877e-01  -3.8449 0.0001258 ***
    ## religiaoEvangélica        6.6854e-01  1.9607e-01   3.4096 0.0006683 ***
    ## religiaoNão tem religião -7.5647e-02  3.7246e-01  -0.2031 0.8390834    
    ## religiaoOutras           -8.3256e-01  3.0432e-01  -2.7358 0.0062994 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(regressao, vcov = vcovHC(regressao, type = "HC3"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                             Estimate  Std. Error  t value  Pr(>|t|)    
    ## (Intercept)               6.2160e+00  5.4715e-01  11.3607 < 2.2e-16 ***
    ## D1A_ID                    1.0403e-02  6.2657e-03   1.6603 0.0970600 .  
    ## D3_ESCOLA                -1.1159e-01  4.7247e-02  -2.3619 0.0183123 *  
    ## D9                       -3.6198e-05  3.6481e-05  -0.9922 0.3212463    
    ## Q1501                    -3.9464e-01  2.6381e-02 -14.9593 < 2.2e-16 ***
    ## Q18                       3.1608e-01  2.8534e-02  11.0772 < 2.2e-16 ***
    ## D2_SEXO                  -6.8736e-01  1.7967e-01  -3.8256 0.0001360 ***
    ## religiaoEvangélica        6.6854e-01  1.9676e-01   3.3978 0.0006978 ***
    ## religiaoNão tem religião -7.5647e-02  3.7488e-01  -0.2018 0.8401094    
    ## religiaoOutras           -8.3256e-01  3.0592e-01  -2.7215 0.0065759 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coeftest(regressao, vcov = vcovHC(regressao, type = "HC4"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                             Estimate  Std. Error  t value  Pr(>|t|)    
    ## (Intercept)               6.2160e+00  5.4609e-01  11.3828 < 2.2e-16 ***
    ## D1A_ID                    1.0403e-02  6.2968e-03   1.6521 0.0987209 .  
    ## D3_ESCOLA                -1.1159e-01  4.7777e-02  -2.3357 0.0196422 *  
    ## D9                       -3.6198e-05  4.2683e-05  -0.8481 0.3965366    
    ## Q1501                    -3.9464e-01  2.6398e-02 -14.9497 < 2.2e-16 ***
    ## Q18                       3.1608e-01  2.8478e-02  11.0992 < 2.2e-16 ***
    ## D2_SEXO                  -6.8736e-01  1.8002e-01  -3.8183 0.0001400 ***
    ## religiaoEvangélica        6.6854e-01  1.9646e-01   3.4029 0.0006849 ***
    ## religiaoNão tem religião -7.5647e-02  3.7493e-01  -0.2018 0.8401286    
    ## religiaoOutras           -8.3256e-01  3.0545e-01  -2.7256 0.0064948 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

O p-valor dos testes foram muito baixos, logo o pressuposto da
homocedasticidade não foi satisfeito, pois podemos rejeitar a hipótese
nula.

### Avalie a multicolinearidade entre as variáveis

``` r
vif(regressao)
```

    ##               GVIF Df GVIF^(1/(2*Df))
    ## D1A_ID    1.219401  1        1.104265
    ## D3_ESCOLA 1.337368  1        1.156446
    ## D9        1.094849  1        1.046350
    ## Q1501     1.119818  1        1.058215
    ## Q18       1.049195  1        1.024302
    ## D2_SEXO   1.023001  1        1.011435
    ## religiao  1.093846  3        1.015062

Quanto maior o valor, maior a multicoinearidade. Todas as variáveis
apresentaram multicolinearidade, mas nenhuma apresentou um valor muito
alarmante. Alguns afirmam que é preocupante valores acima de 10, outros
acima de 4. De qualquer forma, nenhuma variável se afastou muito do
valor 1.

### Verifique a presença de outilier ou observações influentes no modelo

``` r
plot(regressao, 4)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot(regressao, 5)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
outlierTest(lm(Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco))
```

    ## No Studentized residuals with Bonferroni p < 0.05
    ## Largest |rstudent|:
    ##     rstudent unadjusted p-value Bonferroni p
    ## 271 -2.76344          0.0057918           NA

A distância cook mostra que os itens 160, 399 e 1442 são potenciais
outliers. A observação de valor mais influente é a de 1442. O p-valor
corrigido pelo ajuste de Bonferroni é bem baixo, sugerindo também há
presença de um outlier.

### Faça a regressao linear sem a observação mais influente e avalie a alteração do resultado

``` r
banco_filtrado <- banco %>%
  filter(A1_Nquest != 2460) 


regressao1 <- lm(Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco_filtrado)

summary(regressao1)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco_filtrado)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.1171 -2.4749  0.3718  2.3110  8.9899 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               6.240e+00  5.360e-01  11.640  < 2e-16 ***
    ## D1A_ID                    1.133e-02  6.243e-03   1.814 0.069827 .  
    ## D3_ESCOLA                -1.022e-01  4.504e-02  -2.268 0.023446 *  
    ## D9                       -6.396e-05  3.071e-05  -2.083 0.037444 *  
    ## Q1501                    -3.975e-01  2.369e-02 -16.781  < 2e-16 ***
    ## Q18                       3.157e-01  2.600e-02  12.139  < 2e-16 ***
    ## D2_SEXO                  -7.080e-01  1.747e-01  -4.053 5.31e-05 ***
    ## religiaoEvangélica        6.807e-01  1.983e-01   3.433 0.000613 ***
    ## religiaoNão tem religião -6.671e-02  3.481e-01  -0.192 0.848065    
    ## religiaoOutras           -8.193e-01  3.078e-01  -2.662 0.007855 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.292 on 1451 degrees of freedom
    ## Multiple R-squared:  0.3037, Adjusted R-squared:  0.2994 
    ## F-statistic: 70.32 on 9 and 1451 DF,  p-value: < 2.2e-16

``` r
plot(regressao1, 4)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
plot(regressao1, 5)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-6-2.png)<!-- --> A
variável de renda (D9) foi uma das que teve maior alteração: Na
primeira regressão, não era significativo, mas, sem o outlier, está
significativo. O coeficiente continuou negativo (quanto maior a renda,
menor o apreço a Bolsonaro). Demais p-valores: houve mudanças neles, mas
nenhuma grande o suficente para mudar a significância em relação à
primeira regressão. Houve mudanças nos coeficientes também. Um dos que
mais mudou foi de gênero (`D2_SEXO`): antes era -6.874 e, agora, foi de
-7.128. Ou seja, aumentou o efeito negativo (Em outras palavras, sem
outlier o fato de ser mulher aumenta a falta de apreço a Bolsonaro).
Outro coeficiente que merece ser mencionado especificamente é o de “não
ter religião”. Ainda que o p-valor não tenha sido significativo nas duas
regressões, notamos que o coeficiente, antes, era de -7.565 e, agora, é
de -1.583 (ou seja, o impacto negativo diminuiu). Os R² mudaram muito
pouco entre os modelos.
