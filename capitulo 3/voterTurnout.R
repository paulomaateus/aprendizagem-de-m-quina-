#O modelo estudado nessa capitulo sera o de classficacao visto que queremos prever se um eleitor votou
# ou nao na eleicao de 2016 nos EUA baseado nas suas caracteristicas. Como estamos tentando prever um 
# valor discreto, esse modelo eh o mais indicado.
# bibliotecas que serao usadas 
library(tidyverse)
library(tidymodels)
library(themis)

#Coleta de dados. 
#Vale salientar que essa coleta funciona apenas na minha maquina tendo que ocorrer as devidas 
#alteracoes para o uso em outras maquinas.
setwd("C:/Users/paulo/Documents/estudos de machine learning/aprendizagem-de-maquina/capitulo 3")
voters <- read_csv('data/voters.csv')


#Podemos obersvar inicialmente que a quantidade de votantes eh bem maior que a de nao votantes
#Isso indica que ha um grande desbalanceamento na amostra.
voters %>% count(turnout16_2016)

#Sumarizando alguns dados podemos obersevar que muitas pessoas que nao votaram acham que a eleicao 
#nao importa. Uma parte consideravel de votantes acreditam que o crime eh muito importante mas 
#contraditoriamente, uma parte consideravel de votantes acham que as eleicoes nao importam. 
voters %>%
  group_by(turnout16_2016) %>%
  summarise(`Eleicoes nao importam` = mean(RIGGED_SYSTEM_1_2016 <= 2),
            `Economia esta melhorando` = mean(econtrend_2016 == 1),
            `Crime eh muito importante` = mean(imiss_a_2016 == 2))

#Mudando o tipo da coluna dos votantes para factor e removendo coluna irrelevante
voters <- voters %>%
  mutate(turnout16_2016 = factor(turnout16_2016)) %>%
  select(-case_identifier)


#plot 01

        #Visualizando a plotagem de um grafico que relaciona o que as pessoas pensam sobre o rumo
# da economia e se elas sao ou nao votantes. Pode se observar que grande parte dos votantes e nao
#votantes acreditam que a economia se mantem quase a mesma coisa. Porem, quando se diz respeito aos
# que acreditam que a economia esta melhorando, maior parte dessas pessoas sao votantes. Pode se 
#observar tambem que os que acreditam que a economia esta piorando ou simplesmente nao sabem responder
# concentram mais pessoas nao votantes.
voters %>%
  ggplot(aes(econtrend_2016, after_stat(density), fill = turnout16_2016)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  labs(title = "As pessoas acham que a economia esta melhorando ou piorando?")



# Divindo data-set em partes de teste e treino

set.seed(1234)

voters_split <- voters %>%
  initial_split(p = 0.8,
                strata = turnout16_2016)

voters_treino <- training(voters_split)
voters_teste <- testing(voters_split)


rm(voters_split)




# Preparando "receita" do dataset para fazer um upsample (Abordagem que aumenta o tamanho da amostra
#para deixa-la balanceada)

voters_recipe <- recipe(turnout16_2016 ~ ., data= voters_treino) %>%
  step_upsample(turnou16_2016)


# Settando modelo de random forest com uma engine ranger
rf_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")


# Adcionando receita e modelo em um workflow
voters_wf <- workflow() %>%
  add_recipe(voters_recipe) %>%
  add_model(rf_spec)
