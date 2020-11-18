# O modelo que sera estudado dessa vez sera o de classificacao. O que queremos prever usando esse
# modelo eh se um programador trabalha remotamente ou nao, baseado em suas caracteristicas. Usaremos
# uma base de dados dos stackOverflow que possui dados de diversos programadores incritos no site.
library(tidyverse)
library(tidymodels)
library(themis)

setwd("/home/paulo/Documentos/estudo\ de\ machine\ learning/capitulo\ 2/")

stackOverflow <- read_csv('data/stack_overflow.csv')


# Observando a proporcao dos que trabalham remotamente
stackOverflow %>% count(remote, sort = TRUE)
# Observando a distruibuicao de usuarios por pais 
stackOverflow %>% count(country, sort=TRUE)
# A visualizacao disso mostra que nao há um balanceamento entre os dados dos que trabalham remotamente
# e os que nao trabalham. Grande parte nao trabalha de forma remota.

# Tentando observar uma relacao entre os anos de experiencia e se trabalha remotamente. 
# Plot01
ggplot(stackOverflow, aes(remote, years_coded_job)) +
  geom_boxplot()+
  labs(x = NULL,
       y = "Anos de experiencia como programador profissional")

# Ajustando os dados  
stackOverflow <- stackOverflow %>% 
  mutate(remote = factor(remote, levels = c("Remote", "Not remote"))) %>%
  mutate_if(is.character, factor)

stackOverflow <- stackOverflow %>% select(-respondent)


# Iniciando divisao dos dados 

set.seed(1234)
stack_split <- stackOverflow %>%
  initial_split(prop = 0.8,
                strata = remote)

stack_treino <- training(stack_split)
stack_teste <- testing(stack_split)

# Como os dados de programadores remotos estao desbalanceados, isso pode atrapalhar nossas predicoes.
# Para resolver esse problema, iremos balancear essas dados removendo parte do grupo majoritario de
# forma aleatoria. Para isso usaremos um estrategia chamada Downsample

        # settando a estrategia de downsample na variavel stack_recipe
        # stack_recipe estara guardando uma "receita" de sobre qual variavel sera feito o downsample
stack_recipe <- recipe(remote ~ ., data = stack_treino) %>%
  step_downsample(remote)


        #Essas duas linhas de codigo servem para "preparar" a receita e depois "cozinhar" essa
        #essa receita. Logo, teremos na variavel stack_down o data frame totalmente balanceado.
stack_prep <- prep(stack_recipe)
stack_down <- bake(stack_prep, new_data = NULL)

        # agora os dados de treino estao balanceados 
stack_down %>%
  count(remote)

rm(stack_down, stack_prep)

#Treinando os dados
        
        # Regressão logistica 

        # settamos o modelo que sera usado
glm_spec <- logistic_reg() %>%
  set_engine('glm')

        # usamos um workflow para abstrair toda a logica de preparacao e finalizacao da receita 
stack_wf <- workflow() %>%
  add_recipe(stack_recipe)

        # adicionando o modelo e ajustando o workflow
stack_glm <- stack_wf %>%
  add_model(glm_spec) %>%
  fit(data = stack_treino)




        # Arvore de decisao


        # settando o modelo que sera usado
tree_spec <- decision_tree() %>%
  set_engine('rpart') %>%
  set_mode('classification')
        
        # adicionando o modeo e ajustando o workflow
stack_tree <- stack_wf %>%
  add_model(tree_spec) %>%
  fit(data = stack_treino)

