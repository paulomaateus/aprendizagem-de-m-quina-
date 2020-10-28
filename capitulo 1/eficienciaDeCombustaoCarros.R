#Estudo disponibilizado pelo curso de aprendizagem de maquina supervisionado 
#https://supervised-ml-course.netlify.app
#Com a ajuda deste material tentarei aprender alguns conceitos de aprendizagem de maquina na pratica
#Todo conteudo aqui disponivel eh de puro interesse academico 

#O objeto de estudo sera um conjunto de dados coletados pelo US Departament of Energy que contem
#informacoes do consumo de combustiveis de automoveis e diversas outras. O que queremos eh prever 
#a eficiencia dos combusteveis nesses veiculos.

#Usaremos a biblioteca tidyverse que dispoe de uma colecao de pacotes para o manuseio de dados

#O modelo escolhido em primeiro momento para fazer essas predicoes foi o modelo de regressao.
#Esse eh o modelo mais recomendado quando vamos prever grandezas quantitavas continuas como a 
#eficiencia do combustivel em um determinado veiculo que seria algo como kilometros por litros, 
#milhas por galao, etc

#                      Informando as bibliotecas que serao usadas
library(tidyverse)

#                       Lendo os dados que serao usados  
setwd("/home/paulo/Documentos/estudo\ de\ machine\ learning/regressao/")

carros2018 <- read_csv("data/cars2018.csv")

# Rapida visualizacao dos dados 
glimpse(carros2018)


# Com uma rapida olhada nos dados ja eh possivel plotar um grafico trivial do interesse do estudo 
# que seria um histograma da frequencia das milhas por galoes (MPG). 


# plot01
ggplot(carros2018, aes(x = MPG)) +
  geom_histogram(bins = 25) +
  labs(x = "Milhas por galão (eficiencia/mpg)",
       y = "Numero de carros")



# De incio sera usado um modelo ml o mais simples possivel para que se tenha uma ideia do que 
# acontece em um modelo de aprendizagem de maquina

        
        # Criando um df sem as informacoes de index e modelos dos carros pois essas informacoes sao 
        #irrelavantes
carros_semIndex_semModelo <- carros2018 %>%
  select(-Model, -`Model Index`)

        # Guardando na variavel o resultado do treinamento do modelo
fit_all <- lm(MPG ~ ., data = carros_semIndex_semModelo)

        # printando um sumario do modelo
summary(fit_all)

        #Do sumario apresentado temos o resumo de diversas informacoes colhidas pelo treinamento como
        # a forca de alguam correlacoes, a taxa de erro, etc.



# Agora se inicia finalmente o uso do tidyverse para garantirmos uma melhor acuracia nos resultados
# dos treinos 

        #Biblioteca que dispoe de uma colecao de modelos de aprendizagem de maquina 
library(tidymodels)


        #Essa linha de codigo inicializa a divisao dos conjuntos de dados em duas partes que serao 
        #usadas para treino e test. 80% dos dados do conjunto sao dados de treino e 20% sao dados 
        #para testes 
carros_split <- carros_semIndex_semModelo %>% 
  initial_split(prop=0.8, strata = Aspiration)

        # Aqui eh atribuido a duas variaveis os cojuntos de dados de testes e treino, respectivamente. 
carros_treino <- training(carros_split)
carros_teste <- testing(carros_split)


        # Regressao linear

      

        # Realizando o treinamento  
fit_lm <- train(log(MPG) ~ .,
                method = "lm",
                data = carros_treino,
                trControl = train) 

fit_lm 




          #Random forest
library(randomForest)

        # Definindo o tipo do modelo
rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

        # Realizando o treinamento
fit_rf <- rf_mod %>%
  fit(log(MPG) ~ ., 
      data = carros_treino)



