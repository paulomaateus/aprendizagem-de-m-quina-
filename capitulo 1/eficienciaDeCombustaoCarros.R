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
library(cowplot)
library(tidymodels)

#                       Lendo os dados que serao usados  
setwd("/home/paulo/Documentos/estudo\ de\ machine\ learning/capitulo\ 1/")

carros2018 <- read_csv("data/cars2018.csv")


# Com uma rapida olhada nos dados ja eh possivel plotar um grafico trivial do interesse do estudo 
# que seria um histograma da frequencia das milhas por galoes (MPG). 

        # plot01
ggplot(carros2018, aes(x = mpg)) +
  geom_histogram(bins = 25) +
  labs(x = "Milhas por galão (eficiencia/mpg)",
       y = "Numero de carros")



# De incio sera usado um modelo ml o mais simples possivel para que se tenha uma ideia do que 
# acontece em um modelo de aprendizagem de maquina

        
        # Criando um df sem as informacoes de index e modelos dos carros pois essas informacoes sao 
        #irrelavantes
carros_semIndex_semModelo <- carros2018 %>%
  select(-model, -model_index)
rm(carros2018)
        # Guardando na variavel o resultado do treinamento do modelo
fit_all <- lm(mpg ~ ., data = carros_semIndex_semModelo)

        # printando um sumario do modelo
summary(fit_all)

        #Do sumario apresentado temos o resumo de diversas informacoes colhidas pelo treinamento como
        # a forca de alguam correlacoes, a taxa de erro, etc.

rm(fit_all)

# Agora se inicia finalmente o uso do tidyverse para garantirmos uma melhor acuracia nos resultados
# dos treinos 

        #Biblioteca que dispoe de uma colecao de modelos de aprendizagem de maquina 



        
#As seguintes linhas de codigo sao para ajuste dos dados do dataframe.
#Precisamos transformar alguns dados das colunas em factors e precisamos ajustar a coluna de mpg
#para log visto que o histograma do mpg gerou uma distribuicao normal
cols <- c('transmission', 'aspiration', 'lockup_torque_converter', 'drive', 'recommended_fuel',
          'fuel_injection')

carros_semIndex_semModelo[cols] <- lapply(carros_semIndex_semModelo[cols], factor)

carros_semIndex_semModelo <- carros_semIndex_semModelo %>%
  mutate(mpg = log(mpg))
rm(cols)
#fim dos ajustes 


        #settando uma semente de aleatoriedade para que o split gere sempre os mesmos dados 
set.seed(1234)

        #Essa linha de codigo inicializa a divisao dos conjuntos de dados em duas partes que serao 
        #usadas para treino e test. 80% dos dados do conjunto sao dados de treino e 20% sao dados 
        #para testes 
carros_split <- carros_semIndex_semModelo %>% 
  initial_split(prop=0.8, strata = transmission)



        # Aqui eh atribuido a duas variaveis os cojuntos de dados de testes e treino, respectivamente. 
carros_treino <- training(carros_split)
carros_teste <- testing(carros_split)

rm(carros_split, carros_semIndex_semModelo)



        # Regressao linear

        # Indicando qual o tipo de modelo sera usado no treinamento
lm_mod <- linear_reg() %>% 
  set_engine("lm")

        # Realizando o treinamento
fit_lm <- lm_mod %>%
  fit(log(mpg) ~ .,
      data = carros_treino)
        


        #Random forest

        # Definindo o tipo do modelo
rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

        # Realizando o treinamento
fit_rf <- rf_mod %>%
  fit(log(mpg) ~ ., 
      data = carros_treino)


# Avaliando o desempenho dos modelos sobre os dados de treino

        # criando novas colunas no dataframe com os dados da predicao
resultadosTreino <- carros_treino    %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(fit_lm, carros_treino) %>%
              rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, carros_treino) %>%
              rename(.pred_rf = .pred))


        # Settando graficos que mostram a eficiencia dos modelos
p1 <- ggplot(data = resultadosTreino, aes(mpg, .pred_lm)) +
        ggtitle("LM") + 
        geom_abline(lty = 2, color = "gray50") +
        geom_point(color = "red") +
        geom_smooth(method = "lm")

p2 <- ggplot(data = resultadosTreino, aes(mpg, .pred_rf)) +
        ggtitle("RF") +
        geom_abline(lty = 2, color = "gray50") +
        geom_point(color = "orange") +
        geom_smooth(method = "lm")
        

        #avaliando a perfomance 
  
        #plot02
title <- ggdraw() +
  draw_label(
    "Avaliacao dos modelos sobre os dados de treino",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title,plot_grid(p1, p2),ncol = 1, rel_heights = c(0.1, 1))
        
metrics(resultadosTreino, truth =  mpg, estimate = .pred_lm )
metrics(resultadosTreino, truth =  mpg, estimate = .pred_rf )
# Podemos observar que o treinamento de random forest obteve melhores resultados que o de regressao linear.





# Avaliando o desempenho dos modelos sobre os os dados para testes

resultadosTeste <- carros_teste %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(fit_lm, carros_teste)%>%
              rename(.pred_lm = .pred))%>%
  bind_cols(predict(fit_rf, carros_teste) %>%
              rename(.pred_rf = .pred))



        # Settando graficos que mostram a eficiencia dos modelos
p1 <- ggplot(data = resultadosTeste, aes(mpg, .pred_lm)) +
        ggtitle("LM") + 
        geom_abline(lty = 2, color = "gray50") +
        geom_point(color = "red") +
        geom_smooth(method = "lm")

p2 <- ggplot(data = resultadosTeste, aes(mpg, .pred_rf)) +
        ggtitle("RF") +
        geom_abline(lty = 2, color = "gray50") +
        geom_point(color = "orange") +
        geom_smooth(method = "lm")

      
        # Avaliando perfomance    

        #plot03
title <- ggdraw() +
  draw_label(
    "Avaliacao dos modelos sobre os dados de teste",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title,plot_grid(p1, p2),ncol = 1, rel_heights = c(0.1, 1))

metrics(resultadosTeste, truth = mpg, estimate = .pred_lm)
metrics(resultadosTeste, truth = mpg, estimate = .pred_rf)
# Podemos observar que, como na observacao anterior, o modelo de random forest se mostra mais eficiente em prever os dados

rm(title,p1,p2, fit_lm, fit_rf, carros_teste, resultadosTeste, resultadosTreino)

# Agora vamos avaliar nossos modelos usando reamostragem de bootstrap para que os resultados das 
# avaliacoes tenham maior acuracia 

carros_boot <- bootstraps(carros_treino)

        # Settando modelo de regressao linear
lm_res <- lm_mod %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = carros_boot,
    control = control_resamples(save_pred = TRUE)  
  )


        # Settando mode de random forest
rf_res <- rf_mod %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = carros_boot,
    control = control_resamples(save_pred = TRUE)  
  )

        # Prevendo os dados e salvando em resultadosBoots
resultadosBoots <- bind_rows(lm_res %>%
                               collect_predictions() %>%
                               mutate(model = "lm"),
                             rf_res %>%
                               collect_predictions() %>%
                               mutate(model = "rf"))



        #plot04
resultadosBoots %>%
  ggplot(aes(`log(mpg)`, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(aes(color = id), size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(~ model)


# Ao fim da analise, observamoss que o modelo de random forest gera melhores resultados que o de regressao linear

rm(list = ls())




