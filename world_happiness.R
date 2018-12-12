#Ler os dados
dados_2015 = as.data.frame(read.csv("./Dataset/2015.csv", header = TRUE), stringsAsFactors = FALSE)
dados_2016 = as.data.frame(read.csv("./Dataset/2016.csv", header = TRUE), stringsAsFactors = FALSE)
dados_2017 = as.data.frame(read.csv("./Dataset/2017.csv", header = TRUE), stringsAsFactors = FALSE)

#Renomear "Taiwan Province of China" para "Taiwan"
levels = levels(dados_2017$Country)
levels[length(levels) + 1] = "Taiwan"
dados_2017[33, 1] = NA

dados_2017$Country = factor(dados_2017$Country, levels = levels)
dados_2017$Country[is.na(dados_2017$Country)] = "Taiwan"

#Renomear "Hong Kong S.A.R, China" para "Hong Kong"
levels = levels(dados_2017$Country)
levels[length(levels) + 1] = "Hong Kong"
dados_2017[71, 1] = NA

dados_2017$Country = factor(dados_2017$Country, levels = levels)
dados_2017$Country[is.na(dados_2017$Country)] = "Hong Kong"

#Visto que os dados de 2017 não tem a coluna de Region, adiciona-la
region_column = c()
for (row in 1:nrow(dados_2017) ) {
  
  country_name = as.character(dados_2017[row, 1]) #Obtem o nome do país presenta na linha "row"
  
  region_name_2015 = as.character(dados_2015[dados_2015[ , 1] == country_name  , 2])
  
  if(length(region_name_2015) == 0) { #Se o dataset de 2015 não tiver o país country_name então tenta-se com o dataset de 2016
    
    region_name_2016 = as.character(dados_2016[dados_2016[ , 1] == country_name  , 2]) 
    
    if(length(region_name_2016) == 0) {
      
      region_column[row] = NA #Se o dataset de 2016 também não tiver o país country_name por NA
      
    } else {
      
      region_column[row] = region_name_2016
      
    }
    
  } else {
    region_column[row] = region_name_2015
  }
  
}

dados_2017$Region = region_column #Adicionar uma coluna chamada Region ao dataset de 2017

#Simplificar o nome dos dados
colnames(dados_2015) = c("Country", "Region", "Rank", "Score", "Error", "GDP", "Family", "Life Expectancy", "Freedom", "Government Corruption", "Generosity", "Dystopia")
colnames(dados_2016) = c("Country", "Region", "Rank", "Score", "Lower Confidence", "Upper Confidence", "GDP", "Family", "Life Expectancy", "Freedom", "Government Corruption", "Generosity", "Dystopia")
colnames(dados_2017) = c("Country", "Rank", "Score", "Upper Confidence", "Lower Confidence","GDP", "Family", "Life Expectancy", "Freedom", "Generosity", "Government Corruption", "Dystopia", "Region")

dados_2017 = dados_2017[, c(1, 13, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10, 12)] #Mudar a posição da ultima coluna para ser igual à de 2015 e 2016 e meter a Region como segunda coluna

#Padronizar os dados
dados_2015[, -c(1:5)] = scale(dados_2015[, -c(1:5)], scale = TRUE, center = TRUE)
dados_2016[, -c(1:6)] = scale(dados_2016[, -c(1:6)], scale = TRUE, center = TRUE)
dados_2017[, -c(1:5)] = scale(dados_2017[, -c(1:5)], scale = TRUE, center = TRUE)

#Ficar apenas com as variaveis de interesse para a PCA
dados_2015_var = dados_2015[, -c(c(1:5), 12)]
dados_2016_var = dados_2016[, -c(c(1:6), 13)]
dados_2017_var = dados_2017[, -c(c(1:6), 13)]


#-----------------------------------Análise preliminar dos dados--------------------------------------

#Obter a média dos datasets
xbar_1 = colMeans(dados_2015_var)
xbar_2 = colMeans(dados_2016_var)
xbar_3 = colMeans(dados_2017_var)

#Numero de variaveis nos dados, é identico em todos os datasets por isso basta ver para um
p = ncol(dados_2015_var)
correlacoes_distintas = p * (p-1) / 2

#Ver correlações entre as variáveis
s_2015 = var(dados_2015_var)
s_2016 = var(dados_2016_var)
s_2017 = var(dados_2017_var)

round(s_2015, 3)
#Para os dados de 2015 conseguimos ver uma correlação alta entre o GDP (1ª var) e Family (2ª var) e Life Expectancy (3ª var)
#vemos também uma correlação altinha entre Family (2ª var) e Life expectancy (3ª var)

#Vemos que para os datasets de 2016 e 2017 os valores são mais ou menos idênticos
round(s_2016, 3)
round(s_2017, 3)

#Olhando para a matriz variancia-covariancia, que com os dados padronizados, é igual à matriz de correlações
#podemos considerar como viável a utilização da PCA, porque conseguimos identificar uma correlação forte entre variáveis

pairs(dados_2015_var)
pairs(dados_2016_var)
pairs(dados_2017_var)

#Fazendo o plot do pairs conseguimos ter uma representação gráfica da correlação entre as variaveis
#especialmente entre aquelas que identificamos a cima

#------------------------------------------Análise de Perfis-----------------------------------------------
#Uma primeira pergunta que podemos responder sobre o Dataset é se os valores das variáveis mantêm-se ralativamente constantes durante os 3 anos
#Para obter a resposta a esta questão podemos então proceder ao teste de paralelismo

#Teste de paralelismo
#Testar o paralelismo entre 2015 e 2016

contrastes = matrix(
  c(
    -1, 1, 0, 0, 0, 0,
    0, -1, 1, 0, 0, 0,
    0, 0, -1, 1, 0, 0,
    0, 0, 0, -1, 1, 0,
    0, 0, 0, 0, -1, 1
  ), 
  nrow = 5, ncol = 6, byrow = T
)

teste_paralelismo = function(dados_1, dados_2, c, alpha = 0.05) {
  #Executa o teste ao paralelismo sobre os dados enviados por parametros da função
  #Esta função retorna TRUE caso H0 não seja rejeitada, e FALSE caso contrário
  # c representa a matriz de contrastes
  #H0: Cmu1 = Cmu2
  
  n1 = nrow(dados_1)
  n2 = nrow(dados_2)
  q = ncol(dados_1) # = ncol(dados2) = ncol(contrastes)
  
  xbar_1 = colMeans(dados_1)
  xbar_2 = colMeans(dados_2)
  s_1 = var(dados_1)
  s_2 = var(dados_2)
  s_pooled = ((n1 - 1) / (n1 + n2 - 2)) * s_1 + ((n2 - 1) / (n1 + n2 - 2)) * s_2
  
  t_squared = t(c %*% (xbar_1 - xbar_2)) %*% solve(((1/n1) + (1/n2)) * c %*% s_pooled %*% t(c)) %*% (c %*% (xbar_1 - xbar_2)) #valor observado
  
  quantil_critico = ((n1 + n2 - 2) * (q - 1) / (n1 + n2 - q)) * qf(1 - alpha, q - 1, n1 + n2 - q)
  
  p_value = 1 - pf(t_squared / ((n1 + n2 - 2) * (q - 1) / (n1 + n2 - q)), q - 1, n1 + n2 - q)
  
  #Não se rejeita H0 se t_squared < quantil_critico e p_value > alpha
  return(t_squared < quantil_critico && p_value > alpha)
}


teste_coincidencia = function(dados_1, dados_2, alpha = 0.05) {
  #Executa o teste à coincidencia sobre os dados enviados por parametros da função
  #Esta função retorna TRUE caso H0 não seja rejeitada, e FALSE caso contrário
  #H0: 1'mu1 = 1'mu2
  
  ones = rep(1, ncol(dados_1))
  n1 = nrow(dados_1)
  n2 = nrow(dados_2)
  
  xbar_1 = colMeans(dados_1)
  xbar_2 = colMeans(dados_2)
  s_1 = var(dados_1)
  s_2 = var(dados_2)
  s_pooled = ((n1 - 1) / (n1 + n2 - 2)) * s_1 + ((n2 - 1) / (n1 + n2 - 2)) * s_2
  
  t_squared = t(t(ones) %*% (xbar_1 - xbar_2)) %*% solve(((1/n1) + (1/n2)) * t(ones) %*% s_pooled %*% ones) %*% (t(ones) %*% (xbar_1 - xbar_2)) #valor observado
  
  quantil_critico = qf(1 - alpha, 1 , n1 + n2 -2)
  
  p_value = 1 - pf(t_squared, 1 , n1 + n2 -2)
  
  #Não se rejeita H0 se t_squared < quantil_critico e p_value > alpha
  return(t_squared < quantil_critico && p_value > alpha)
}


teste_horizontalidade = function(dados_1, dados_2, c, alpha = 0.05) {
  #Executa o teste à horizontalidade sobre os dados enviados por parametros da função
  #Esta função retorna TRUE caso H0 não seja rejeitada, e FALSE caso contrário
  # c representa a matriz de contrastes
  #H0: 1/2 * C * (mu1 + mu 2) = 0
  
  n1 = nrow(dados_1)
  n2 = nrow(dados_2)
  q = ncol(dados_1) # = ncol(dados2) = ncol(contrastes)
  
  xbar_1 = colMeans(dados_1)
  xbar_2 = colMeans(dados_2)
  xbar = (n1 * xbar_1 + n2 * xbar_2) / (n1 + n2)
  
  s_1 = var(dados_1)
  s_2 = var(dados_2)
  s_pooled = ((n1 - 1) / (n1 + n2 - 2)) * s_1 + ((n2 - 1) / (n1 + n2 - 2)) * s_2
  
  t_squared = (n1 + n2) * t(c %*% xbar) %*% solve(c %*% s_pooled %*% t(c)) %*% (c %*% xbar) #valor observado
  
  quantil_critico = ((n1 + n2 - 1) * (q -1) / (n1 + n2 - q)) * qf(1 - alpha, q - 1, n1 + n2 - q)
  
  p_value = 1 - pf(t_squared / ((n1 + n2 - 1) * (q -1) / (n1 + n2 - q)), q - 1, n1 + n2 - q)
  
  #Não se rejeita H0 se t_squared < quantil_critico e p_value > alpha
  return(t_squared < quantil_critico && p_value > alpha)
}

teste_paralelismo(dados_2015_var, dados_2016_var, contrastes)
#Visto que não se rejeitou H0 podemos então concluir que os dois perfis de 2015 e 2016 são então paralelos
#fazendo então sentido testar a sua coincidência e horizontalidade
#Com este resultado já podemos admitir que os dois perfis são iguais durante os dois anos mas poderá
#haver uma diferença constante de valores
#Exemplo [1, 2, 3] e [2, 3, 6] são paralelos mas não coincidentes porque há uma constante que os diferencia (multiplicar por 2)

teste_coincidencia(dados_2015_var, dados_2016_var)
#Não existem evidências estatisticas para rejeitar H0 então conclui-se que estes dois perfis são coincidentes
#O que nos diz que estes dois perfis mantêm-se exatamente iguais
#É também então interessante estudar a sua horizontalidade


teste_horizontalidade(dados_2015_var, dados_2016_var, contrastes)
#Não existem evidências estatisticas para rejeitar H0 então concluimos que os dois perfis são também horizontais

#Visto que o perfil de 2015 é paralelo, horizontal e coincidente ao perfil de 2016 podemos usar qualquer um
#destes dois para testar as mesmas condições em relação ao perfil de 2017

teste_paralelismo(dados_2016_var, dados_2017_var, contrastes)

teste_coincidencia(dados_2016_var, dados_2017_var)

teste_horizontalidade(dados_2016_var, dados_2017_var, contrastes)

#Onde verificamos que em nenhum dos casos existe evidência estatistica para rejeitar H0 então concluimos que
# os três perfis, 2015, 2016 e 2017 são paralelos, coincidentes e horizontais.
#Isto é, os três perfis mantêm-se significativamente iguais ao longo dos três anos.



#-----------------------------------------Teste de Mauchly---------------------------------------------
#Teste de Mauchly
#O teste de mauchly é bom para ver se matriz identidade é significativamente identica à matriz identidade
#Se for quer dizer que não tem muito interesse fazer a PCA
#então aqui o que queremos para usar a PCA é rejeita H0

#Mas visto que este teste pressupõe normalidade multivariada é primeiro preciso estudar o ajustamento
#da normal multivariada aos nossos dados. Uma das maneiras de fazermos isso é através de um plot do gráfico QQ
#Este plot compara os quantis amostrais (empíricos) com os quantis teóricos que esperariamos obter se os dados
#tivessem vindo mesmo de uma normal multivariada

library(car)
dev.off()
qqp(dados_2015_var, distribution = "norm")
qqp(dados_2016_var, distribution = "norm")
qqp(dados_2017_var, distribution = "norm")

#Podemos ver nos 3 gráficos que os três datasets diferentes têm um bom ajuste a uma normal multivariada
#logo podemos então utilizar o teste de Mauchly.

mauchly_test = function(dados, alpha = 0.05) {
  #Executa o teste de Mauchly para os dados que forem passados por argumento
  #Esta função retorna TRUE caso H0 não seja rejeitada, e FALSE caso contrário
  
  library("psych")
  s = var(dados)
  n = nrow(dados)
  p = ncol(dados)
  
  u = ((p^p) * det(s)) / (tr(s))^p
  u_star = -(n - 1 - (((2 * p^2) + p + 2) / (6*p))) * log(u) #Valor observado
  p_value = 1 - pchisq(u_star, (1/2) * p * (p + 1) - 1) #P-value do valor observado
  quantil_critico = qchisq(1 - alpha, (1/2) * p * (p + 1) - 1) #Quantil critico a comprar com o valor observado
  
  #Não se rejeita H0 se u_star < quantil_critico e p_value > alpha
  return(u_star < quantil_critico && p_value > alpha)
}

mauchly_test(dados_2015_var)
mauchly_test(dados_2016_var)
mauchly_test(dados_2017_var)

#O teste deu FALSE nos tres anos significa que se rejeitou H0 nos três datasets logo existem evidências estatisticas
#que permitem concluir que a matriz de variancia-covarianca dos 3 datasets é significativamente diferente da 
#matriz identidade (assumindo a existência de normalidade multivariada) o que significa que existem variáveis
#cuja correlação entre si é diferente de 0 logo torna-se pretinente a utilização da PCA


#O R também disponibiliza uma função que possibilita executar o mesmo teste de uma maneira mais simples
mauchly.test(lm(as.matrix(dados_2015_var)~1))
mauchly.test(lm(as.matrix(dados_2016_var)~1))
mauchly.test(lm(as.matrix(dados_2017_var)~1))

#Onde podemos observar que para os três datasets os p-values foram todos inferiores a 2.2e-16, muito
#inferiores ao nosso alpha estabelecido (0.05) logo tiramos as mesmas conclusões que a cima


#----------------------------------Análise de Componentes Principais-------------------------------------

pca = function(s, main_title){
  #Realiza o estudo de componentes principais para um determinado dataset precisando apenas 
  #de fornecer a matriz de variancias-covariancias
  #Esta função retorna os eigen vectors da matriz passada por argumento da função que correspondem às 
  #componentes principais do dataset
  
  ev = eigen(s)
  variabilidade_total = sum(ev$values)
  
  prop_table = data.frame(ev = ev$values)
  prop_table$std = sqrt(prop_table$ev) #
  prop_table$prop = prop_table$ev/6 #aqui obtemos a proporcao de variabilidade que cada comb linear guarda (foi o que eu fiz no for)
  prop_table$cprop = cumsum(prop_table$prop) #Aqui obtemos a proporcao cumulativa
  print(round(prop_table, 3))
  
  plot(1 : nrow(prop_table), prop_table$ev, type = "b", main =main_title ,ylab = "Valores próprios", xlab = "CP")
  text(x = prop_table$ev, labels = paste(round(prop_table$ev, 2)), pos = 4, cex = 0.8)
  return(ev$vectors)
}

#Existem vários critérios escolhidos para escolher o número de componentes principais a reter
#Percentagem da variabilidade explicada - Neste critério podemos estabelecer uma percentagem da variabilidade
#explicada que queremos reter e guardar as componentes principais suficientes para atingir essa percentagem.

#Querendo manter pelo menos 80% da variabilidade poderiamos ver na coluna cprop que nos dá a proporção cumulativa
#que precisariamos de guardar as 3 primeiras componentes principais de maneira a reter 80% da variabilidade

#Critério de Kaiser - Reter apenas as variáveis cujo valor próprio seja superior a 1 (caso as variáveis estejam padronizadas)
#Com estre critério reteriamos apenas as duas primeiras componentes principais

#Scree plot - Metodo mais gráfico que determina o número de CP's pelo ponto onde a linha desenhada
#diminui acentuadamente de inclinação tornando-se horizontal
#Segundo este critério reteriamos também as 3 componentes principais




#----------------Para os dados de 2015
componentes_principais_2015 = pca(s_2015, "Screeplot 2015 dataset")

#Utilizando o critério de Kaiser as componentes principais seriam:
componentes_principais_2015[,1]
componentes_principais_2015[,2]

#O R possui uma função que também permite realizar o estudo dos componentes principais
pca_2015 = prcomp(dados_2015_var)
summary(pca_2015)
screeplot(pca_2015, type = "l", main = "Screeplot 2015 dataset")
#Aqui obteriamos exatamente os mesmo resultados que nas conclusões tiradas em cima visto que é exatamente
#o mesmo procedimento que em cima. Poderiamos obter as compoentes principais através de 
pca_2015$rotation[,1]
pca_2015$rotation[,2]
#Onde, segundo o critério de Kaiser, manteriamos apenas as 2 primeiras componentes principais




#----------------Para os dados de 2016
compoenentes_principais_2016 = pca(s_2016, "Screeplot 2016 dataset")

#Utilizando o critério de Kaiser as componentes principais seriam também as duas primeiras componentes
componentes_principais_2016[,1]
componentes_principais_2016[,2]

#Vendo pela função do R do prcomp
pca_2016 = prcomp(dados_2016_var)
summary(pca_2016)
screeplot(pca_2016, type = "l", main = "Screeplot 2016 dataset")
#Chegariamos a mesma conclusão, usando o criério de Kaiser ficariamos apenas com as duas primeiras
#componentes principais

pca_2016$rotation[, 1] #Podiamos acede-las desta forma
pca_2016$rotation[, 2] #Podiamos acede-las desta forma






#----------------Para os dados de 2017
compoenentes_principais_2017 = pca(s_2017, "Screeplot 2017 dataset")

#Utilizando o critério de Kaiser as componentes principais seriam também as duas primeiras componentes
componentes_principais_2017[,1]
componentes_principais_2017[,2]

#Vendo pela função do R do prcomp
pca_2017 = prcomp(dados_2017_var)
summary(pca_2017)
screeplot(pca_2017, type = "l", main = "Screeplot 2017 dataset")
#Chegariamos a mesma conclusão, usando o criério de Kaiser ficariamos apenas com as duas primeiras
#componentes principais

pca_2017$rotation[, 1] #Podiamos acede-las desta forma
pca_2017$rotation[, 2] #Podiamos acede-las desta forma

#Embora tenhamos visto com este estudo que grande parte da variabilidade dos datasets (cerca de 80%) pode
#ser explicada por apenas duas componentes principais, o resto do estudo aos datasets continuará a ser trabalhado
# sobre todas as variaveis do dataset visto a sua dimensionalidade não ser muito elevada não fazendo sentido assim
# descartar 20% da variabilidade.



#-------------------------------------Análise Descriminante-------------------------------------------

#O terceiro estudo consiste em criar um descriminante linear entre os diferentes continentes de maneira a estudar
#A separação existente entre os mesmo
#Neste caso serão usados os dados de 2017 visto que como vimos na análise de perfis os 3 dataset mantêm-se significativamente
#iguais ao longo do tempo logo poderá usar-se qualquer um dos 3 datasets.

#Primeiro agrupamos os nossos grupos de paises organizados por continente
library(data.table) #Para usar a função %like%

#Obter as variáveis de interesse de países que a região contenha uma certa String
europa = dados_2017[dados_2017[, "Region"] %like% "Europe", -c(c(1:6), 13)]
america_norte = dados_2017[dados_2017[, "Region"] %like% "North America", -c(c(1:6), 13)]
america_sul = dados_2017[dados_2017[, "Region"] %like% "Latin America", -c(c(1:6), 13)]
africa = dados_2017[dados_2017[, "Region"] %like% "Africa", -c(c(1:6), 13)]
asia = dados_2017[dados_2017[, "Region"] %like% "Asia", -c(c(1:6), 13)]
oceania = dados_2017[dados_2017[, "Region"] %like% "Australia", -c(c(1:6), 13)]


#Adicionar o novo nome do grupo aos dados
europa$Region = "Europe"
america_norte$Region = "North America"
america_sul$Region = "South America"
africa$Region = "Africa"
asia$Region = "Asia"
oceania$Region = "Oceania"

#Agora que temos os nosso dados agrupados por continentes podemos então criar os descriminantes entre eles

#Primeiro juntamos os nossos novos dados num dataframe só
todos_grupos = rbind(rbind(rbind(rbind(rbind(europa, america_norte), america_sul), africa), asia), oceania)

library("MASS")

resultado_lda = lda(Region ~ GDP + Family + `Life Expectancy` + Freedom +
                      `Government Corruption` + Generosity, data = todos_grupos)

# Obtenção dos coeficientes das combinações lineares
resultado_lda$scaling


#Construção do classificador
predictions = predict(resultado_lda, newdata = todos_grupos[, -7])

#Obtenção das previsões
predictions$class


#Criação da confusion matrix com a ground truth e as previsões
table(todos_grupos[, 7], predictions$class)

#A taxa de acerto é então a soma da diagonal sobre o número total de observações
taxa_acerto = sum(diag(table(todos_grupos[, 7], predictions$class))) / nrow(todos_grupos)

#E a taxa de erro é 1 - taxa de acerto
taxa_erro = 1 - taxa_acerto