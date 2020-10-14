# Vamos iniciar a segmentação de clientes

# Primeiro vamos carregar os dados.
cliente_dados = read.csv("C:/Users/ferna/Desktop/R-Scripts/Rstudio/Costumer_Segmentation.csv")
str(cliente_dados)

# Para ver todos os nomes das colunas
names(cliente_dados)

# Vamos ver agora as 6 primeiras linhas do nosso conjunto de dados.
head(cliente_dados)

# Vamos criar dois gráficos um de barra e outro de pizza para ver
# a distribuição de gêneros no nosso conjunto de dados.

genero=table(cliente_dados$Genre)
barplot(a,main="Gráfico para comparação de gênero",
        ylab="Contagem",
        xlab = "Gênero",
        col=rainbow(2),
        legend=row.names(genero))

# Como esse gráfico é possível identificar que o número de clientes do gênero
# feminino é maior do que o masculino

# Agora vamos plotar um gráfico de pizza para ver a proporção de entre
# os gêneros

pizza_prop = round(genero/sum(genero) * 100)
lbs=paste(c("Feminino","Masculino"), " ",pizza_prop,"%",sep=" ")
library(plotrix)
pie3D(genero, labels=lbs,
      main="Gráfico de proporção entre feminino e masculino")
# Através do gráfico de pizza podemos ver que a porcentagem do gênero feminino
# é de 56% e o de masculino é de 44%.

# Agora vamos plotar um gráfico para ver a distribuição das idades.
# O gráfico ideal para isso é o histograma.
hist(cliente_dados$Age,
     col="blue",
     main="Histograma para ver a frequência das idades",
     xlab = "Idade",
     ylab = "Frequência",
     labels =TRUE)

# Através desse gráfico acima podemos concluir que a faixa de idade entre 30 a 35
# são é faixa que mais compram e a faixa entre 60 a 65 são a que menos compram.


# Agora vamos analisar o salário anual dos clientes
summary(cliente_dados$Annual.Income..k..)
hist(cliente_dados$Annual.Income..k..,
     col="red",
     main="Histograma para salário anual",
     xlab = "Salário Anual",
     ylab = "Frequência",
     labels = TRUE)

# O gráfico acima mostra que os clientes que recebem entre 70 a 80 mil anuais
# São a maior faixa de clientes.



# Vamos analisar agora a pontuação de gastos dos clientes
summary(cliente_dados$Spending.Score..1.100.)

# Com o resumo dos dados podemos ver que a maior pontuação é de 99 e a menor é
# de 1 a média da pontuação de gasto é de 50.20

# Vamos analisar usando o gráfico boxplot.
boxplot(cliente_dados$Spending.Score..1.100.,
        horizontal = TRUE,
        col = "green",
        main="Boxplot para análise descritiva da pontuação de gastos")

# Vamos plotar um histograma
hist(cliente_dados$Spending.Score..1.100.,
     main = "Histograma para Pontuação de gastos",
     xlab = "Pontuação de gasto",
     ylab = "Frequência",
     col="purple",
     labels = TRUE)
# O gráfico acima mostra que o clientes que tem entre 40 a 50 pontos são a maior
# faixa dos clientes.


# Agora vamos usar um algoritmo para a segmentação dos clientes.
# O algoritmo que iremos utilizar é o k-means
# Para que o K-means seja utilizado, nós temos que definir qual é a 
# quantidade ideal de clusters e par isso podemos utilizar 2 métodos
# O método do cotovelo
# O método Silhouette


# vamos começar pelo método de cotovelo.
library(purrr)
set.seed(123)
# Função para calcular o total de intra-cluster das somas dos quadrados
iss <- function(k) {
  kmeans(cliente_dados[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Números do Clusteres K",
     ylab="Total de intra-clusteres das somas dos quadrados")

# Através do gráfico acima podemos ver que o número de clusteres ideal
# é 4 porque o valor está na curva do gráfico de cotovelo.


# Agora vamos utilizar o método média Silhouette ou Average Silhoutte.
# Esse método mede o quão bem um ponto se encaixa em um cluster.
# Essa medição é feita através de um gráfico medindo quão perto os pontos
# de um cluster estão dos pontos de outro cluster mais próximo.
# O coeficiente de Silhouette indica que quando mais próximo de +1,
# mais longe dos pontos do outro cluster, e quando próximo de 0, indica
# que os pontos estão mais perto.

# Vamos importar as bibliotecas para trabalhar com esse método
library(cluster)
library(gridExtra)
library(grid)

k2 <- kmeans(cliente_dados[,3:5],2,iter.max = 100, nstart = 50, algorithm = "Lloyd")
s2 <- plot(silhouette(k2$cluster,dist(cliente_dados[,3:5],"euclidean")))

# Analisando o gráfico acima, podemos entender que calculando a média com 2 
# clusters o valor da média é de 0.29

# Vamos tentar com 3 clusters
k3<-kmeans(cliente_dados[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(cliente_dados[,3:5],"euclidean")))

# Agora teve uma média de 0.38.

# Vamos tentar com 4 clusters
k4<-kmeans(cliente_dados[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(cliente_dados[,3:5],"euclidean")))

# A média foi de 0.41.
# Veja que a média vai subindo, nós temos que continuar até achar uma 
# quantidade que tenha um média maior.

# Vamos tentar com 5 clusters.
k5<-kmeans(cliente_dados[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(cliente_dados[,3:5],"euclidean")))

# Como a médio foi maior que anterior nós temos que continuar
# até que o valor da média diminuar.

# Vamos com 6 clusters.
k6<-kmeans(cliente_dados[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(cliente_dados[,3:5],"euclidean")))

# O resulta foi de 0.45.


# Vamos com 7 clusters.
k7<-kmeans(cliente_dados[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(cliente_dados[,3:5],"euclidean")))

# Veja que o resultado agora foi de 0.44, significa que nós que podemos
# terminar a procura ideal de clusters como o maior foi com 6 clusters
# Então essa quantidade é a ideal para o nosso processo de análise.
# Porque se você continuar fazendo com mais clusters irá ter valores
# menores daqui em diante.

# Agora vamos usar um função que vai determinar e mostrar o número ideal
# de clusters e assim irá confirmar se nós acertamos na escolha utilizando
# os procedimentos anteriores.

# Vamos importar as bibliotecas para esse processo.
library(NbClust)
library(factoextra)

fviz_nbclust(cliente_dados[,3:5], kmeans, method = "silhouette")

# pelo gráfico mostrado acima, podemos ver que acertamos ao escolher
# a quantidade de 6 clusters.

# Agora vamos plotar um gráfico Kmeans utilizando 6 clusters.

set.seed(1)
ggplot(cliente_dados, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segmento Clientes de Shopping", subtitle = "Using K-means Clustering")


# Entendendo o gráfico acima.
# Os clusters 4 e 5 representa os clientes com salários médios.
# O cluster 1 representa os clientes com maiores salários e com maior pontuação.
# O cluster 6 são os clientes com os menores salários e com a menor pontuação.
# O cluster 2 representa os clientes com maiores pontuações e menor salário.
# O cluster 3 representa os clientes com maiores salários, porém com baixa pontuação.
