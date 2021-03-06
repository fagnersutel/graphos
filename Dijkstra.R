#https://uqkdhanj.wordpress.com/2015/02/10/dijkstras-shortest-pathway-algorithm/

### Cria-se uma matriz de distancias
### Defina todas as conex�es imposs�veis entre n�s para um n�mero grande
### Como o algoritmo est� procurando um m�nimo, as dist�ncias muito grandes nunca ser�o selecionadas
#inicial = 999

#crio 49 dist�ncias aleat�reas referente a distancia entre 7 pontos
inicial = runif(49, min=0, max=999)
#Crio uma matriz 7x7 com ops valores aleat�rios
S=matrix(inicial,7,7)
#visualizo a matriz
S
#Seto para zero os valores de um ponto a ele mesmo
S[1,1]=0
S[2,2]=0
S[3,3]=0
S[4,4]=0
S[5,5]=0
S[6,6]=0
S[7,7]=0
S[1,2]=S[1,2]/33
S[1,3]=S[1,3]/33
S[2,5]=S[2,5]/33
S[3,4]=S[3,4]/33
S[4,5]=S[4,5]/33
S[4,6]=S[4,6]/33
S[5,6]=S[5,6]/33
S[6,7]=1
#Verifico o ajuste
S
#Crio a representa��o visual
graphdj <- graph.adjacency(S, weighted=TRUE)
#ploto a representa��o
plot(graphdj)
### Lisa e par�metros da fun��oList of input parameters for function
n=sample(1:7, 1, replace=F)  #Numero de nos
n
v=sample(1:7, 1, replace=F) #N� de origen
v
dest=n #N� de destino
dest
cost=S #Matriz de dist�ncias
cost

### Algor�timo Dijkstra's
dijkstra=function(n,v,cost,dest){
  
  #Criar vari�veis vazias para armazenar dados
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # Para cada n� na rede
  for(i in 1:n){
    prev[i] = -1
    #= Distancia para cada n� inicial v at� todos n�se i na rede
    dest[i] = cost[v,i] 
  }
  
  #Inicializar p contador que acompanha o n�mero de atapas ao longo da rede
  count=2
  
  # La�o at� chegar ao n� de destino  n
  while(count <= n){
    #min=999
    min=max(inicial)
    
    # loop sobre cada n�
    for(w in 1:n){
      #se o novo caminho for menor do que o mais pequeno existente e a flag [w] � 
      #igual a zero (aka, n�s ainda n�o inclu�mos esse n� na rota)
      if(dest[w] < min && !flag[w]){
        # Substitua o m�nimo pelo novo caminho mais curto u atualize o contador
        min=dest[w]
        u=w
      }
    }
    flag[u] = 1 #Aponto o destino
    count = count+1
    
    # loop over each node again keeping in mind where we have already been
    for(w in 1:n){
      #Se a nova rota � mais curta que a anterior
      if((dest[u]+cost[u,w] < dest[w]) && !flag[w]){
        dest[w]=dest[u]+cost[u,w] #Atualizamos a distancia ao destino
        prev[w]=u #Tra�ar o n� visitado
      }
    }
  }
  return(prev)
}


### Fun��o que cria o  path
savepath = function(f,x){
  path=x
  while(f[x] != -1){
    path=c(path,f[x])
    x=f[x]
    savepath(f,x)
  }
  path=c(path,1)
  return(path)
}

### Rodamos o algor�timo Dijkstra's com a matriz de dist�ncias
prev = dijkstra(n,v,cost,dest)
#Crio o caminho
path = savepath(prev,dest)

### Printo o caminho
path



### Create a distance matrix S.
### Set all the impossible connections between nodes to a large number.
### Because the algorithm is looking for a minimum, very large distances will never be selected

S=matrix(999,7,7)
S[1,2]=1
S[1,3]=2
S[2,5]=2
S[3,4]=1
S[4,5]=1
S[4,6]=3
S[5,6]=2
S[6,7]=1
#Crio a representa��o visual
graphdj <- graph.adjacency(S, weighted=TRUE)
#ploto a representa��o
plot(graphdj)
### List of input parameters for function
n=length(S[,1]) #number of nodes
v=1 #source node
v
dest=n #destination node
dest
cost=S #distance matrix

### Dijkstra's algorithm
dijkstra=function(n,v,cost,dest){
  
  #create empty variables to store data
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # for every node in the network
  for(i in 1:n){
    prev[i] = -1
    dest[i] = cost[v,i] #= distance from start node v to every other node i in the network
  }
  
  #initialise counter which keeps track of number of steps through network
  count=2
  
  # until we have reached our destination node n
  while(count <= n){
    min=999
    
    # loop over each node
    for(w in 1:n){
      #if the new path is less long than the existing smallest one and flag[w] is equal to zero (aka we've not already incuded that node in route)
      if(dest[w] < min && !flag[w]){
        # overwrite the minimum with the new shortest path and update counter
        min=dest[w]
        u=w
      }
    }
    flag[u] = 1 #indicate that we go to this site
    count = count+1
    
    # loop over each node again keeping in mind where we have already been
    for(w in 1:n){
      #if the new route is shorter than the previous route
      if((dest[u]+cost[u,w] < dest[w]) && !flag[w]){
        dest[w]=dest[u]+cost[u,w] #update the distance to destination
        prev[w]=u #keep track of the node visited
      }
    }
  }
  return(prev)
}

### create function which returns path
savepath = function(f,x){
  path=x
  while(f[x] != -1){
    path=c(path,f[x])
    x=f[x]
    savepath(f,x)
  }
  path=c(path,1)
  return(path)
}

### Run Dijkstra's algorithm with our distance matrix
prev = dijkstra(n,v,cost,dest)
path = savepath(prev,dest)

### Print path
path







#Ponto de Partida inicial
install.packages('igraph')
library(igraph)

g <- graph.ring(10)
g
shortest.paths(g)
get.shortest.paths(g,2, 5)
get.all.shortest.paths(g, 1, 5:7)
average.path.length(g)
## Weighted shortest paths
el <- matrix(nc=3, byrow=TRUE,
             c(0,1,0, 0,2,2, 0,3,1, 1,2,0, 1,4,5, 1,5,2, 2,1,1, 2,3,1,
               2,6,1, 3,2,0, 3,6,2, 4,5,2, 4,7,8, 5,2,2, 5,6,1, 5,8,1,
               5,9,3, 7,5,1, 7,8,1, 8,9,4) )
teste <- as.list(t(el[,1:2]))
g2 <- add.edges(graph.empty(10), teste, weight=el[,3])
shortest.paths(g2, mode="out")
