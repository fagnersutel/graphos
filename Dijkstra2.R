#http://sachaepskamp.com/documentation/qgraph/centrality.html
#https://toreopsahl.com/2010/04/21/article-node-centrality-in-weighted-networks-generalizing-degree-and-shortest-paths/
#install.packages('qgraph')
library(qgraph)
set.seed(1)
adj <- matrix(sample(0:1,10^2,TRUE,prob=c(0.8,0.2)),nrow=10,ncol=10)
adj
Q <- qgraph(adj)
Q
centrality(Q)
a = centrality(Q)
tab = a$ShortestPathLengths
tab
set.seed(Sys.time())

### Lisa e parâmetros da funçãoList of input parameters for function
n=sample(1:10, 1, replace=F)  #Numero de nos
n
v=sample(1:10, 1, replace=F) #Nó de origen
v
dest=n #Nó de destino
dest
cost=adj #Matriz de distâncias
cost
inicial = 10
### Algorítimo Dijkstra's
dijkstra=function(n,v,cost,dest){
  
  #Criar variáveis vazias para armazenar dados
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # Para cada nó na rede
  for(i in 1:n){
    prev[i] = -1
    #= Distancia para cada nó inicial v até todos nóse i na rede
    dest[i] = cost[v,i] 
  }
  
  #Inicializar p contador que acompanha o número de atapas ao longo da rede
  count=2
  
  # Laço até chegar ao nó de destino  n
  while(count <= n){
    #min=999
    min=max(inicial)
    
    # loop sobre cada nó
    for(w in 1:n){
      #se o novo caminho for menor do que o mais pequeno existente e a flag [w] é 
      #igual a zero (aka, nós ainda não incluímos esse nó na rota)
      if(dest[w] < min && !flag[w]){
        # Substitua o mínimo pelo novo caminho mais curto u atualize o contador
        min=dest[w]
        u=w
      }
    }
    flag[u] = 1 #Aponto o destino
    count = count+1
    
    # loop over each node again keeping in mind where we have already been
    for(w in 1:n){
      #Se a nova rota é mais curta que a anterior
      if((dest[u]+cost[u,w] < dest[w]) && !flag[w]){
        dest[w]=dest[u]+cost[u,w] #Atualizamos a distancia ao destino
        prev[w]=u #Traçar o nó visitado
      }
    }
  }
  return(prev)
}


### Função que cria o  path
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

### Rodamos o algorítimo Dijkstra's com a matriz de distâncias
prev = dijkstra(n,v,cost,dest)
#Crio o caminho
path = savepath(prev,dest)

### Printo o caminho
v
dest
path


S=matrix(999,7,7)
S
S[1,2]=1
S[1,3]=2
S[2,5]=2
S[3,4]=1
S[4,5]=1
S[4,6]=3
S[5,6]=2
S[6,7]=1
Q <- qgraph(S)
Q



##############
# Load tnet
install.packages('tnet')
library(tnet)

# Load network
net <- cbind(
  i=c(1,1,2,2,2,2,3,3,4,5,5,6),
  j=c(2,3,1,3,4,5,1,2,2,2,6,5),
  w=c(4,2,4,4,1,2,2,4,1,2,1,1))
net
# Calculate degree centrality (note that alpha is included in the list of measures)
degree_w(net, measure=c("degree", "output", "alpha"), alpha=0.5)

# Calculate closeness centrality
closeness_w(net, alpha=0.5)

# Calculate betweenness centrality
betweenness_w(net, alpha=0.5)


#https://toreopsahl.com/tnet/weighted-networks/clustering/
#https://toreopsahl.com/tnet/weighted-networks/clustering/

# Load networks
data(tnet)
net <- list()
net[[1]] <- Freemans.EIES.net.1.n48
net[[2]] <- Freemans.EIES.net.2.n48
net[[3]] <- Freemans.EIES.net.3.n32

net[[4]] <- Cross.Parker.Consulting.net.info
net[[5]] <- Cross.Parker.Consulting.net.value
net[[6]] <- Cross.Parker.Manufacturing.net.info
net[[7]] <- Cross.Parker.Manufacturing.net.aware
net[[8]] <- celegans.n306.net
net[[9]] <- USairport.n500.net
net[[10]] <- Newman.Condmat.95.99.net.1mode.wNewman

# Calculate values
output <- data.frame(CGT0=NaN, Cw=NaN, Ratio=NaN)
for(i in 1:length(net)) 
    output[i,c("CGT0","Cw")] <- clustering_w(net[[i]], measure=c("bi","am"))
    output[,"Ratio"] <- output[,"Cw"]/output[,"CGT0"]
