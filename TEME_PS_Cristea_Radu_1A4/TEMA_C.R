#C1

#a

Permutare=function(n) 
{
  U=runif(n) 
  permutation=order(U)
  return(permutation)
}

Permutare(5)

#b

random=function()
{
  return(sample(c(0, 1), 1)) 
}

Comparare=function(Wi,Wj) 
{
  Lij=min(length(Wi),length(Wj))
  for(h in 1:Lij) 
  {
    if(Wi[h]<Wj[h])
    {
      return(-1) 
    }
    else if(Wi[h]>Wj[h])
    {
      return(1) 
    }
  }
  while(TRUE) 
  {
    if (length(Wi) < length(Wj)) 
    {
      Wi=c(Wi, random()) 
    } 
    else if (length(Wi) > length(Wj)) 
    {
      Wj=c(Wj, random()) 
    } 
    else 
    {
      Wi=c(Wi, random())
      Wj=c(Wj, random())
    }
    
    if (Wi[length(Wi)] < Wj[length(Wj)]) 
    {
      return(-1)  
    } 
    else if (Wi[length(Wi)] > Wj[length(Wj)]) 
    {
      return(1)   
    }
  }
}
Wi=sample(c(0, 1), 5, replace = TRUE)
Wj=sample(c(0, 1), 3, replace = TRUE)
print(Wi)
print(Wj)
Comparare(Wi, Wj)

#c 

QS=function(cuv)
{
  if (length(cuv) <= 1) 
  {
    return(cuv)
  }
  pivot_index=sample(1:length(cuv), 1) 
  pivot=cuv[pivot_index] 
  stanga=list()
  dreapta=list()
  for (word in seq_along(cuv))
    if(word!=pivot_index)
    {
      if (Comparare(cuv[[word]], pivot)==-1) 
      {
        stanga=c(stanga, cuv[[word]])
      } 
      else 
      {
        dreapta=c(dreapta, cuv[[word]])
      } 
    }
  sorted_stg=QS(stanga)
  sorted_drp=QS(dreapta)
  return(c(sorted_stg, pivot, sorted_drp))
}

cuv=list("1100", "1010", "0110", "1001", "0011", "0101")
print(cuv)
sorted=QS(cuv)
print(sorted)

#d

generate_random_binary_string=function(k) 
{ 
  return(paste(sample(c(0, 1), k, replace = TRUE), collapse = ""))
}

Perm_aleatoare=function(n, k) 
{
  for(i in 1:n)
    cuv[i]=generate_random_binary_string(k)
  print(cuv)
  words=QS(cuv)
  sorted_indices=sapply(words, function(str)
    { 
    match(str, cuv) 
  })
  return(sorted_indices)
}

n=5
k=4
permutation=Perm_aleatoare(n, k)
print(permutation)

#C2

#a

Taiere=function(graph, n) 
{
  V=V(graph)
  m=length(E(graph)) 
  A=sample(V, n, replace = FALSE) 
  ct=0 
  for (e in E(graph)) 
  {
    ends=ends(graph, e) 
    if ((ends[1] %in% A && !(ends[2] %in% A)) || (!(ends[1] %in% A) && ends[2] %in% A)) 
    {
      ct = ct + 1
    }
  }
  return(ct)
}

library(igraph)

#10 noduri 15 muchii
graph=erdos.renyi.game(10, 0.3)
cut_size=Taiere(graph, 5)
print(cut_size)

#b

Taiere_maxima=function(graph, n, nr)
{
  max_cut_size = 0;
  for (i in 1:nr)
  {
    cut_size=Taiere(graph, n) 
    if (cut_size > max_cut_size) 
    {
      max_cut_size =cut_size;
    }
  }
  return(max_cut_size) 
}
library(igraph)  
graph=erdos.renyi.game(10, 0.3)
max_cut=Taiere_maxima(graph, 5, 1000)
print(max_cut)