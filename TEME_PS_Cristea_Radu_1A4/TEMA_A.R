#A1

#a

Calcul_prob=function(lambda,p,n,k,m)
{
  prob_pois=dpois(k:m,lambda)
  prob_geom=dgeom(k:m,p)
  prob_binom=dbinom(k:m,n,p)
  rez=list(prob_pois=prob_pois,prob_geom=prob_geom, prob_binom = prob_binom)
  return (rez)
}

Calcul_prob(2,1/2,6,2,5)

#b

Grafic=function(lambda, p, n, k, m)
{
  prob_pois=dpois(k:m, lambda)
  prob_geom=dgeom(k:m, p)
  prob_binom=dbinom(k:m, n, p)
  grafice=cbind(prob_geom, prob_pois, prob_binom) #matrice cu 3 vectori
  barplot(grafice, beside=T) 
}
Grafic(2, 1/2, 6, 2, 5)

#c

Cauta_val=function(lambda)
{
  val=1 - 10^(-6); 
  k=0; 
  prob=ppois(k, lambda); 
  while (prob <= val)
  {
    k=k+1; #creste nr
    prob=ppois(k, lambda); 
  }
  return(k)
}

Cauta_val(3)

#A2

#a

Frecvente=function()
{
  data=read.csv("note_PS.csv", header=TRUE) 
  P=data[['P']] 
  S=data[['S']] 
  fr_P=table(P) 
  fr_S=table(S) 
  fr_abs_P=as.vector(fr_P) 
  fr_abs_S=as.vector(fr_S)
  fr_rel_P=fr_abs_P/length(P)
  fr_rel_S=fr_abs_S/length(S)
  med_P=mean(P)
  med_S=mean(S)
  rez=list(fr_abs_P=fr_abs_P,fr_abs_S=fr_abs_S,fr_rel_P=fr_rel_P,fr_rel_S=fr_rel_S,med_P=med_P,med_S=med_S)
  return(rez)
}

Frecvente()



#b

eliminare_val_aberante=function(data, col)
{
  sample=data[[col]]
  med=mean(sample) 
  dev=sd(sample) 
  v=vector()
  j=0
  for (i in 1:length(sample)) 
  {
    if (sample[i] >= med-2*dev & sample[i] <= med+2*dev) 
    { 
      j = j + 1 
      v[j] = sample[i]
    }
  }
  return(v)
}

f=function(fisier, esantion)
{
  data=read.csv(fisier, header=TRUE) 
  sample = eliminare_val_aberante(data, esantion)
  interval=seq(1, 10, 1) 
  hist(sample, breaks=interval, right=T, freq=F)# historigrama 
}

f("note_PS.csv", 'P')
f("note_PS.csv", 'S')






