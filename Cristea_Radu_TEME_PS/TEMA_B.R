#B1

B1=function(N,R,r)
{
  ct=0
  for(i in 1:N)
  {
    x1=runif(1,-R-r,R+r)
    x2=runif(1,-R-r,R+r)
    x3=runif(1,-r,r)
    
    if((x3^2 + (sqrt(x1^2+x2^2)-R)^2)<r^2) 
    {
      ct=ct+1
    }
  }
  volum_torus=(ct/N)*8*(R+r)*(R+r)*r
  return (volum_torus)
}
R=10
r=3
v=c(10000,20000,50000)
for(N in v)
{
  volum_exact=2* pi^2 * R*r^2
  estimare_vol=B1(N,R,r)
  absolut=abs(estimare_vol-volum_exact)
  relativ=absolut/volum_exact
  
  cat("Estimarea volumului pentru N=",N,":",estimare_vol,"\n")
  cat("Volumul exact al torului:", volum_exact, "\n")
  cat("Eroarea absoluta pentru N =", N, ":", absolut, "\n");
  cat("Eroarea relativa pentru N =", N, ":", relativ, "\n");
}


#B2

B2=function(n,a,b,c,d)
{
  ct=0
  for(i in 1:n)
  {
    x=runif(1,a,b)
    y=runif(1,c,d)
    if((y>=0) & (y<=2*x) & (y<=6-3*x))
    {
      ct=ct+1
    }    
  }
  interior=ct/n
  arie_rectangulara=(b-a)*(d-c)
  estimare=interior*arie_rectangulara
  return(estimare)
  
}
a=0
b=5
c=0
d=6
n=20000
arie_estimata=B2(n,a,b,c,d)
cat("Estimarea ariei triunghiului:",arie_estimata, "\n")
arie_exacta=0.5*(2-0)*(12/5)
cat("Aria exacta a triunghiului:", arie_exacta, "\n")
absolut=abs(arie_estimata-arie_exacta)
relativ=absolut/arie_exacta
cat("Eroare absoluta:",absolut,"\n")
cat("Eroare relativa:",relativ,"\n")


#B3
B3.a=function(n)
{
  sum=0
  for(i in 1:n)
  {
    u=runif(1,-1,1);
    sum=sum+(2*u-1)/(u^2-u-6)
  }
  estimare=(2*sum/n);
  val_exacta=log(3,base=exp(1))-log(2,base=exp(1))
  absolut=abs(val_exacta-estimare)
  relativ=absolut/val_exacta
  rezultat=list(Estimare=estimare,Absolut=absolut,Relativ=relativ)
  return(rezultat)
}
B3.a(20000)
B3.b=function(n)
{
  sum=0
  for(i in 1:n)
  {
    u=runif(1,3,11);
    sum=sum+((u+4)/(u-3)^(1/3));
  }
  estimare=(8*sum/n)
  cat("Estimarea integralei:", estimare, "\n")
  val_exacta=61.2
  cat("Valoarea exacta a integralei:", val_exacta, "\n")
  absolut=abs(estimare-val_exacta)
  relativ=absolut/abs(val_exacta)
  rezultat=list(Estimare=estimare,Absolut=absolut,Relativ=relativ)
  return (rezultat)
}
B3.b(20000)
B3.c=function(n)
{
  sum=0
  for(i in 1:n)
  {
    u = rexp(1, 1);
    sum = sum + u*exp(-u*u)/exp(-u);
  }
  estimare=sum/n
  cat("Estimarea integralei:", estimare, "\n")
  val_exacta=0.5
  cat("Valoarea exacta a integralei:", val_exacta, "\n")
  absolut=abs(estimare-val_exacta)
  relativ=absolut/abs(val_exacta)
  rezultat=list(Estimare=estimare,Absolut=absolut,Relativ=relativ)
  return (rezultat)
}
B3.c(20000)


# ex B4

#a)

B4a=function(utilizatori_initial,n,p,q,utilizatori_doriti,nr_simulari)
{
  ani=vector();
  
  for(i in 1:nr_simulari)
  {
    utilizatori=utilizatori_initial
    nr_ani=0
    
    while(utilizatori<utilizatori_doriti)
    {
      utilizatori_noi=rbinom(1,n,p)
      utilizatori_retrasi=rbinom(1,utilizatori,1-q)
      utilizatori=utilizatori_retrasi+utilizatori_noi
      nr_ani=nr_ani+1
    }
    ani[i]=nr_ani
    
  }
  medie=mean(ani)
  return(medie)
}
utilizatori_initial=10000
n=1000
p=0.25
q=0.01
utilizatori_doriti=15000
nr_simulari=1000
estimare_ani=B4a(utilizatori_initial,n,p,q,utilizatori_doriti,nr_simulari)
cat("Pentru a avea 15000 de utilizatori vor trece in medie:",estimare_ani,"\n")


#B4b

B4b=function(utilizatori_initial,n,p,q,utilizatori_doriti,nr_simulari)
{
  ani_si_luni=40+10/12
  luni=round(ani_si_luni*12)
  simulari_reusite=0
  for(i in 1:nr_simulari)
  {
    utilizatori=utilizatori_initial
    for(luna in 1:luni)
    {
      utilizatori_noi=rbinom(1,n,p)
      utilizatori_retrasi=rbinom(1,utilizatori,1-q)
      utilizatori=utilizatori_retrasi+utilizatori_noi
      
    }   
    
    if(utilizatori >= utilizatori_doriti)
      simulari_reusite=simulari_reusite+1
    
  }
  return(simulari_reusite/nr_simulari)
  
}
utilizatori_initial=10000
n=1000
p=0.25
q=0.01
utilizatori_doriti=15000
nr_simulari=1000
probabilitate=B4b(utilizatori_initial,n,p,q,utilizatori_doriti,nr_simulari)
cat("Probabilitatea ca dupa 40 de ani si 10 luni sa fie cel putin 15000 de utilizatori:",probabilitate,"\n") 


#B4c
B4c=function(utilizatori_initial,n,p,q,utilizatori_doriti,ani_doriti,nr_simulari,speranta,eroare)
{
  luni=round(ani_doriti*12)
  probabilitati=vector()
  
  for(i in 1:nr_simulari)
  { 
    utilizatori=utilizatori_initial
    for(luna in 1:luni)
    {
      utilizatori_noi=rbinom(1,n,p)
      utilizatori_retrasi=rbinom(1,utilizatori,1-q)
      utilizatori=utilizatori_retrasi+utilizatori_noi
    }
    
    if(utilizatori>=utilizatori_doriti)
      probabilitati[i]=1
    else
      probabilitati[i]=0
  }
  prob_med=mean(probabilitati)
  prob_sd=sd(probabilitati)
  z=qnorm((1+speranta)/2)
  err=z*prob_sd/sqrt(nr_simulari)
  if(err<=eroare)
  {
    x=c(prob_med,err)
    return(x)
    
  }
  else
  {
    return("err")
  }
}
val=B4c(10000,1000,0.25,0.01,15000,40+10/12,10000,0.99,0.01)
cat("Probabilitate",val)




