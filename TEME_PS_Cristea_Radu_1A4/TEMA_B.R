#B1


interior=function(x1, x2, x3, R, r)
{
  return((x3^2 + (sqrt(x1^2 + x2^2) - R)^2) < r^2)
}

MC_volum=function(N, R, r) 
{
  C_N = 0; 
  for (i in 1:N)
  {
    x1=runif(1, -R-r, R+r);
    x2= runif(1, -R-r, R+r);
    x3=runif(1, -r, r);
    if (interior(x1, x2, x3, R, r)) 
    {
      C_N = C_N + 1;
    }
  } 
  torus_volume = (C_N / N) * 8 * (R+r) * (R+r) * r;
  return(torus_volume)
}

R=10
r=3
N_values=c(10000, 20000, 50000) 

for (N in N_values) 
{
  estimated_volume=MC_volum(N, R, r); 
  exact_volume=2 * pi^2 * R * r^2 
  eroare_absoluta=abs(estimated_volume - exact_volume);
  eroare_relativa=eroare_absoluta / exact_volume;

  cat("Estimarea volumului pentru N =", N, ":", estimated_volume, "\n")
  cat("Volumul exact:", exact_volume, "\n")
  cat("Eroarea absoluta pentru N=", N, ":", eroare_absoluta, "\n");
  cat("Eroarea relativa pentru N=", N, ":", eroare_relativa, "\n");
}


#B2


interior_tr=function(x, y) 
{
  return((y >= 0) & (y <= 2 * x) & (y <= 6 - 3 * x))
}

MC_area_tr=function(N, a, b, c, d) 
{
  C_N = 0;
  for (i in 1:N) 
  {
    x=runif(1, a, b)
    y=runif(1, c, d)
    if (interior_tr(x, y)) 
    {
      C_N = C_N + 1;
    }
  }
  patrat = (b - a) * (d - c);
  triunghi = (C_N / N)*patrat;
  return(triunghi)
}

N=20000
a=0
b=2
c=0
d=12 / 5
estimated_area=MC_area_tr(N, a, b, c, d)
cat("Estimarea ariei triunghiului:", estimated_area, "\n")
exact_area=0.5 * (2 - 0) * (12 / 5) 
cat("Aria exacta a triunghiului:", exact_area, "\n")
eroare_absoluta=abs(estimated_area - exact_area)
eroare_relativa=eroare_absoluta / exact_area
cat("Eroarea absoluta:", eroare_absoluta, "\n")
cat("Eroarea relativa:", eroare_relativa, "\n")


#B3

#a

MC_integral=function(N) 
{
  sum = 0;
  for (i in 1:N) 
  {
    x=runif(1, -1, 1)
    sum = sum + (2*x - 1) / (x*x - x - 6);
  }
  val=2*sum / N; #1-(-1)
  return(val)
}

est_integral=MC_integral(20000)
cat("Estimarea integralei:", est_integral, "\n")

exact_integral=log(3,base = exp(1)) - log(2,base = exp(1))
cat("Valoarea exacta a integralei:", exact_integral, "\n")
eroare_absoluta=abs(est_integral - exact_integral)
eroare_relativa=eroare_absoluta / abs(exact_integral)
cat("Eroarea absoluta:", eroare_absoluta, "\n")
cat("Eroarea relativa:", eroare_relativa, "\n")


#b

MC_integral=function(N) 
{
  sum = 0;
  for (i in 1:N) 
  {
    x=runif(1, 3, 11)
    sum = sum + (x + 4) / ((x - 3)^(1/3));
  }
  val = 8 * sum / N; 
  return(val)
}

est_integral=MC_integral(20000)
cat("Estimarea integralei:", est_integral, "\n")

exact_integral=61.2
cat("Valoarea exacta a integralei:", exact_integral, "\n")
eroare_absoluta=abs(est_integral - exact_integral)
eroare_relativa=eroare_absoluta / abs(exact_integral)
cat("Eroarea absoluta:", eroare_absoluta, "\n")
cat("Eroarea relativa:", eroare_relativa, "\n")


#c 

f=function(x) 
{
  return(x * exp(-x^2))
}

MC_integral=function(N) 
{
  sum = 0;
  for (i in 1:N) 
  {
    u=runif(1, 0, 1)
    x = -log(1-u);
    sum = sum + f(x);
  }
  val = sum / N; 
  return(val)
}

est_integral= MC_integral(20000)
cat("Estimarea integralei:", est_integral, "\n")

exact_integral=1/2
cat("Valoarea exacta a integralei:", exact_integral, "\n")
eroare_absoluta=abs(est_integral - exact_integral)
eroare_relativa=eroare_absoluta / abs(exact_integral)
cat("Eroarea absoluta:", eroare_absoluta, "\n")
cat("Eroarea relativa:", eroare_relativa, "\n")


#B4 

#a

Crestere=function(initial, n, p, q, target, nr_simulari) 
{
  ani=vector()
  for (i in 1:nr_simulari)
  {
    m=initial;
    nr_ani=0;
    while (m < target)
    {
      new= rbinom(1, n, p) 
      retained=rbinom(1, m, 1 - q) 
      m = retained + new;
      nr_ani = nr_ani + 1; 
    }
    ani[i] = nr_ani;
  }
  
  medie=mean(ani);
  return(medie) 
}

med=Crestere(10000, 1000, 0.25, 0.01, 15000, 1000)
cat("medie",med);

#b

Probabilitate=function(initial, n, p, q, target, nr_simulari) 
{
  nr_ani=40 + 10/12; 
  months=round(nr_ani* 12) 
  count=0;
  for (i in 1:nr_simulari) 
  {
    u=initial;
    for (m in 1:months) 
    {
      new=rbinom(1, n, p) 
      retained=rbinom(1, u, 1 - q)
      u=retained + new 
    }
    if (u>=target) 
    {
      count = count + 1; 
    }
  }
  return (count / nr_simulari)
}

pr=Probabilitate(10000, 1000, 0.25, 0.01, 15000, 1000)
cat("Probabilitate:", pr);

#c

Probabilitate=function(initial, n, p, q, target, target_timp, nr_simulari, pr, error_m) 
{
  months=round(target_timp * 12) 
  prob=vector()
  for (i in 1:nr_simulari) 
  {
    u = initial; 
    for (m in 1:months) 
    {
      new=rbinom(1, n, p) 
      retained <- rbinom(1, u, 1 - q) 
      u=retained + new 
    }
    if (u >= target) 
    {
      prob[i]=1
    } 
    else 
    {
      prob[i]=0
    }
  }
  prob_med=mean(prob) 
  prob_sd= sd(prob) 
  z=qnorm((1 + pr) / 2) 
  error=z * prob_sd / sqrt(nr_simulari) 
  if (error <= error_m) 
  {
    x=c(prob_med, error) 
    return (x)
  } 
  else 
  {
    return ("error")
  }
}

val=Probabilitate(10000, 1000, 0.25, 0.01, 15000, 40 + 10/12, 10000, 0.99, 0.01)
cat("Probabilitate",val);