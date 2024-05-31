#TEMA D
#D1
D1=function(fis, alfa) 
{
   s=read.csv(fis, header=TRUE,text=FALSE)
  numeric_values=as.numeric(s$probabilitati)
  numeric_values=numeric_values[!is.na(numeric_values)]  
  med=mean(numeric_values)
  n=length(numeric_values)
  sigma=sqrt(92.16)  
  critical_z=qnorm((1 - alfa / 2), 0, 1)
  a=med - critical_z * sigma / sqrt(n)
  b=med + critical_z * sigma / sqrt(n)
  interval=c(a, b)
  return(interval)
}
interval_95= D1("probabilitati.csv", 0.05)
interval_99=D1("probabilitati.csv", 0.01)
print(interval_95)
print(interval_99)

#2
D2=function(fis, alfa)
{
  scanat=scan(fis, what = "", sep = "\n", quiet = TRUE)
  numere=as.numeric(scanat)
  numere=numere[!is.na(numere)]
  
  n=length(numere)
  s=sd(numere)
  xn=mean(numere)
  se=s/sqrt(n) # eroarea standard 
  critical_t=qt((1-alfa/2),(n - 1)) 
  a=xn-critical_t*se
  b=xn+critical_t*se
  interval= c(a, b)
  return(interval)
}
v_95=D2("statistica.csv", 0.05)
v_99=D2("statistica.csv", 0.01)
print(v_95)
print(v_99)


#3
test_proportion=function(alfa, n, succese, p0, tip_ipoteza)
{
  p_prim=succese/n;
  z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n);
  print(paste("z_score:", z_score));
  
  if(tip_ipoteza=="r")
  {
    critical_z=qnorm((1-alfa));
    print(paste("critical_z:", critical_z));
    if(z_score<=critical_z)
    {
      print("Ipoteza nula nu se poate respinge");
    }
    else
    {
      print("Ipoteza nula se respinge");
    }
  }
  if(tip_ipoteza=="l")
  {
    critical_z=qnorm(alfa);
    print(paste("critical_z:", critical_z));
    if(z_score>=critical_z)
    {
      print("Ipoteza nula nu se poate respinge");
    }
    else
    {
      print("Ipoteza nula se respinge");
    }
  }
  
  if(tip_ipoteza=="s")
  {
    critical_z=qnorm((1-alfa/2));
    print(paste("critical_z:", critical_z));
    if(abs(z_score)<=critical_z)
    {
      print("Ipoteza nula nu se poate respinge");
    }
    else
    {
      print("Ipoteza nula se respinge");
    }
  }
}

print("Test la nivel de semnificație 5%:")
test_proportion(0.05, 100, 14, 0.15, "s")
print("Test la nivel de semnificație 1%:")
test_proportion(0.01, 100, 14,0.15, "s")
#Deducem ca schimbarea structurii temelor de laborator nu a avut un efect semnificativ





