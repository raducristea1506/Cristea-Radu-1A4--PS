
#I2
parabola_area=function(){
  f=function(x)-2*x*x+5*x-2

  exact_area=integrate(f,0,2)$value
  n=10000
  x=runif(n, 0, 2)
  y=runif(n, 0, 9/8)
  inside=y<=f(x)
  prop_inside=mean(inside)
  area=(2-0)*9/8*prop_inside
  rel_error=abs(exact_area - area) / exact_area
  
  result=list(area=area, exact_area=exact_area, rel_error = rel_error)
  return(result)
}
result=parabola_area()

print(result$area)
print(result$exact_area)
print(result$rel_error)

# II.1b
f=function(x) exp(x)
exact=integrate(f,1,4)$value
n=10000
x=seq(1,4,length.out=n+1)
y=f(x)
approx=3/(2*n)*(sum(y)-y[1]-y[n+1]+2*sum(y[2:n]))
abs_error =abs(exact-approx)
rel_error=abs_error/exact*100
print(exact)
print(approx)
print(abs_error)
print(rel_error)


#II2
f=function(u) 
{
  exp(-2*u*u)
}
N=50000
x=rexp(N,3)
estimare=sum(f(x))/(3*N)
true_value=sqrt(pi)/8
error=abs(estimare-true_value)
print(estimate)
print(true_value)
print(error)

#III 2

N=10000
s=0
for (i in 1:N)
{
  x=1/4*rexp(1,4)+3/4*rexp(1,12)
  s=s+x
}
expectation=s/N
print(expectation)


