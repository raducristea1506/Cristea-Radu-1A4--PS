outliers_mean=function(sample)
{
  m=mean(sample)
  s=sd(sample)
  outliers=vector()
  j=0
  for (i in 1:length(sample))
  {
    if(sample[i]<m-2*s | sample[i]>m+2*s)
    {
      j=j+1
      outliers[j]=sample[i]
    }
  }
  return(outliers)
}

outliers_iqr=function(sample)
  {
  Q1=as.vector(quantile(sample))[1+1]
  Q3=as.vector(quantile(sample))[3+1]
  IQR=Q3-Q1
  outliers=vector()
  j=0
  for (i in 1:length(sample))
  {
    if (sample[i]<Q1-1.5*IQR | sample[i]>Q3+1.5*IQR)
    {
      j=j+1
      outliers[j]=sample[i]
    }
  }
  return(outliers)
}
sample=scan("sample2.txt")
outliers_iqr(sample)
outliers_mean(sample)
summary(sample)

