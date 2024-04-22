tablou =  read.csv("life_expect.csv", header=T, sep=',')

male=tablou[['male']]
min1=min(male)
max1=max(male)
interval = seq(66.1, 86, 6)
hist(male, breaks=7, right=T, freq=F)

female=tablou[['female']]
min2=min(female)
max2=max(female)


interval1 = seq (75, 87, 1.2)
hist(female, breaks=interval1, right=T, freq=F)

