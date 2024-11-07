data(anscombe)
print(anscombe)
#>    x1 x2 x3 x4    y1   y2    y3    y4
#> 1  10 10 10  8  8.04 9.14  7.46  6.58
#> 2   8  8  8  8  6.95 8.14  6.77  5.76
#> 3  13 13 13  8  7.58 8.74 12.74  7.71
#> 4   9  9  9  8  8.81 8.77  7.11  8.84
#> 5  11 11 11  8  8.33 9.26  7.81  8.47
#> 6  14 14 14  8  9.96 8.10  8.84  7.04
#> 7   6  6  6  8  7.24 6.13  6.08  5.25
#> 8   4  4  4 19  4.26 3.10  5.39 12.50
#> 9  12 12 12  8 10.84 9.13  8.15  5.56
#> 10  7  7  7  8  4.82 7.26  6.42  7.91
#> 11  5  5  5  8  5.68 4.74  5.73  6.89
attach(anscombe)

layout(matrix(1:4,2), respect=TRUE)

m1=lm(y1~x1)
plot(anscombe$x1, anscombe$y1)
abline(a=m1$coefficients[1],b=m1$coefficients[2],col="red")

m2=lm(y2~x2)
plot(anscombe$x2, anscombe$y2)
abline(a=m2$coefficients[1],b=m2$coefficients[2],col="red")

m3=lm(y3~x3)
plot(anscombe$x3, anscombe$y3)
abline(a=m3$coefficients[1],b=m3$coefficients[2],col="red")

m4=lm(y4~x4)
plot(anscombe$x4, anscombe$y4)
abline(a=m4$coefficients[1],b=m4$coefficients[2],col="red")

data(CO2)
d = CO2[CO2$Type=="Quebec", ]
head(d)
#>   Plant   Type  Treatment conc uptake
#> 1   Qn1 Quebec nonchilled   95   16.0
#> 2   Qn1 Quebec nonchilled  175   30.4
#> 3   Qn1 Quebec nonchilled  250   34.8
#> 4   Qn1 Quebec nonchilled  350   37.2
#> 5   Qn1 Quebec nonchilled  500   35.3
#> 6   Qn1 Quebec nonchilled  675   39.2


m1=lm(d$uptake~d$conc)
m2=lm(d$uptake~log10(d$conc))
summary(m1)
summary(m2)#le 2eme modèle à un R2 sup dû à la fonction log

layout(matrix(1:2, 1), respect=TRUE)

plot(d$conc, d$uptake, main="uptake~conc")
abline(m1,col="red")

plot(log10(d$conc), d$uptake, main="uptake~log10(conc)")
abline(m2,col="red")

