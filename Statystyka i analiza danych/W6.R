#WYKŁAD6
#P1
#Sposób 1
typ1  = c(2830, 2840, 2800, 2880,2820)
typ2 = c(2790, 2720, 2770, 2780, 2760)
alpha = 0.99
n1 = length(typ1)
n2 = length(typ2)
xbar = mean(typ1)
ybar = mean(typ2)
s1sq = var(typ1)
s2sq = var(typ2)
df = n1+n2-2 #degrees of freedom
spsq = ((n1-1)*s1sq+(n2-2)*s2sq)/(n1+n2-2)
kwantyl = qt(1-alpha/2, df)
L = xbar- ybar - kwantyl*sqrt(spsq*(1/n1+1/n2))
U = xbar- ybar + kwantyl*sqrt(spsq*(1/n1+1/n2))
#Z ufnością 99% przedział (69,78 ; 70,21) pokrywa nieznaną różnicę średnich czasów dwóch typów żarówek LED

#Sposób 2
rrr = t.test(typ1, typ2, var.equal = T, conf.level = 1-alpha)
rrr$conf.int
#Z ufnością 99% przedział (69,78 ; 70,21) pokrywa nieznaną różnicę średnich czasów dwóch typów żarówek LED

#Sposób 1
#H0: mu1 <= mu2 H1: mu1 > mu2
#H0: mu1 - mu2 <= 0 H1: mu1 -mu2 > 0
t = (xbar-ybar)/sqrt(spsq*(1/n1+1/n2)) #t = 4.14
Kwantyl = qt(1-alpha, df) #Kwantyl = -2.89
# R = (-2.89, inf)
# t należy do R -> odrzucamy H0
# na poziomie istotności 1% dane potwierdzają, że żywotność żarówek 1 typu jest dłuższa niż żarówek 2 typu

rrr = t.test(typ1, typ2, var.equal = T, mu = 0, alternative = "greater")
rrr$p.value
#alpha = 0.01 > 0.002 = pval -> odrzucamy H0
# na poziomie istotności 1% dane potwierdzają, że żywotność żarówek 1 typu jest dłuższa niż żarówek 2 typu

#JEDNORODNOŚĆ WARIANCJI
# H0: sigma1sq = sigma2sq H1: sigma1sq != sigma2sq
f = s1sq/s2sq 
alpha = 0.01
#1.205
kwantylL=qf(alpha/2, n1-1, n2-1)
#0.04
kawantylU=qf(1-alpha/2, n1-1, n2-1)
#23.15
#R należy do przedziału (0;0.04) (15.97;INF)
# f nie należy do R -> brak podstaw do odrzucenia H0
# na poziomie istotności 1% dane nie potwierdzają, że wariancje są różne

var.test(typ1, typ2)$p.value
#alpha = 0.01 < p_value = 0.86 - brak podstaw do odrzucenia H0

#Różnica proporcji populacyjnych:
#H0:    pt >= paw     H1:  pt < paw
#H0:    pt - paw >= 0    H1:  pt - paw < 0
tt = 63
nt = 100
taw= 107
naw= 150
prop.test(c(tt, taw), c(nt, naw), alternative = "less")

#alpha = 0.05  < 0.10 = pval - brak podstaw do odrzucenia H0
#na poziomie istotności 5% dane nie potwierdzają, że proporcja zdanych testów jest lepsza w nowym sposobie nauczania
