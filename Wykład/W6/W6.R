typ1  = c(2830, 2840, 2800, 2880,2820)
typ2 = c(2790, 2720, 2770, 2780, 2760)

alpha = 0.01

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

#Z ufnością 99% przedział pokrywa nieznaną różnicę średnich czasów dwóch typów żarówek LED

rrr = t.test(typ1, typ2, var.equal = T, conf.level = 1-alpha)
rrr$conf.int

#H0: mu1 <= mu2 H1: mu1 > mu2
t = (xbar-ybar)/sqrt(spsq*(1/n1+1/n2))
Kwantyl = qt(1-alpha, df)
# R = (2.89, inf)
# t należy do R -> odrzucamy H0
# na poziomie istotności 1% dane potwierdzają, że żywotność żarówek 1 typu jest dłuższa niż żarówek 2 typu

rrr = t.test(typ1, typ2, var.equal = T, mu = 0, alternative = "greater")
rrr$p.value
#alpha = 0.01 > 0.002=pval -> odrzucamy H0

# jdenorodność wariancji
# H0: sigma1sq = sigma2sq          H1: sigma1sq != sigma2sq
f = s1sq/s2sq #1.205
kwantylL=qf(alpha/2, n1-1, n2-1)
kawantylU=qf(1-alpha, n1-1, n2-1)
# f nie należy do R -> nie odrzucamy 
# na poziomie istotności 1% dane nie potwierdzają, że wariancje są różne

var.test(typ1, typ2)$p.value
#alpha = 0.01 < - nie odrzucamy H0

#H0:    pt >= pn       H1:  pt < pn

tt = 63
nt = 100

tn = 107
nn = 150

prop.test(c(tt, tn), c(nt, nn), alternative = "less")

#alpha = 0.05  < 0.10 = pval - nie odrzucamy H0











