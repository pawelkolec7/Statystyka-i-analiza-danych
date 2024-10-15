czas = c(8,12,26,10,23,21,16,22,18,17,36,9)
xbar = mean(czas)
xbar

#muhat = 19.17

#śrendi czas oczekiewnia przez wszytskich pacjentów został oszcowany na 18h i 10m

s = sd(czas)
n = length(czas)
alpha = 0.05

L = xbar-qt(1-alpha/2, n-1)*s/sqrt(n)
U = xbar+qt(1-alpha/2, n-1)*s/sqrt(n)

mean = t.test(czas,conf.level=0.95)
mean$conf.int

var(czas)
sd(czas)
n = 12
alpha = 0.05
alphapol = alpha/2

Lchi = qchisq(1-alphapol, n-1)
Pchi = qchisq(alphapol, n-1)

L = (n-1)*var/Lchi
P = (n-1)*var/Pchi

#Z ufnnością 0.95 przedział (33.014;189.653) pokrywa prawdziwą nieznaną wartość wariancji dla populacji sigma^2


Chi2 = sigma.test(czas, conf.level=0.95)
sqrt(Chi2$conf.int)

n = 150
t = 70
phat = t/n

alpha = 0.10
z=qnorm(1-alpha/2)

L=phat-z*(sqrt(phat*(1-phat)/n))
U=phat+z*(sqrt(phat*(1-phat)/n))

#Z ufnością 95% przedział od 38,6% do 54,7% pokrywa nieznaną prawdziwą proporcję WSZYTSTKICH
#uczniów pozytywnie nastawionych do nowego programu nauczania
#zaokrąglamy bezpiecznie, dolny w dół, górny w górę

binom.test(t,n)
ddd = binom.test(t,n)
ci=ddd$conf.int
U-L

ci[2] - ci[1]
  
phat = 0.46
ee = 0.03

alpha = 0.05
z = qnorm(1-alpha/2)

# z*sqrt(phat*(1-phat)/n) = 0.03

#1060 ludzi zbadano o prezydenta






