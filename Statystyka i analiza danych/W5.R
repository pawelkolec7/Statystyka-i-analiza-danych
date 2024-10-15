#WYKŁAD5:
#H1 - hipoteza badacza
#H0 - hipoteza, którą odrzucamy, przeciwna do badacza
#p−value – zmienna losowa oznaczająca wartość „graniczną”, decydująca o nieodrzuceniu / odrzuceniu hipotezy H0
#α > p-wartość: odrzucamy hipotezę H0 (przyjmujemy H1)
#α < p-wartość: brak podstaw do odrzucenia H0
#α>p-wartość: na poziomie istotności α dane potwierdzają hipotezę badacza (H1)
#α<p-wartość: na poziomie istotności α dane nie potwierdzają hipotezy badacza (H1)


#P1:
#H0: mu <= 36, H1: mu > 36 - wydobycie większe od 36 - to co nas interesuje

#P2:
opony=c(53.417, 51, 48.583, 53, 49, 51, 52, 50)
n = length(opony)
xbar = mean(opony)
s = sd(opony)
alpha = 0.05
#H1: mu > 50 H0: mu <= 50

#Sposób 1 - jak t należy do obszaru krytycznego to odrzucamy H0.
mu0 = 50
t = (xbar - mu0)/(s/sqrt(n)) 
#t = 1.607

qt(1-alpha, n-1)
#R = (1,89, inf)
#T nie należy do R -> brak podstaw do odrzucenia H0
#Na poziomie istotności 5% dane nie potwierdzają, że srendnia żywotność przekracza 50000 mil

#Zmiana poziomu istotności
alpha = 0.1
qt(1-alpha, n-1)
#R = (1,41, inf)
#t = 1.607 należy do obszaru krytcznego -> odrzucamy H0
#na poziomie istotności 10% dane potwierdzają, że średnia żywotność opon przekracza 50000 mil

#Sposób 2:
#α > p-wartość: odrzucamy hipotezę H0 (przyjmujemy H1)
#α < p-wartość: brak podstaw do odrzucenia H0
#α>p-wartość: na poziomie istotności α dane potwierdzają hipotezę badacza (H1)
#α<p-wartość: na poziomie istotności α dane nie potwierdzają hipotezy badacza (H1)

t.test(opony, mu=mu0, alternative = "greater")
#alpha = 0.1 > 0.07-pval -> odrzucamy H0
#odrzucamy hipotezę H0 (przyjmujemy H1)

#P3
czas = c(10, 10, 15, 12, 9, 8, 4, 10, 3, 4, 6, 5, 7, 8, 13)
n = length(czas)
ssq = var(czas)
alpha = 0.05

#H0 sigmasq >= 25 H1: sigmasq < 25
sigmasq0 = 25
chisq = (n-1)*ssq / sigmasq0
qchisq(alpha, n-1)
#R = (0, 6.57)
# chsq nie należy do R -> brak podstaw do odrzucenia H0
# na poziomie instotności 5% dane nie potwierdzają przypsuszcenia, 
#że zrównicowanie czasu naprawy użądzeń jest któtsze niż 25

sigma.test(czas, sigmasq=sigmasq0, alternative = "less")
#alpha = 0.05 < 0.06 = pval -> brak podstaw do odrzucenia H0

#P4
n = 200
t = 111
phat = t/n
alpha = 0.05
p0 = 0.5

#H0: p = 0.5    H1: p1 != 0.5
z = (phat-p0)/sqrt(p0*(1-p0)/n)
qnorm(1-alpha/2)
# R = (-inf, -1.96) lub (1.96, inf)

# Z nie należy o R
# Brak podstaw do odrzucenia H0
# Na poziomie istotności 5% dane nie potwoerdzają, że po 10 
#latach proporcja inżynierów pozostających w swojej profesji jest różńa od 50%

binom.test(t, n,  p=p0, alternative = "two.sided")
# alpha = 0.05 < 0.13=pval -> brak podstaw do odrzucenia H0
prop.test(t, n, p=p0, alternative = "two.sided")
