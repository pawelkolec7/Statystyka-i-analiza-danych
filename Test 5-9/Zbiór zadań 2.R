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

#LAB5
#ŚREDNIA
dane = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L5\\L\\dane_hip.csv", sep = ";", dec = ",")
wiatr = na.omit(dane$wiatr)
#ZAD1:
#Sposób 1:
#Procedura testowa
#1. 
#H0: mu <= 4 H1: mu > 4
#2.
x_bar = mean(wiatr)
n = length(wiatr)
sd = sd(wiatr)
t = x_bar
t = (x_bar - 4)/(sd/sqrt(n))
#t = 2.41
#3.
alfa = 0.05
qt(1-alfa, n-1)
#R(1.795885;INF)
#4. 
#t należy do R, w związku z czym odrzucamy H0
#5. 
#Na poziomie istotności alpha = 0.05 dane potwierdzają hipotezę, że srednia prędkość wiatru przekracza 4 m/s.
#Tak jest sens budować elektrownię wiatrową

#Sposób 2:
#Procedura testowa
#1. 
#H0: mu <= 4 H1: mu > 4
#2.
t.test(wiatr, mu = 4, alternative="greater")
#3.
#p_value = 0.01705
#4.
#alfa = 0.05 > p_value = 0.01705 - odrzucamy H0
#5. 
#Na poziomie istotności alpha = 0.05 dane potwierdzają hipotezę, że srednia prędkość wiatru przekracza 4 m/s.
#Tak jest sens budować elektrownię wiatrową

#ZAD2:
#Sposób 1:
pompa = na.omit(dane$pompa)
#Procedura testowa
#1. 
#H0: mu >= 3.5 H1: mu < 3.5
#2.
t.test(pompa, mu = 3.5, alternative="less")
#3.
#p_value = 0.1521
#4.
#alfa = 0.01 < p_value = 0.1521 - brak podstaw do odrzucenia H0
#Na poziomie istotności alpha = 0.01 dane nie potwierdzają hipotezy, że współczynnik efektywności pompy cieplnej w
#jego gospodarstwie domowym jest znacznie mniejszy niż 3,5.

#Sposób 2:
#Procedura testowa
#1. 
#H0: mu >= 3.5 H1: mu < 3.5
#2.
x_bar = mean(pompa)
n = length(pompa)
sd = sd(pompa)
t = x_bar
t = (x_bar - 3.5)/(sd/sqrt(n))
#t = -1.09
#3.
alfa = 0.01
qt(1-alfa, n-1)
#R(-INF; -2.821438)
#4. 
#t nie należy do R, w związku z czym brak podstaw do odrzucenia H0
#5. 
#Na poziomie istotności alpha = 0.01 dane nie potwierdzają hipotezy, że współczynnik efektywności pompy cieplnej w
#jego gospodarstwie domowym jest znacznie mniejszy niż 3,5.

#ZAD3:
#Procedura testowa
morze = na.omit(dane$morze)
#1. 
#H0: mu = 3.5 H1: mu != 870
#2.
z.test(morze, sigma.x = 5, mu = 870, alternative="two.sided")
#3.
#p_value = 0.6547
#4.
#alfa = 0,05 < p_value = 0.6547 - brak podstaw do odrzucenia H0
#5.
#Na poziomie istotności alpha = 0.05 dane potwierdzają hipotezę, że że średnia głębokość morza w tym rejonie jest różna od 870 m. 

#ZAD4:
#Procedura testowa
blaszki = na.omit(dane$blaszki)
#1. 
#H0: mu <= 0,04 H1: mu > 0,04
#2.
zsum.test(mean(blaszki), sd(blaszki), length(blaszki), mu = 0.04, alternative="greater")
#3.
#p_value = 0.6547
#4.
#alfa = 0,02 < p_value = 0.05041 - brak podstaw do odrzucenia H0
#5.
#Na poziomie istotności alpha = 0.05 dane potwierdzają hipotezę, że produkowane przez ten automat blaszki są grubsze niż nominalna grubość.

#WARIANCJA
#ZAD5:
#a)
#Procedura testowa
mleko = na.omit(dane$mleko)
#1. 
#H0: mu = 1.7 H1: mu =! 1.7
#2.
t.test(mleko, mu = 1.7, alternative="two.sided")
#3.
#p_value = 0.1114
#4.
#alfa = 0,05 < p_value = 0.1114 - brak podstaw do odrzucenia H0
#5.
#Na poziomie istotności alpha = 0.05 dane potwierdzają hipotezę, że średnia zawartość tłuszczu w mleku różni się od 1,7 %
#b)
#Sposób 1:
#Procedura testowa
mleko = na.omit(dane$mleko)
alfa = 0.05
#1. 
#H0: sig^2 >= 0.02 H1: sig^2 < 0.02
#2.
Chi2 = (n-1)*var(mleko)/0.02
#Chi2 = 5.2
#3.
qchisq(alfa, n-1)
#R(0;3.32)
#4. 
#Chi2 nie należy do obszaru testowrgo R - nie mamy ppodstw do odzrucenienia H0
#5. 
#Na poziomie istotności alpha = 0.05 dane nie potwierdzają hipotezę, że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0,02 (%)2

#Sposób 2:
#Procedura testowa
mleko = na.omit(dane$mleko)
#1. 
#H0: sig^2 >= 0.02 H1: sig^2 < 0.02
#2.
sigma.test(mleko, sigmasq = 0.02, alternative="less")
#alpha = 0.05 < p_value = 0.1835 ->  nie mamy podstw do odzrucenienia H0
#5. 
#Na poziomie istotności alpha = 0.05 dane nie potwierdzają hipotezy, że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0,02 (%)2

#ZAD6:
#a1)
#Procedura testowa
kukulki = na.omit(dane$kukulki)
#1. 
#H0: mu = 17 H1: mu =! 17
#2.
z.test(kukulki, sigma.x = 2.5, mu = 17, alternative="two.sided")
#3.
#p_value = 0.01258
#4.
#alfa = 0,05 > p_value = 0.01258 - odrzucamy hipotezę H0
#5.
#Na poziomie istotności alpha = 0.05 dane nie potwierdzają hipotezy, że średnia długość jaj strzyżyka wynosi 17 mm.

#a2)
#Procedura testowa
kukulki = na.omit(dane$kukulki)
sigmasq = 2.5^2
#1. 
#H0: sig^2 = 0.02 H1: sig^2 =! 0.02
#2.
sigma.test(kukulki, sigmasq = 2.5^2, alternative="two.sided")
#alpha = 0.05 < p_value = 0.4984 ->  nie mamy podstw do odzrucenienia H0
#5. 
#Na poziomie istotności alpha = 0.05 dane potwierdzają hipotezę, że wariancja długości podrzuconych jaj wynosi 2.5 mm.

#b) H0 : µ = µ0:
srednia = mean(kukulki)
liczebnosc = length(kukulki)
odchylenie = sd(kukulki)
alfa = 0.05
Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
#Przedział ufności to (17.36; 19.36)
#µ0 nie należy do przedziału ufności, w związku z czym odrzucamy H0, zgadza się

#ZAD7:
#Średnia
#1. 
#H0: mu <= 55 H1: mu > 55
#2.
zsum.test(60, 20, 100, mu = 55, alternative="greater")
#3.
#p_value = 0.006
#4.
#alfa = 0,01 > p_value = 0.006 - odrzucamy hipotezę H0
#5.
#Na poziomie istotności alpha = 0.01 dane potwierdzają hipotezy, że że fabryka działa niezgodnie z prawem.

#Wariancja
n = 100
od_jakie_badamy = 18
od_zmierzone = 20
#1. 
#H0: sig^2 = 18^2 H1: sig^2 =! 18^2
#2.
Chi2 = ((n-1)*od_zmierzone^2)/(od_jakie_badamy^2)
#Chi2 = 122.22
#3.
alfa = 0.01
qchisq(alfa / 2, n - 1)
qchisq(1 - alfa/2,n - 1)
#R = (0, 66.5) suma (139, inf)
#4.
#Chi2 nie należy do obszaru testowrgo R - nie mamy podstw do odzrucenienia H0
#5. 
#Na poziomie istotności alpha = 0.01 dane nie potwierdzają hipotezę, dane potwierdzają hipotezy, dane nie potwierdzają hipotezy,
#że wariancja pomiarów jest różna od 18.

#ZAD8:
n = 2500
T = 1600
p_hat = T/n
alfa = 0.05

#Sposób 1:
#1. H0: p=60% H1: p!=60%
#2.
Z = (p_hat - 0.6)/sqrt(0.6*(1-0.6)/n)
#Z = 4.0824
qnorm(1-alfa/2)
#R(-INF;1.959964) suma (1.959964;INF)
#Z należy do R -> odrzucamy H0
#Na poziomie istotności alfa=0.05 dane potwierdzają hipotezę, że dokładnie 60% osób nie chce pójść na wybory.

#1. H0: p=60% H1: p!=60%
#2.
binom.test(T, n, p = 0.6, alternative="two.sided")
#3. p-value = 4.413e-05 < alfa = 0.05 -> odrzucamy H0
#Na poziomie istotności alfa=0.05 dane potwierdzają hipotezę, że dokładnie 60% osób nie chce pójść na wybory.

#ZAD9:
n = 1200
T = 16
#1. H0: p <= 2% H1: p < 2%
#2.
binom.test(T, n, p = 0.02, alternative="less")
#3. p-value = 0.05451 > alfa = 0.05 -> brak podstaw do odrzucenia H0
#Na poziomie istotności alfa=0.05 dane nie potwierdzają hipotezy, że frakcja ta w badanej fermie jest mniejsza niż 2%.

#ZAD10:
n = 1100
T = 1000
p = 0.90
#1. H0: p <= 90% H1: p > 90%
#2.
binom.test(T, n, p, alternative="greater")
#3. p-value = 0.1701 > alfa = 0.05 -> brak podstaw do odrzucenia H0
#Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy, że procent Polaków, którzy nie przeczytali żadnej książki jest większy niż 90. 

###################################################################################################################################################
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

#LAB6
dane = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L6\\L\\DwiePopulacje.csv", sep =';')
#Zad.1 
reg1 = na.omit(dane$cel1)
reg2 = na.omit(dane$cel2)
n1 = length(reg1)
n2 = length(reg2)
x_bar1 = mean(reg1)
x_bar2 = mean(reg2)
var1 = var(reg1)
var2 = var(reg2)
sp2 = ((n1-1)*var1+(n2-1)*var2) / (n1+n2-2)

#a.
# [1] H0: mu1-mu2 = 0   H1: mu1-mu2 != 0
t = (x_bar1 - x_bar2 - 0) / sqrt(sp2*(1/n1 + 1/n2)) # [2] t = -1.539823
alpha = 0.02
qt(1-alpha/2, n1+n2-2) # [3] R = (-INF, -2.47266) u (2.47266, +INF)
# [4] t nie należy do R   ->  brak podstaw do odrzuceni H0
# [5] na poziomie istoności 0.02 dane nie potwierdzają hipotezy, że przeciętna zawartość celulozy dla regionu I różni się istotnie od przeciętnej
# zawartości celulozy dla regionu II.

t.test(reg1, reg2, var.equal = TRUE, mu = 0, alternative = "two.sided") #p_value = 0.1352
# alpha = 0.02 < p_value = 0.1352  ->  brak podstaw do odrzucenia H0
# na poziomie istoności 0.02 dane nie potwierdzają hipotezy, że przeciętna zawartość celulozy dla regionu I różni się istotnie od przeciętnej
# zawartości celulozy dla regionu II.

#b. 
# [1] H0: mu1 - mu2 = 0   H1: mu1 - mu2 != 0
F = var1/var2 # [2] F = 0.4786012
qf(alpha/2, n1-1, n2-2)
qf(1-alpha/2, n1-1, n2-2)
# [3] R = (_INF, 0.1617909) u (3.765269, +INF)
# [4] F nie należy do R  ->  brak podstaw do odrzucenia H0
# [5] na poziomie istotności 0.02 dane nie potwierdzają hipotezy o różnożności wariancji. Zatem możemy założyć, że wariancje są jednorodne.

var.test(reg1, reg2, alternative = "two.sided") # p_value = 0.3225
# alpha = 0.02 < p_value = 0.3225  ->  brak podstaw do odrzucenia H0

#c.
t.test(reg1, reg2, var.equal = TRUE, conf.level = 0.98)$conf.int
# na poziomie ufności 0.98 przedział (-13.51; 3.15) pokrywa nieznaną prawdziwą różnicę średnich zawartości celulozy w drewnie w dwóch regionach.
# ponieważ przedział ufności pokrywa wartość 0 zatem nie mamy podstaw do odrzucenia H0.

#Zad2.
tradycyjna = na.omit(dane$tradycyjna)
nowa = na.omit(dane$nowa)
#H0: var1 - var2 = 0   H1: var1 - var2 != 0
var.test(tradycyjna, nowa, alternative = "two.sided")
# alpha = 0.1 < p_value = 0.36  ->  brak podstaw do odrzucenia H0
# na poziomie istotności 0.1 dane nie potwierdzają hipotezy o różnożności wariancji. Zatem możemy założyć, że wariancje są jednorodne.

#Założenie: Rozkład normalny, wariancje są jednorodne

#H0:  mu_n - mu_t >= 0   H1:  mu_n - mu_t < 0
t.test(nowa, tradycyjna, var.equal = TRUE, alternative = "less") 
#p_value = 0.61
# alpha = 0.1 < p_value = 0.61  ->  brak podstaw do odrzucenia H0
# na poziomie istoności 0.1 dane nie potwierdzają hipotezy, że średni czas budowy metodą tradycyjną jest dłuższy od średniego czasu budowy nową metodą

#Zad3.
publiczny = na.omit(dane$publiczny)
prywatny = na.omit(dane$prywatny)
#H0: var1 - var2 = 0   H1: var1 - var2 != 0
var.test(publiczny, prywatny, alternative = "two.sided")
# alpha = 0.1 > p_value = 0.09  ->  odrzucamy hipotezę H0
# na poziomie istotności 0.1 dane potwierdzają hipotezę o różnożności wariancji. Zatem możemy założyć, że wariancje są różne.

#Założenie: Rozkład normalny, wariancje nie są jednorodne

#H0:  mu_pub - mu_pryw >= 0   H1:  mu_pub - mu_pryw < 0
t.test(publiczny, prywatny, var.equal = FALSE, alternative = "less") 
# alpha = 0.1 > p_value = 0.02  ->  odrzucamy hipotezę H0
# na poziomie istoności 0.1 dane potwierdzają hipotezę, że publiczne źródła finansowania udzielają, przeciętnie rzecz biorąc, mniejszych kredytów

#Zad4.
zawodnik1 = na.omit(dane$zawodnik1)
zawodnik2 = na.omit(dane$zawodnik2)

#H0: var1 >= var2   H1: var1 < var2 
#H0: var1 - var2 >= 0   H1: var1 - var2 < 0
var.test(zawodnik1, zawodnik2, alternative = "less")
# alpha = 0.05 < p_value = 0.21  ->  brak podstaw do odrzucenia H0
# na poziomie istotności 0.05 dane nie potwierdzają hipotezy o większej regularności wyników pierwszego zawodnika.

#Zad5.
L1 = na.omit(dane$L1)
L2 = na.omit(dane$L2)
#H0: var1 - var2 = 0   H1: var1 - var2 != 0
var.test(L1, L2, alternative = "two.sided")
# alpha = 0.1 < p_value = 0.64  ->  brak podstaw do odrzucenia hipotezy H0
# na poziomie istotności 0.1 dane nie potwierdzają hipotezę o różnożności wariancji. Zatem możemy założyć, że wariancje są jednakowe.

#Założenie: Rozkład normalny, wariancje są jednorodne

#H0: L1 <= L2   H1: L1 > L2 
#H0:  L1 - L2 <= 0   H1:  L1 - L2 > 0
t.test(L1, L2, var.equal = TRUE, alternative = "greater") 
# alpha = 0.1 > p_value = 0.08  ->  odrzucamy hipotezę H0
# na poziomie istoności 0.1 dane potwierdzają hipotezę, że średni czas działania leku L1 jest istotnie dłuższy niż dla leku L2

#Zad6.
#a.
T1 = 0.78 * 1200
T2 = 0.8 * 2000
n1 = 1200
n2 = 2000
prop.test(c(T1, T2), c(n1, n2), conf.level = 0.9)$conf.int
# Z ufnością 0.9 przedział (-0.0452; 0.0053) pokrywa nieznaną prawdziwą różnicę porporcji osób zadowolonych z pracy w Polsce i USA.

#b.
# H0: p1-p2 >= 0    H1: p1-p2 < 0
prop.test(c(T1, T2), c(n1, n2), alternative = "less")
# p_value = 0.09583
# alpha = 0.1 > p_value = 0.096 ->  odrzucamy H0
# na poziomie istotności 0.1 dane potwierdzają hipotezę, że proporcja zadowolonych Polaków jest mniejsza niż zadowolonych Amerykanów.

#c.
# H0: p - 0.75 <= 0    H1: p - 0.75 > 0
prop.test(x = 0.78 * 1200, n = 1200, p = 0.75, alternative = "greater", conf.level = 0.9)
# p_value = 0.009
# alpha = 0.1 > p_value = 0.009 -> odrzucamy H0
# na poziomie istotności 0.1 dane potwierdzają hipotezę, że procent Polaków zadowolonych z pracy jest większy niż 75.

#Zad7.
AAz = 313
BAz = 28
n1 = AAz + BAz
AAf = 145
BAf = 56
n2 = AAf + BAf
T1 = AAz/n1
T2 = AAf/n2

prop.test(c(AAz, AAf), c(n1, n2), conf.level = 1 - 0.05) #p_value = 0
# alpha > p_val  ->  0drzucamy H0
# na poziomie istotności 0.05 nie dane potwierdzają hipotezy, że częstość występowania malarii typu A zależy od regionu.

prop.test(c(AAz, AAf), c(n1, n2), alternative = "two.sided", conf.level = 0.95)
# Z ufnością 0.95 przedział (0.124; 0.269) pokrywa nieznaną prawdziwą różnicę badanych częstości występowania malarii typu A.

#Zad8.
T11 = 73
n11 = 105
T30 = 102
n30 = 110
prop.test(c(T11,T30), c(n11, n30), conf.level = 1 - 0.05) #p_value = 0

#H0:p1 - p2 = 0  H1:p1 - p2 != 0
# aplha = 0.05 > p_val = 0.05 ->  odrzucamy H0
# na poziomie istotności 0.05 dane potwierdzają hipotezę, że proporcja przeżywalności zależy od temperatury.

#b)
prop.test(c(T11, T30), c(n11, n30), alternative = "less", conf.level = 0.95)$conf.int
# Z ufnością 0.95 przedział (-0.3257; -0.1383) pokrywa nieznaną prawdziwą różnicę badanych częstości występowania malarii typu A.


# Zadanie 9
przed = c(15, 4, 9, 9, 10, 10, 12, 17, 14)
po = c(14, 4, 10, 8, 10, 9, 10, 15, 14)
roznica = przed - po
# H0: mu = 0    H1: mu != 0
t.test(roznica, conf.level = 1 - 0.05)
# [3] p_value = 0.08052
# [4] 0.05 = alpha < p_value = 0.08  ->  brak podstaw do odrzucenia H0
# [5] na poziomie istotności 0.05 dne nie potwierdzają hipotezy, że dany rodzaj leku zmienia wartości określonego parametru biochemicznego.


# Zadanie 10
przed = c(6.55, 5.98, 5.59, 6.17, 5.92, 6.18, 6.43, 5.68)
po = c(6.78, 6.14, 5.80, 5.91, 6.10, 6.01, 8.18, 5.88)
roznica = przed - po
# H0: mu = 0    H1: mu != 0
t.test(roznica, conf.level = 1 - 0.1)
# 0.10 = alpha > p_value = 0.23  ->  brak podstaw do odrzucenia H0
# na poziomie istotności 0.10 dne nie potwierdzają hipotezy, że pH wody zależy od głębokości.

t.test(roznica, conf.level = 1 - 0.9)
# 0.10 = alpha < p_value = 0.23  ->  brak podstaw do odrzucenia H0
# na poziomie istotności 0.90 dne nie potwierdzają hipotezy, że pH wody zależy od głębokości.

#######################################################################################################################################################
chomiki = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_chomiki.csv", sep = ";", dec = ",")
cisnienie = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_cisnienie.csv", sep = ";", dec = ",")
kopalnie = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_kopalnie.csv", sep = ";", dec = ",")
mikrometr = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_mikrometr.csv", sep = ";", dec = ",")
pulapki = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_pulapki.csv", sep = ";", dec = ",")
sportowcy = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_sportowcy.csv", sep = ";", dec = ",")

#Zad1.
obiekty = rep(names(cisnienie), each = length(cisnienie$Niskie))
wyniki = c(cisnienie$Niskie, cisnienie$Srednie, cisnienie$Silne, cisnienie$BardzoSilne)
cisnienieTest = data.frame(obiekty,wyniki) 

# Średnie próbkowe.
srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$obiekty), mean)

#H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0.
bartlett.test(wyniki~obiekty, cisnienieTest) 
#p_value = 0.5009

# 0.05 = alpha < p_val = 0.5  ->  brak podstaw od odrzucenia H0.
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji.
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.

# H0: mu1 = mu2 = mu3 = mu4   H1: ~H0 
anova(lm(wyniki~obiekty)) 
# p_value = 0.09735
# 0.05 = alpha < p_val = 0.09735   -> brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia hipotezy H0, zatem ciśnienie nie ma wpływ na wielkość produkcji.

#Zad2.

obiekty = rep(names(kopalnie), each = length(kopalnie$K1))
wyniki = c(kopalnie$K1, kopalnie$K2, kopalnie$K3, kopalnie$K4, kopalnie$K5)
kopalnieTest = data.frame(obiekty,wyniki) 

# Średnie próbkowe.
srednie = sapply(split(kopalnieTest$wyniki, kopalnieTest$obiekty), mean)

#H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0.
bartlett.test(wyniki~obiekty, kopalnieTest) 
#p_value = 0.03

# 0.01 = alpha < p_val = 0.03  ->  brak podstaw od odrzucenia H0.
# Na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji.
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.

# H0: mu1 = mu2 = mu3 = mu4   H1: ~H0 
anova(lm(wyniki~obiekty)) 
# p_value = 0.46
# 0.05 = alpha < p_val = 0.46  -> brak podstaw do odrzucenia H0
# Na poziomie istotności 0.01 nie mamy podstaw do odrzucenia hipotezy H0, zatem zawartości popiołu dla ekogroszku produkowanego w pięciu kopalniach 
# można uznać za jednakowe.

#Zad3.
mikrometr1 = na.omit(mikrometr$mikrometrI)
mikrometr2 = na.omit(mikrometr$mikrometrII)
mikrometr3 = na.omit(mikrometr$mikrometrIII)
# H0: mu1 = mu2 = mu3  H1: ~H0
obiekty = rep(names(mikrometr), c(length(mikrometr1), length(mikrometr2), length(mikrometr3)))
wyniki = c(mikrometr1, mikrometr2, mikrometr3)

anova(lm(wyniki~obiekty)) 
#p_value = 0.069
# alpha = 0.05 < p_value = 0.069 ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia hipotezy H0, zatem wybór mikrometru nie ma wpływu na uzyskane wyniki.

#Zad4.
obiekty = rep(names(sportowcy), each = length(sportowcy$Niepalacy))
wyniki = c(sportowcy$Niepalacy, sportowcy$Lekkopalacy, sportowcy$Sredniopalacy, sportowcy$Duzopalacy)

# a)  H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0
bartlett.test(wyniki~obiekty) 
#p_value = 0.8517
# alpha = 0.01 < p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji, 
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE

# H0: mu1 = mu2 = mu3 = mu4  H1: ~H0
anova(lm(wyniki~obiekty)) 
#p_value = 0.003979
# alpha = 0.01 > p_val  ->  odrzucamy H0
# Na poziomie istotności 0.05 odrzucamy hipotezy H0, zatem palenie papierosów może wpływać na rytm zatokowy serca.

# b) Chcemy sprawdzić, któe grupy są do siebie podobne (nie różnią się istotnie)
TukeyHSD(aov(wyniki~obiekty), ordered = T)
# Grupy jednorodne:(ujemne lwr)
# Sredniopalacy-Niepalacy, Duzopalacy-Niepalacy, Duzopalacy-Sredniopalacy, Lekkopalacy-Duzopalacy
# => Sredniopalacy-Niepalacy-Duzopalacy, Lekkopalacy-Duzopalacy
plot(TukeyHSD(aov(wyniki~obiekty), ordered = T))

#Zad5.
chomiki1 = na.omit(chomiki$I)
chomiki2 = na.omit(chomiki$II)
chomiki3 = na.omit(chomiki$III)
chomiki4 = na.omit(chomiki$IV)

obiekty = rep(names(chomiki), c(length(chomiki1), length(chomiki2), length(chomiki3), length(chomiki4)))
wyniki = c(chomiki1, chomiki2, chomiki3, chomiki4)

# a)  H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0
bartlett.test(wyniki~obiekty) 
#p_value = 0.213
# alpha = 0.05 < p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji, 
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE

# H0: mu1 = mu2 = mu3 = mu4 H1: ~H0
anova(lm(wyniki~obiekty)) 
#p_value = 0.024
# alpha = 0.05 > p_value = 0.024 -> odrzucamy H0
# Na poziomie istotności 0.05 odrzucamy hipotezy H0, masa gruczołu tarczycowego zależy od poziomu inbredu.

# b) Chcemy sprawdzić, któe grupy są do siebie podobne (nie różnią się istotnie)
TukeyHSD(aov(wyniki~obiekty), ordered = T)
# Grupy jednorodne:(ujemne lwr)
# II-I, III-I, III-II, IV-II, IV-III
# => I-II-III, II-III-IV
plot(TukeyHSD(aov(wyniki~obiekty), ordered = T))

#Zad6.
obiekty = rep(names(pulapki), each = length(pulapki$rozsiany))
wyniki = c(pulapki$rozsiany, pulapki$skoncentrowany, pulapki$roslina.zywicielka, pulapki$powietrzny, pulapki$gruntowy)
Test = data.frame(obiekty,wyniki) 

# Średnie próbkowe.
srednie = sapply(split(Test$wyniki, Test$obiekty), mean)

#H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0.
bartlett.test(wyniki~obiekty, Test) 
#p_value = 0.07
# alpha = 0.05 < p_val = 0.07  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji, 
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE

# H0: mu1 = mu2 = mu3 = mu4 H1: ~H0
anova(lm(wyniki~obiekty)) 
#p_value = 0
# alpha = 0.05 > p_value = 0 -> odrzucamy H0
# Na poziomie istotności 0.05 odrzucamy hipotezy H0, strategia lokalizacji może mieć wpływ na liczbę uwięzionych ciem cygańskich.

# b) Chcemy sprawdzić, któe grupy są do siebie podobne (nie różnią się istotnie)
TukeyHSD(aov(wyniki~obiekty), ordered = T)
# Grupy jednorodne:(ujemne lwr)
# rozsiany-gruntowy, skoncentrowany-roslina.zywicielka, powietrzny-roslina.zywicielka, powietrzny-skoncentrowany   
# => powietrzny-roslina.zywicielka-skoncentrowany, rozsiany-gruntowy
plot(TukeyHSD(aov(wyniki~obiekty), ordered = T))

#######################################################################################################################################################
data = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L8\\W\\MatProd.csv", sep = ";", dec = ",")
#niezerowa kowariancja - istneje zależność miedzy zmiennymi
#dodatnia - wraz ze wzrostem x, rośnie y
#ujemna - wraz ze wzrostem x, maleje y
plot(data$materialy, data$produkcja)

cov(data$materialy, data$produkcja)
#kowariancja rónza od zera, zatem istenej związek między ilościa zużytuch materiałow, a wartościa produkcji
#kowariancja jest dodatnia, a więc związek jest rosnący, co oznacza, że im więcje materiałów zużyjemy, tym większa będzie wielkość produkcji

cor(data$materialy, data$produkcja)
#isntnjeje bardzo silna zależność linowa, między ilością materiałów, a wartoscią produkcji

#jeśli x wzrośnie o 1, to y wzrośnie o b1
b1hat = cov(data$materialy, data$produkcja)/var(data$materialy)
b0hat = mean(data$produkcja) - b1hat*mean(data$materialy)
#yhat = 2.36 + 0.22x
curve(b0hat+b1hat*x, 0, 30, add=T)

prosta = lm(data$produkcja~data$materialy)
prosta
abline(prosta)

(b0hat + b1hat*23)*1000

predict(data$produkcja, data.frame(data$materialy=23))
mat = data$materialy
prod = data$produkcja
prosta = lm(prod~mat)
predict(prosta, data.frame(mat=23))

predict(prosta, data.frame(mat=c(23, 7, 12)))

#Funkcja regresji jest dobrze dopasowana, jak ma około 80%
#zmienność y wyjaśniona jest o 80% przez zmienność X

(cor(mat, prod))^2
#Zmienność wartości produkcji prawie w 94% wyjaśniona jest przez zmienność ilości zużytegii materiału

#H0: b1 = 0, H1: b1 != 0
anova(prosta)
#alpha = 0.01 > pval = 0 -> odrzucamy H0
#Na poziomie istotności 0.01 dane potwierdzają, że regresja jest istotna.

#Zad1.
chemikalia = data = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L8\\L\\Reg_chemikalia.csv", sep = ";", dec = ",")
chemikalia_x = chemikalia$surowiec
chemikalia_y = chemikalia$produkt

# a)
plot(chemikalia_x, chemikalia_y) 

#b)
cov(chemikalia_x, chemikalia_y) 
# Kowariancja jest różna od 0, zatem istnieje liniowa zależność między ilością zużytych surowca, a końcową wielkością produkcji środków chemicznych.
# Ponieważ kowariancja jest dodatnia, zatem wraz ze wzrostem zużytego surowca wzrasta końcowa wielkość produkcji środków chemicznych.

#c)
cor(chemikalia_x, chemikalia_y) 
# współczynnik korelacji r = |0.8953468| > 0.8, zatem
# istnieje bardzo silna zależność liniowy między ilością zużytych surowca, a końcową wielkością produkcji środków chemicznych.

#d)
prosta = lm(chemikalia_y ~ chemikalia_x) 
wspolczynniki <- coef(prosta)
rownanie = paste("y =", round(wspolczynniki[1], 2), "+", round(wspolczynniki[2], 2), "*x")
rownanie
# y = 3.62x + 22.41 
# równanie regresji liniowej między wielkością produkcji, a ilością zużytego surowca.

# e)
abline(prosta) 

# f) (interpretacja wsoółczynnika regresji liniowej)
# jeśli ilość surowca wzrośnie o 1 litr, to wielkość produkcji wzrośnie o 3.62kg.

# g) oraz h)

predict(prosta, data.frame(chemikalia_x=20))
predict(prosta, data.frame(chemikalia_x=15))

predict(prosta, data.frame(chemikalia_x=c(20,15))) 
# g) 94.8kg h) 76.7kg
# Jeżeli zużytej do produkcji 20 litrów surowca, to wielkość produkcji wynisie 94.8kg
# Jeżeli zużytej do produkcji 15 litrów surowca, to wielkość produkcji wynisie 76.7kg

summary(prosta) 
# i) współczynnik determinacji (Multiple R-squared) = 0.8016 * 100% = 80.16%
cor(chemikalia_x, chemikalia_y)^2 # <-- albo to to też i) to samo 80.16%
# Prosta regersji liniowej jest dobrze dopasowana do danych.
# Wartość końcowej produkcji jest wyjaśniana w ok 80% przez ilość zużytego surowca. 

# j) H0: b1 = 0 (regresja liniowa jest nieistotna)    H1: b1 != 0 (regresja liniowa jest istotna)
anova(prosta)
# alpha = 0.05 > 0.0004617 = p_val  ->  odrzucamy H0
# Na poziomie istotności 0.05 dane potwierdzają hipotezę, że liniowa regresja jest istotna.

#Zad2.
urzadzenie = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L8\\L\\Reg_urzadzenie.csv", sep = ";", dec = ",")
urzadzenie_x = urzadzenie$efektywnosc
urzadzenie_y = urzadzenie$zywotnosc

# a)
plot(urzadzenie_x , urzadzenie_y) 

#b)
cov(urzadzenie_x , urzadzenie_y)  
# Kowariancja jest różna od 0, zatem istnieje liniowa zależność między ilością zużytych surowca, a końcową wielkością produkcji środków chemicznych.
# Ponieważ kowariancja jest ujemna, zatem wraz ze wzrostem efektywnosci maleje zywotnosc urzadzenia.

#c)
cor(urzadzenie_x , urzadzenie_y)
# współczynnik korelacji r = |-0.9094164| > 0.8, zatem
# istnieje bardzo silna zależność liniowy między efektywnoscia a zywotnosc urzadzenia.

#d)
prosta = lm(urzadzenie_y ~ urzadzenie_x) 
wspolczynniki <- coef(prosta)
rownanie = paste("y =", round(wspolczynniki[1], 2), "+", round(wspolczynniki[2], 2), "*x")
rownanie
# y = -0.86x + 18.88
# równanie regresji liniowej między efektywnoscia a zywotnosc urzadzenia.
abline(prosta) 

#e) (interpretacja wsoółczynnika regresji liniowej)
# jeśli efektywność wzrośnie o 1 element, to żywotność urządzenia zmaleje o 0.86 miesiąca

#f oraz g)
predict(prosta, data.frame(urzadzenie_x=11))
predict(prosta, data.frame(urzadzenie_x=19))

predict(prosta, data.frame(urzadzenie_x=c(11,19))) 
# g) 9.39 miesiąca h) 2.49 miesiąca
# Jeżeli urządzenia przy efektywności produkcji 11 elementów, to żywotność urządzenia wyniesie 9.4 miesiąca.
# Jeżeli urządzenia przy efektywności produckji 19 elementów, to żywotność urządzenia wyniesie  2.5 miesiąca.

summary(prosta) 
# i) współczynnik determinacji (Multiple R-squared) = 0.827 * 100% = 82.7%
cor(urzadzenie_x , urzadzenie_y)^2 # <-- albo to to też i) to samo 82.7%
# Prosta regersji liniowej jest dobrze dopasowana do danych.
# Wartość końcowej produkcji jest wyjaśniana w ok 82.7% przez liczbe wyprodukowanych przez to urządzenie elementów.

# j) H0: b1 = 0 (regresja liniowa jest nieistotna)    H1: b1 != 0 (regresja liniowa jest istotna)
anova(prosta)
# alpha = 0.01 > 0.0006735 = p_val ->  odrzucamy H0
# Na poziomie istotności 0.01 dane potwierdzają hipotezę, że liniowa regresja jest istotna.

#Zad3.
ph = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L8\\L\\Reg_arszenik.csv", sep = ";", dec = ",")
ph_x = ph$pH
ph_y = ph$arszenik

# a)
plot(ph_x , ph_y) 

#b)
cov(ph_x , ph_y) 
# Kowariancja jest != 0, zatem istnieje liniowa zależność między zakwaszeniem gleby a ilością usuniętego arszeniku.
# Ponieważ kowariancja jest ujemna, zatem wraz ze wzrostem zakwaszeniem gleby maleje ilość usuniętego arszeniku.


#c)
cor(ph_x , ph_y) 
# współczynnik korelacji r = |--0.9504953| > 0.8, zatem
# istnieje bardzo silna zależność liniowa między zakwaszeniem gleby a ilością usuniętego arszeniku.

#d)
prosta = lm(ph_y ~ ph_x) 
wspolczynniki <- coef(prosta)
rownanie = paste("y =", round(wspolczynniki[1], 2), "+", round(wspolczynniki[2], 2), "*x")
rownanie
# y = 190.27 + -18.03*x
# równanie regresji liniowej między zakwaszeniem gleby a ilością usuniętego arszeniku.
abline(prosta) 

#e) (interpretacja wsoółczynnika regresji liniowej)
# jeśli pH gleby wzrośnie o 1, to ilość usuniętego przez proces arszeniku zmaleje o 18.03%

#f oraz g)
predict(prosta, data.frame(ph_x=7.5))
predict(prosta, data.frame(ph_x=9))

predict(prosta, data.frame(ph_x=c(7.5,9))) 
# g) 55.01 procenta h) 27.09 procenta
# Jeśli pH gleby wyniesie 7.5, to ilość arszeniku, który zostanie usunięty to 55.01%
# Jeśli pH gleby wyniesie 9, to ilość arszeniku, który zostanie usunięty to 27.96%

summary(prosta) 
# i) współczynnik determinacji (Multiple R-squared) = 0.9034 * 100% = 90.3%
cor(ph_x , ph_y)^2 # <-- albo to to też i) to samo 90.3%
# Prosta regersji liniowej jest dobrze dopasowana do danych.
# Wartość pH gleby jest wyjaśniana w ok 90.34% przez ilość arszeniku w %.

# j) H0: b1 = 0 (regresja liniowa jest nieistotna)    H1: b1 != 0 (regresja liniowa jest istotna)
anova(prosta)
# alpha = 0.01 > 0.0 = p_val ->  odrzucamy H0
# Na poziomie istotności 0.01 dane potwierdzają hipotezę, że liniowa regresja jest istotna.

#######################################################################################################################################################
# Zad1.
# H0: rozkład częstotliwości emerytów, którzy wróciili do pracy w hrabstwie Allegheny, 
# odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reyolda.
# H1: ~H0

# Emeryci: ZIO, S, FK, W
observedf = c(122, 85, 76, 17) #zaobserwowana częstość
expectedp = c(0.38, 0.32, 0.23, 0.07) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.1 < 0.35 = p_value  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że rozkład częstotliwości emerytów, którzy wróciili do pracy w hrabstwie Allegheny, 
# NIE odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reyolda.
# Zatem rozkład częstotliwości emerytów, którzy wróciili do pracy w hrabstwie Allegheny, 
# odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reyolda.

#Zad2.
#H0: rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w wieku od 1 do 18 lat rozkłada się jak opisano w artykule
#H1: ~H0
#Możliwości: W, Z, S
observedf = c(68, 27, 5) #zaobserwowana częstość
expectedp = c(0.74, 0.16, 0.10) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.1 > 0.005 = p_value  ->  odrzucamy H0
# Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w 
# wieku od 1 do 18 lat NIE rozkłada się jak opisano w artykule

#Zad3.
# H0: rozkład częstotliwości mieszanki smakowej wynosi po 20% dla każdego smaku cukierków producenta Skittels.
# H1: ~H0
observedf = c(7+20+4+12, 20+5+16+9, 10+5+13+16, 7+13+21+3, 14+17+4+17) #zaobserwowana częstość
expectedp = c(0.2, 0.2, 0.2, 0.2, 0.2) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.05 < 0.8369 = p_value  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że mieszanka smakowa wynosi 20% dla każdego smaku.

#Zad4.
dane = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L9\\L\\normalnosc_ozon.csv", sep = ";", dec = ",")

# H0: stężenie ozonu MA rozkład normalny 
# H1: stężenie ozonu NIE MA rozkładu normalnego

pearson.test(dane$ozon, adjust = FALSE)
# alpha = 0.05 < 0.269 = p_val  ->  brak podstaw do odrzucenia H0
pearson.test(dane$ozon, adjust = TRUE)
# alpha = 0.05 < 0.146 = p_val  ->  brak podstaw do odrzucenia H0
lillie.test(dane$ozon)
# alpha = 0.05 < 0.2774 = p_val  ->  brak podstaw do odrzucenia H0
shapiro.test(dane$ozon) # <-- to najlepsze (jak są dane auu)
# alpha = 0.05 < 0.1098 = p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 dane nie potwierdzają hipotezy, że stężenie ozonu NIE MA rozkładu normalnego.
# Zatem rozkład stężenia ozonu jest normalny.

#Zad6.
dane = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L9\\L\\normalnosc_punkty.csv", sep = ";", dec = ",")

# H0: stężenie ozonu MA rozkład normalny 
# H1: stężenie ozonu NIE MA rozkładu normalnego

shapiro.test(dane$punkty)
# alpha = 0.01 > 0.0006= p_val  ->  odrzucamy H0
# Na poziomie istotności 0.01 dane potwierdzają hipotezę, że liczba punków NIE MA rozkładu normalnego.

#Zad7.
# H0: miejsce zamieszkania danej osoby nie zależy od liczby lat studiów.
# H1: miejsce zamieszkania danej osoby zależy od liczby lat studiów
miej = c(15, 12, 8)
podmiej = c(8, 15, 9)
wiej = c(6, 8, 7)
MPW = data.frame(miej, podmiej, wiej)
chisq.test(MPW)
# alpha = 0.05 < 0.557 = p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 dane nie potwierdzają hipotezy, że miejsce zamieszkania danej osoby zależy od liczby lat studiów.

#Zad8.
# H0: odsetek pasażerów, którzy zagubili bagaż w trakcie lotu, nie zależy od linii lotniczej
# H1: odsetek pasażerów, którzy zagubili bagaż w trakcie lotu, zależy od linii lotniczej
l1 = c(10, 90)
l2 = c(7, 93)
l3 = c(4, 96)
LLL = data.frame(l1, l2, l3)
chisq.test(LLL)
# alpha = 0.05< 0.251 = p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 dane nie potwierdzają hipotezy, że odsetek pasażerów, którzy zagubili bagaż w trakcie lotu, zależy od linii lotniczej

#Zad9.
# H0: odczucia danej osoby w związku z ograniczeniami dotyczącymi anten satelitarnych nie są powiązane z jej wiekiem
# H1: odczucia danej osoby w związku z ograniczeniami dotyczącymi anten satelitarnych są powiązane z jej wiekiem
g1 = c(96, 201, 3)
g2 = c(96, 189, 15)
g3 = c(90, 195, 15)
g4 = c(36, 234, 30)
A = data.frame(g1, g2, g3, g4)
chisq.test(A)
# alpha = 0.05 > 0 = p_val  ->  odrzucamy H0
# Na poziomie istotności 0.05 dane potwierdzają hipotezy, że odczucia danej osoby w związku z ograniczeniami 
# dotyczącymi anten satelitarnych są powiązane z jej wiekiem