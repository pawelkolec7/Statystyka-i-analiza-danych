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

