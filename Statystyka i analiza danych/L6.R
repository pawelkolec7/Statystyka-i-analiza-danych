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
