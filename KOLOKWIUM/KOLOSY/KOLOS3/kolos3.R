#α > p-wartość: odrzucamy hipotezę H0 (przyjmujemy H1)
#α < p-wartość: brak podstaw do odrzucenia H0
#α>p-wartość: na poziomie istotności α dane potwierdzają hipotezę badacza (H1)
#α<p-wartość: na poziomie istotności α dane nie potwierdzają hipotezy badacza (H1)

#ZAD1:
# H0: p >= 2.3
# H1: mu < 2.3 (przeciętna długość łodygi słonecznika zwyczajnego jest mniejsza niż 2.3 cm)
dane0 = read.csv("C:\\Users\\kolec\\Desktop\\KOLOKWIUM\\KOLOSY\\KOLOS3\\dane_K2.csv", sep = ";", dec = ",")
slonecznik = dane0$slonecznik
alfa = 0.03
t.test(slonecznik, mu = 2.3, alternative="less")

# alfa = 0.03 < p-value = 0.8366 - brak podstaw do odrzucenia H0
# na poziomie istotności 0.03 dane nie potwierdzają hipotezy badacza, że przeciętna długość łodygi słonecznika zwyczajnego jest mniejsza niż 2.3 cm.

#ZAD2:
dane = read.csv("C:\\Users\\kolec\\Desktop\\KOLOKWIUM\\KOLOSY\\KOLOS3\\ANOVA_powietrze.csv", sep = ";", dec = ",")

stare_miasto = na.omit(dane$Stare_Miasto)
nowe_miasto = na.omit(dane$Nowe_Miasto)
wilda = na.omit(dane$Wilda)
jezyce = na.omit(dane$Jezyce)
grunwald = na.omit(dane$Grunwald)

obiekty = rep(names(dane), c(length(stare_miasto), length(nowe_miasto), length(wilda), length(jezyce), length(grunwald)))
wyniki = c(stare_miasto, nowe_miasto, wilda, jezyce, grunwald)
Test = data.frame(obiekty,wyniki) 

#a)
# H0: mu1 = mu2 = mu3 = mu4 (Brak istotnych różnic między rzeczywistymi średnimi zawartościami dwutlenku siarki w dzielnicach Poznania.)
# H1: ~H0 
anova(lm(wyniki~obiekty))

# alfa = 0.05 > p-value = 2.061e-08 - odrzucamy hipotezę H0 
# na poziomie istotności 0.05 odrzucamy hipotezę H0,
# stwierdzamy zatem, że sa istotne roznice miedzy rzeczywistymi srednimi
# zawartościami dwutlenku siarki w dzielnicach Poznania.

#b) Musi być w przedziale 0:
# Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
# Grupy ktore nie roznia sie miedzy soba istotnie:
# Grupy jednorodne: Stare_Miasto-Grunwald.

#ZAD3:
dane1 = read.csv("C:\\Users\\kolec\\Desktop\\KOLOKWIUM\\KOLOSY\\KOLOS3\\Reg_odpady.csv", sep = ";", dec = ",")
wytworzone_x = na.omit(dane1$wytworzone)
wykorzystane_y = na.omit(dane1$wykorzystane)

# a)
cor(wytworzone_x, wykorzystane_y) 
# współczynnik korelacji r = |0.9235867| > 0.8, zatem
# istnieje bardzo silna zależność liniowa między roczną wielkością wytworzonych odpadów, a ilością wykorzystanych wtórnie.

#b)
prosta = lm(wykorzystane_y ~ wytworzone_x) 
wspolczynniki = coef(prosta)
rownanie = paste("y =", round(wspolczynniki[1], 2), "+", round(wspolczynniki[2], 2), "*x")
rownanie
# y = -193.72 + 2.15*x
# równanie regresji liniowej między roczną wielkością wytworzonych odpadów, a ilością wykorzystanych wtórnie.

plot(wytworzone_x, wykorzystane_y, main = "Wykres regresji liniowej", xlab = "wielkością wytworzonych odpadów", ylab = "ilością wykorzystanych wtórnie")
abline(prosta)

#c)
predict(prosta, data.frame(wytworzone_x = 125))
# Jeżeli wielkość wytworzonych odpadów wyniesie 125 mln ton, to wykorzystanych wtórnie zostanie 74.45964 mln ton.

#ZAD4:
# H0: dane potwierdzają, że odsetek pasażerów, któy zgubił bagaż w trakcie lotu, zależy od linii lotniczych
# H1: ~H0
TAK = c(9, 7, 5)
NIE = c(91, 93, 95)
A = data.frame(TAK, NIE)
chisq.test(A)
# alpha = 0.03 < 0.5409 = p_val  ->  brak podsawt do odrzucenia H0
# Na poziomie istotności 0.03 dane nie potwierdzają hipotezy, że odsetek pasażerów, któy zgubił bagaż w trakcie lotu, zależy od linii lotniczych.





