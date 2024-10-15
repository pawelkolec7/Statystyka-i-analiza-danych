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

