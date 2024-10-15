c(1,2,3,4,5)

dane = read.csv("C:\\Users\\kolec\\Desktop\\Semestr 4\\Statystyka\\Wykład\\W1\\ozon.csv")
table(dane)

discrete.histogram(dane)
discrete.histogram(dane,freq = T)

plot(table(dane)/length(dane))
plot(table(dane))

pie(table(dane))

rowery = c(1,4,0,2,4,5,2,3,1,3,3,3,3,4,4,5,5,5,5,2,2,2,2,0,0,0,2,2,1,1,1,1,3)
discrete.histogram(rowery)

dane1 = c(1,2,3,4,5,3,2,2,5,5)
table(cut(dane1,5))

hist(dane1, main = 'tytuł', xlab = 'etykieta osi OX')
hist(dane1, main = 'tytuł', xlab = 'etykieta osi OX', freq = FALSE)

pie(table(cut(dane1,5)))

max(dane)
min(dane)
ceiling(dane)

mean(dane1)

quantile(dane1, probs = 0.1)
quantile(dane1)
summary(dane1)

var(dane1)
sd(dane1)

boxplot(dane1)

grupaA = c(3.0, 3.0, 4.0, 4.5, 4.5)
grupaB = c(2.0, 3.5, 4.0, 4.5, 5.0)
boxplot(grupaA, grupaB)