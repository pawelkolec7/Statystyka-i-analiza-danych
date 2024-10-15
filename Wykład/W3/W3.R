#Wartość oczekiwana - srednia liczba zdjęć jakie zrobiliśmy jak byśmy je robiliśmy w nieskończość, średnio powinno byc tyle zdjęć
#Ile powinniśmy się spowiedziewać poprawnych wyników
#Odchylenie standardowe - odchyły od wartości oczekiwanej,ile średnio się odchylają wartości od wartości oczekiwanej
#To samo co na prezentacji
n=20
x=0:n
p=0.1

prob = dbinom(x,n,p)

rbind(x,prob)

expect=sum(x*prob)

variance=sum(x^2*prob)-expect^2
std=sqrt(variance)

#genreowanie danych 5 liczb do 3, z rozkładem prawdopodobieństwa 0.6
rbinom(5,n,p)

#binom - rozkład dwumianowy

plot(x,prob, type="h")

#n-liczba aparatów
#p-sznasa na zrobienie zdjęcia
#w zależnoći od parametru n i p - inny wykres
plot(x,prob, type="h")

#funkcja gęstości - idzie po słupkach - waga dziecka
#pole pod krzywą równe 1, leży nad osią
#punkt nie ma pola - prawdopodobieństwo równe 0

#rozkład wykładniczy - brak pamięci - zadanie z tańmą magnetofonową

rm(x)#kasuj znaczenie x
curve(0.01*exp(-0.01*x),0,500)
f=function(x){0.01*exp(-0.01*x)}
#całka w R:
integrate(f,0,50)

lam = 0.01
pexp(50, lam)


#b P(X>0.9995)=1-F(0.9995)
1-pnorm(0,995, mu, sig)

#b P(X>0.9995)=1-F(0.9995)
1-pnorm(0,995, mu, sig)



