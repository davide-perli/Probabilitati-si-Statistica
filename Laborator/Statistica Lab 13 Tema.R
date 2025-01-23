#Statistica Lab 13 

#Metoda verosimilitatii maxime
#Daca o functie cu valori pozitive are punct de maxim sau minim local,
#acesta se pastreaza si daca se logaritmeaza functia initiala
f1 <- function(x)
{
  sin(x)
}

t <- seq(0.001,pi-0.001,0.001)
#Q: De ce nu am luat intervalul (0,pi)?

plot(t,f1(t),type="l",ylim=c(-1,1))
lines(t,log(sin(t)),col="blue")
abline(v=pi/2,col="red")

#gasirea punctelor de extrem cu functia optimize
require(graphics)
#in mod implicit cauta punctul de minim
optimize(f1,c(0.001,pi-0.001))

#vreau punctul de maxim

optimize(f1,c(0.001,pi-0.001),maximum=T)


# Metoda MVM liniile 28-61
#Problema cu f(x)=x^2/(54*teta^3)*exp(-x/(3*teta))

#const=ln(produs(xi^2))
#x este vectorul ce reprezinta esantionul dat
x <- rgamma(1000,3,90)
n_calc <- 1000
xbar_calc <- sum(x)/n_calc #mean(x)
const_calc <- sum(2*log(x)) # variabila care nu trebuie calculata de fiecare data in logaritmarea verosimilitatii, e calculata o singura data

logVerosim <- function(teta,xbar,n,const)
{
  -n*log(54)-3*n*log(teta)+const-1/(3*teta)*n*xbar 
}
t <- seq(0.001,0.5,0.0001)
plot(t,logVerosim(t,xbar_calc,n_calc,const_calc),type="l")

#Q: Ce reprezinta 100 mai jos?
#"A wild guess" legat de intervalul pe care sa cautam punctul de maxim
#Daca facem plot inainte vedem mai bine care e intervalul mai bun

o <- optimize(logVerosim,c(0.001,100),maximum=T,xbar=xbar_calc,n=n_calc,const=const_calc)
teta_maxim <- o$maximum
#ce obtin daca iau intervalul de cautare mai fin
o <- optimize(logVerosim,c(0.001,0.1),maximum=T,xbar=xbar_calc,n=n_calc,const=const_calc)
teta_maxim <-o$maximum

teta_estimat <- xbar_calc/9

#Observam ca teta_estimat si teta_maxim(calculat prin metode numerice) sunt foarte apropiate

t <- seq(0.001,0.5,0.0001)
plot(t,logVerosim(t,xbar_calc,n_calc,const_calc),type="l")
abline(v=teta_maxim,col="red")

#Metoda momentelor
#De ce metoda momentelor uneori nu e buna

#Folositi metoda momentelor pentru a estima teta pentru X~Unif(0,teta) in baza unui 
#esantion de volum 100

#Estimatorul dat de metoda momentelor este teta=2*xbar

teta <- 18
sample_size <- 30
nr_simulari <- 1000
teta_estimat <- numeric(nr_simulari)

for( i in 1:nr_simulari)
{
  u <- runif(sample_size,0,teta)
  xbar <- mean(u)
  teta_estimat[i] <- 2*xbar
}
hist(teta_estimat)
plot(density(teta_estimat))
abline(v=18, col="green")

#y <- runif(1000,6,13)
#hist(y)

#MVM pentru aceeasi problema
#teta_MVM=max(valorile din esantion)
teta <- 18
x <- runif(1000,0,teta)
teta_MVM <- max(x)

for( i in 1:nr_simulari)
{
  u <- runif(sample_size,0,teta)
  teta_estimat[i] <- max(x)
}
hist(teta_estimat)
plot(density(teta_estimat))
abline(v=18, col="green")

#Tema: Optimizati codul de mai sus
# pentru MM liniile 71-84

# setez parametrii si un seed pentru ca rezultatul sa fie acelasi cand e reprodus
set.seed(25281)# patratul lui 159
teta <- 18
sample_size <- 30 # in fiecare simulare a modelului sunt extrase 30 de esantioane
nr_simulari <- 1000 # 1000 de simulari

# generez toate esantioanele o data, intr-o matrice cu functia matrix cu 1000 de randuri care de fapt sunt nr_simulari si 30 de coloane care e fiecare esantion calculat (in numar de 30) pentru fiecare simulare
# runif genereaza nr_simulari * sample_size numere random dintr-o distributie care va lua valori intre 0 si teta care de fapt e 18
u <- matrix(runif(nr_simulari * sample_size, 0, teta), nrow = nr_simulari)# pana la urma fiecare rand de fapt contine randurile unei simulari

# calculez media pentru fiecare rand al matricei u,insemnand ca o simulare va avea o valoare in vectorul meu de valori de xbar-uri
xbar <- numeric(nr_simulari)# 1000 de randuri pt 1000 de simulari

for (i in 1:nr_simulari)
{
  row_sum <- 0
  for (j in 1:sample_size)
  {
    row_sum <- row_sum + u[i, j]
  }
  xbar[i] <- row_sum / sample_size
  teta_estimat[i] <- 2 * xbar# la finalul fiecarei simulari calculez estimatorul in functie de xbar (exemplul de mai sus)
}
hist(teta_estimat)
plot(density(teta_estimat))
abline(v = 18, col = "green")

## pentru MVM liniile 91-102

x <- runif(1000, 0, teta)# esantion uniform
teta_MVM <- max(x)# aplic functie de max pe fiecare esantion 
# teta_MVM e o valoare foarte apropiata de 18 (cum ar fi 17.99775...)

## Metoda MVM pentru problema de la clasa (grupa 232, lista de probleme subpunctul j))
#Problema cu f(teta)(x) = 2/teta*x*e^(-x^2/teta), x apartine (0,1), teta > 0

x <- rgamma(1000,3,90)# vector esantioane
n_calc <- 1000
xbar_calc <- sum(x)/n_calc # media 1/n*(suma dupa i de la 1 la n din)xi
const_calc <- sum(log(x)) # variabila care nu trebuie calculata de fiecare data in logaritmarea verosimilitatii, e calculata o singura data
# log(a*b)=log(a)+log(b)

logVerosim <- function(teta,xbar,n,const)
{
  n*log(2)-n*log(teta)+const-1/teta*n*xbar 
}
t <- seq(0.001,0.5,0.0001)
plot(t,logVerosim(t,xbar_calc,n_calc,const_calc),type="l")

o <- optimize(logVerosim,c(0.001,100),maximum=T,xbar=xbar_calc,n=n_calc,const=const_calc)
teta_maxim <- o$maximum

# estimatorul calculat numeric doar folosind MVM, altfel cel folosind MM difera
teta_estimat <- xbar_calc

