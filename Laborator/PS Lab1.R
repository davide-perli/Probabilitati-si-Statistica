                                            #Lab2 Probabilitati si Statistica

#Afisarea unui mesaj
cat("Laboratorul a inceput", "de",5,"minute")
objects()
#alternativ ls()
cat("Obiectele din sesiunea curenta de R sunt:\n", objects())
rm() #dau lista obiectelor pe care vreau sa le sterg
#Q: Daca vreau sa le sterg pe toate cum procedez?
#Tipuri de obiecte in R: vectori, liste, matrice, dataframes, factors, functii
#Tipuri de date: numeric(integer, double), complex, character, raw

#Operatorul de atribuire
u <- 3
3->v
d1 <- 1
d2 <- 2
d3 <- 3
d4 <- 4
d5 <- 5
d6 <- 6
rez <- d1+d3->d4->d5
#Q: Explicati de ce nu merge urmatoarea atribuire multipla
d2+d4 ->d6<-d1

#I.Vectori
#I.1. Atribuire

#I.1.2.1 Functia c()
v <- c(1,3,7) #functia de concatenare
v1 <- c(2,v)
v2 <- c(v,2)
v3 <- c(3,c(1,2),8,9:120)
#Atribuire si afisare
(v4 <- c(v1,v2,3))

#I.1.2. Operatorul :
1:5
v <- 1:7
n <- 10
#Q: Explicati rolul parantezelor si al operatorului : in urmatoarea situatie
1:n-1
1:(n-1)
2*1:5
2*1+1:5
2+1:5
(2*1+1):5
30:3
#Q: Explicati comportamentul operatorului : 
1.4:5

#I.1.3 Functia seq
seq(5,9,0.1) #generarea unei secvente de numere
seq(5,9,length.out = 11)

#I.1.4 Functia rep
rep(1,2)
rep(c(2,3),5)
rep(c(2,3),c(1,3))

#Q: Explicati urmatoarea secventa de cod
(b <- c(rep(rep(c(0,2),3),1:6),rep(rep(c(0:3,6),c(2,4:7)),24)))

#I.1.5 Operatorul de selectie []
v5 <- v3[1:4]
v6 <- v3[c(1,5,6)]
v7 <- v3[v3>100]
v7 <- v3[(v3>100)&(v3<110)]

#Q: Explicati urmatoarea secventa de cod
x <- sample(1:1000,100)
x[x%%8==0][1:3]

#Q: Explicati urmatoarea secventa de cod
x_1 <- x[-(1:3)]

#I.2. Functii uzuale
length(x)
min(x)
max(x)
sort(x)
sum(x)
prod(x)
range(x)
exp(x)
abs(x)
log(abs(x))
sqrt(abs(x))
sin(x)
cos(x)

#I.3. Operatii cu vectori
a <- 1:3
b <- 4:6
c <- a+b
d <- a*b
e <- 3*a
f <- a^3
a1 <- 1:6

#Q: Explicati ce face urmatoarea comanda
c1 <- a1+b

###########################################################################
#Intermezzo: exemple de tipuri de date
x <- 3.14
y <- 2.44
z <- x + y
#afisez tipul obiectului
typeof(z)
#afisez clasa din care face parte
class(z)
#verific daca este de tip numeric
is.numeric(z)
#definesc o variabila de tip integer
nr_studenti <- as.integer(28)
typeof(nr_studenti/3)
#definesc o variabila de tip complex
complex1 <- as.complex(-2+2i)
#definesc un obiect ce contine 3 numere complexe si il afisez
(complex2 <- complex(3,10,2))
vector_numeric <- c(1,2,10)
class(vector_numeric)
vector_caracter <- c("Text","nou!")
vector_caracter
class(vector_caracter)
(vector_integer <- 1:10)
class(vector_integer)
class(vector_logic <- c(TRUE, FALSE))
vector_mixt <- c(1,2,"a","b")
vector_mixt
class(vector_mixt)
# Q: De ce vector_mixt este in continuare un vector si nu o lista?

##########################################################################

#I.4 Vectori logici

#sunt creati de obicei prin evaluarea unor expresii logice
x <- sample(1:100,10)
y <- (x[1:50]<30)
#Q: Ce este in neregula cu expresia de mai sus?

#NA=not available

#Q: Cum putem elimina in mod automat valorile NA dintr-un vector?
#HINT: is.na()

z <- x[1:5]<x[6:10]

#Q: Creati un vector logic t ce compara daca elementul de pe pozitia i(impara) 
# este mai mic decat elementul de pe pozitia para imediat urmatoare

#Q: Determinati pozitiile pentru care conditia de la prima intrebare este adevarata
#HINT: which()

#Q: Determinati cate numere cuprinse intre 411 si 7870 sunt divizibile cu 9
#dar nu cu 5


###########################################################################
# Pachetul prob
library(prob)
rolldie(1)
tosscoin(1)


#To do

# Construiti doi vectori x si y cu 1000 de elemente fiecare, extrase in mod
#aleator din multimea cu numere intregi -24500:76000.
#a)Stabiliti care dintre cei doi vectori are mai multe elemente,
#luate in valoare absoluta, mai mari decat valoarea absoluta a elementului
#corespondent din celalalt vector
#b)Stabiliti care dintre cei doi vectori are minimul pe o pozitie mai mare
#c)Stabiliti care dintre cei doi vectori are cele mai multe valori care se repeta
#d)Stabiliti care dintre cei doi vectori are o secventa de cel putin 2 valori consecutive
#e)Stabiliti care dintre cei doi vectori are mai multe valori divizibile cu corespondentele
#lor din celalalt vector.

#2) 