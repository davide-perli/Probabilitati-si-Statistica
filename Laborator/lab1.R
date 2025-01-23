#Lab2 Probabilitati si Statistica

#Afisarea unui mesaj
cat("Laboratorul a inceput", "de",5,"minute")
objects()
#alternativ ls()
cat("Obiectele din sesiunea curenta de R sunt:\n", objects())
rm() #dau lista obiectelor pe care vreau sa le sterg
#Q: Daca vreau sa le sterg pe toate cum procedez?
rm(list = objects())
objects()
#Tipuri de obiecte in R: vectori, liste, matrice, dataframes(ca o matrice dar fiecare coloana poate sa contina alt tip), factors, functii
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
#nu merge deoarece d6 poate sa fie asignat cu valori pe rand si nu simultan, nefiind cunoscuta prioritatea asignarii (prioritati necunoscute pentru d2+d4 si d1)

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
#e ilustrata prioritatea operatiilor de la mare la mica : > * > +. parantezele obliga evaluarea expresiei care are o operatie cu o prioritate mai mica, spre exemlplu diferenta de rezultat intre liniile 50 si 48
#: descrie o secventa care incepe cu elem din st si se termina cu cel din dreapta (inclusiv), incrementarea pentru fiecare valoare fiind 1 (in cazul in care nu apare operatorul *); decrementarea are loc numai daca val din st > val din dr
#Q: Explicati comportamentul operatorului :
1.4:5
#in acest caz secventa porneste de la 1 si se termina pana la valoarea lui 5 (strict mai mic), fiind o valoare cu .4 

#I.1.3 Functia seq
seq(5,9,0.1) #generarea unei secvente de numere
seq(5,9,length.out = 11)

#I.1.4 Functia rep
rep(1,2)
rep(c(2,3),5)
rep(c(2,3),c(1,3))

#Q: Explicati urmatoarea secventa de cod
rep(c(0,2),3)
rep(rep(c(0,2),3),1:6)
c(0:3,6)
c(2,4:7)
rep(c(0:3,6),c(2,4:7))
rep(rep(c(0:3,6),c(2,4:7)),24)
(b <- c(rep(rep(c(0,2),3),1:6),rep(rep(c(0:3,6),c(2,4:7)),24)))

#I.1.5 Operatorul de selectie []
v5 <- v3[1:4]
v6 <- v3[c(1,5,6)]
v7 <- v3[v3>100]
v7 <- v3[(v3>100)&(v3<110)]

#Q: Explicati urmatoarea secventa de cod
x <- sample(1:1000,100) # valorile intre 1 si 1000 randomised, in total fiind 100 de astfel de val
x[x%%8==0][1:3] # primele 3 val care dau 0 la impartirea cu 8 x[conditie][interval indici cu val cu ac conditie]

#Q: Explicati urmatoarea secventa de cod
x_1 <- x[-(1:3)] # toate val lui x cu exceptia primelor trei valori

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
c1 <- a1+b # se aduna fiecare elem cu acelasi indice din vectorii a1 si b, iar pt ca lungimea b e mai mica ca cea a lui a1, deci cicleaza indicii de la b, continuandu-se cu restul elementelor din a1 (pozitiile 4, 5 si 6 ca indici nu au val coresp pt b, dar 4 = 3*1+1, 5 = 3*1+2, 6 = 3*2, asadar c1[indice>3] = b[3n+k]+a1[indice], daca indice de forma 3n+k, k=0,1,2)

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
#elem sunt transformate intr-un tip general, pt ca vectorul e format dintr-un singur tip de elem (date omogene)
##########################################################################

#I.4 Vectori logici

#sunt creati de obicei prin evaluarea unor expresii logice
x <- sample(1:100,10)
y <- (x[1:50]<30)
class(y) #vector cu val de adevar pt fiecare val daca respecta sau nu conditia < 30
(y)
#Q: Ce este in neregula cu expresia de mai sus?
#y presupune ideea ca x are mai mult decat 50 de elemente, insa lungimea vectorului x este de 10, de aceea apar val de NA pentru elem cu indicii de la 11 la 50

#NA=not available

#Q: Cum putem elimina in mod automat valorile NA dintr-un vector?
#HINT: is.na()
#!is.na(vector) inlocuieste toate val cu TRUE, iar is.na() cu FALSE, deci trebuie sa fie o conditie pentru care se aleg elementele vectorului x
y <- (x[!is.na(x)]<30)
(y)

z <- x[1:5]<x[6:10]

#Q: Creati un vector logic t ce compara daca elementul de pe pozitia i(impara) 
# este mai mic decat elementul de pe pozitia para imediat urmatoare
2*0:(length(x))+1
2*1:length(x)
x[2*0:(length(x)/2-1)+1] # impartire lungime vector x la doi pentru a scapa de valorile de NA
#var 2: x[!is.na(x[2*0:length(x)+1])]
x[2*1:(length(x)/2)]
vec <- (x[2*0:(length(x)/2-1)+1]<x[2*1:(length(x)/2)])
(vec)

#Q: Determinati pozitiile pentru care conditia de la prima intrebare este adevarata
#HINT: which()
vec_pos <- which(vec)
(vec_pos)

#Q: Determinati cate numere cuprinse intre 411 si 7870 sunt divizibile cu 9
#dar nu cu 5
num <- 411:7870
nr <- length(num[num%%9==0 & num%%5!=0])
(nr)

###########################################################################
# Pachetul prob
library(prob)
rolldie(1)
tosscoin(1)


#To do

# Construiti doi vectori x si y cu 1000 de elemente fiecare, extrase in mod
#aleator din multimea cu numere intregi -24500:76000.
x <- sample(-24500:76000, 1000)
y <- sample(-24500:76000, 1000)
#a)Stabiliti care dintre cei doi vectori are mai multe elemente,
#luate in valoare absoluta, mai mari decat valoarea absoluta a elementului
#corespondent din celalalt vector
x_y <- c(x, y)[abs(x)>abs(y)]
y_x <- c(x, y)[abs(x)<abs(y)]
nr_x_y <- sum(abs(x)>abs(y))
nr_y_x <- sum(abs(x)<abs(y))
if(nr_x_y>nr_y_x)
{
  print(cat("x cu ", nr_x_y, " elem mai mari castiga"))
} else if(nr_x_y<nr_y_x)
{
  print(cat("y cu ", nr_y_x, " elem mai mari castiga"))
}
#b)Stabiliti care dintre cei doi vectori are minimul pe o pozitie mai mare
x_min_pos <- which.min(x)
y_min_pos <- which.min(y)
if(x_min_pos<y_min_pos)
{
  print(cat("Pozitia minima e in vect x: cu val ", min(x), ", poz ", x_min_pos))
} else if(x_min_pos>y_min_pos)
{
  print(cat("Pozitia minima e in vect y: cu val ", min(y), ", poz ", y_min_pos))
}
#c)Stabiliti care dintre cei doi vectori are cele mai multe valori care se repeta
# vector de frecvente folosind table(); perechi val: aparitii_val
table(x)
table(y)
max_freq_x = max(table(x))
max_freq_y = max(table(y))
if(max_freq_x<max_freq_y)
{
  print(cat("Maximul de repetii ale unei val apartine vect y, ea fiind ", max_freq_y))
} else if(max_freq_x>max_freq_y)
{
  print(cat("Maximul de repetii ale unei val apartine vect x, ea fiind ", max_freq_x))
}
#d)Stabiliti care dintre cei doi vectori are o secventa de cel putin 2 valori consecutive
if(any(diff(sort(x))==1))
{
  print("Vectorul x are cel putin 2 val consecutive")
}
if(any(diff(sort(y))==1))
{
  print("Vectorul y are cel putin 2 val consecutive")
}
#e)Stabiliti care dintre cei doi vectori are mai multe valori divizibile cu corespondentele
#lor din celalalt vector.
k_x_div <- sum(abs(x)%%abs(y)==0 & y!=0)
k_y_div <- sum(abs(y)%%abs(x)==0 & x!=0)
if(k_x_div>k_y_div)
{
  print("x are mai multe val divizibile")
} else if(k_x_div<k_y_div)
{
  print("y are mai multe val divizibile")
}
