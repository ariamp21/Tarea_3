#EJERCICIO 1
#Opción ganadora sistema plebiscito int, int, int -> string
#DEF: Según la cantidad de votos, corroborar que se cumpla un quorum establecido (50%+1),
#si hay menos votos que la cantidad requerida, pero existe un 30% elegido en una opción, 
#gana esta opción, de lo contrario, gana el NO. Si se cumple el quorum, gana la opción con 
#más votos, en caso de empate, gana el NO.
#EJ: FuncionEleccion (100,60,40) -> SI
FuncionEleccion <- function(total, votosSI, votosNO){
  if((votosSI+votosNO)<=(total*0.5+1)&&(votosSI>=(0.3*total))){
    print ("SI")
  }else if((votosSI+votosNO)<=(total*0.5+1)&&(votosNO>=(0.3*total))){
    print ("NO")
  }else if((votosSI+votosNO)<=(total*0.5+1)&&(votosSI<=(0.3*total))||(votosNO<=(0.3*total))){
    print ("NO")
  }else if((votosSI+votosNO)>=(total*0.5+1)&& (votosSI>votosNO)){
    print ("SI")
  }else if((votosSI+votosNO)>=(total*0.5+1)&&(votosNO>votosSI)){
    print ("NO")
  }else{
    print ("NO")
  }
}

FuncionEleccion (100, 60, 40)
FuncionEleccion (100,30,10)
FuncionEleccion (100,10,20)
FuncionEleccion (100,50,50)


#EJERCICIO 2
#Capital inicial a invertir: float, float, int -> float
#DEF: A partir del capital final, con su debido interés y el n° de años que desea invertir, me retorne #el capital inicial necesario para obtener ese monto final
#EJ: FuncionCapitalInicial (1763193.69216,0.08,5) -> 1.200.000

FuncionCapitalInicial<-function(Cn,i,t){
  print(Cn/(1+i)**t)
}
FuncionCapitalInicial (1763193.69216,0.08,5)

#EJERCICIO 3:
#funcionLargo3 -> 
#Se crearán 3 listas, una contendrá un numero de palabras al azar con dis#tinta cantidad de letras, otra separará cada palabra y otra contabiliza
#rá el número de letras de cada una. Luego se realiza una función para re#presentar la totalidad de los datos en una tabla. El objetivo final era #que me entregase solo las palabras de largo 3.
#Ej: funcionLargo3 <- function(ListaLargo3,Tres) -> 

Palabra     QLetras
1   Ariam       5
2     Sol       3
3     Paz       3
4  Alicia       6
5    Miau       4
6     Mar       3

ListaLargo3 <-list("Ariam", "Sol", "Paz", "Alicia", "Miau", "Mar")

Separar <- list()
for (grepl in 1:length(ListaLargo3)){
Separar[grepl] <- c(print(strsplit(ListaLargo3[[grepl]], split="")))
}
Tres <- list()
for (Letra in 1:length(Separar)){
Tres[Letra] <- c(print(length(Separar[[Letra]])))  
}
tabla <- data.frame(Palabra=unlist(ListaLargo3), QLetras=as.numeric(Tres))
funcionLargo3 <- function(ListaLargo3,Tres){
   print (tabla)
}

#EJERCICIO 4
#ContarPuntos  ((char, char)…(char,char)) -> int
#DEF: Utilizar una función que sume los puntos totales del mazo, a través de una lista que contenga los puntos de cada carta y su frecuencia. 
#EJ: ((A,3),(J,4)) -> 104
A <- 20
J <- 11
Q <- 12
K <- 13
ListaPuntos <- list(c(A,3),c(J,4))
value <- unlist(ListaPuntos[1]
ContarPuntos <- function((x,y),(b,z)){
print (x*y)+(b*z)
}
For ((w,i) in 1:length(ListaPuntos)){
ContarPuntos (ListaPuntos[w,i])
}
