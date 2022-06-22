setwd("C:/Users/Magali Giansiracusa/Desktop/labo")
#tp1 Magali Giansiracusa
listado <- read.csv("nombres-2000-2004.csv")
listado
# el dataset contiene la siguiente info: nombres de nacidos entre los años 2000 y 2004, junto con su respectiva cantidad de repeticiones
# y el año en el que dichas repeticiones sucedieron
#cambiar los nombresa minúscula

listado$nombre <- tolower(listado$nombre)
#cambié los nombres a minúscula
length(unique(listado$nombre))
#en total hay 517925 nombres distintos
anio2000 <- listado[listado$anio==2000,]
anio2001 <- listado[listado$anio==2001,]
anio2002 <- listado[listado$anio==2002,]
anio2003 <- listado[listado$anio==2003,]
anio2004 <- listado[listado$anio==2004,]
listadoAnios <- list(anio2000,anio2001,anio2002,anio2003,anio2004)
listadoAnios
for(i in 1:5){
  print(length(unique(listadoAnios[[i]]$nombre)))
}
#en el año 2000 hay 158179 nombres distintos
#en el año 2001 hay 150701 nombres distintos
#en el año 2002 hay 154105 nombres distintos
#en el año 2003 hay 163210 nombres distintos
#en el año 2004 hay 171119 nombres distintos
#se puede concluir que tiene que haber una interseccíon entre los distintos años
#porque la sumatoria total de los años es menor a la sumatoria individual total de todos los años por separado
sum(158179,150701,154105, 163210, 171119)
#la sumatoria por separado dio 797314, por lo que la intersección a priori es la resta entre el total y la sumatoria
#279389
797314- 517925
J <- matrix(ncol = 5,nrow = 5)
#para llenar esta matriz, hago un doble for para ir recorriendola tanto en filas como columas
# y completo con la división entre la intersección y la unión
for(i in 1:5){
  for(j in 1:5){
    inter <- intersect(listadoAnios[[i]]$nombre,listadoAnios[[j]]$nombre)
    uni <- union(listadoAnios[[i]]$nombre,listadoAnios[[j]]$nombre)
    J[i,j]<-length(inter)/length(uni)
  }
}
J
#        [,1]      [,2]      [,3]      [,4]      [,5]
#[1,] 1.0000000 0.1962078 0.1952402 0.1919984 0.1859885
#[2,] 0.1962078 1.0000000 0.2027606 0.1985911 0.1919966
#[3,] 0.1952402 0.2027606 1.0000000 0.2058148 0.1993893
#[4,] 0.1919984 0.1985911 0.2058148 1.0000000 0.2030724
#[5,] 0.1859885 0.1919966 0.1993893 0.2030724 1.0000000
#La diagonal de la matriz es de 1 porque estamos comparando años iguales entonces la inter y la uni serán iguales
# se puede notar que a medida que los años se van distanciando más entre si, el numero de Jaccard decrece
# esto quiere decir que hay menos inter, lo que tiene sentido si se piensa que a medida que van pasando
#los años se hacen mas "populares" otros nombres
for(i in 1:5){
  print(head(listadoAnios[[i]]$nombre,10))
  print(head(rev(listadoAnios[[i]]$nombre),10))
}
# se puede notar que la gran mayoría de los nombres mas populares se van repitendo
#a medida que pasan los años, algo común ya que las "modas" de nombres tardan varios
#años en cambiar
#con respecto a los menos usados, se los caracteriza por ser una combinación
#de nombres no convencional (como por ejemplo juan manuel que si lo es)
#además, suelen ser nombres más comunes en otros paíces o idiomas (como yacqueline o katherine)

listadoTotal <- aggregate(cantidad ~ nombre, data = listado, sum)
listadoTotal
#2
da_el_paso <- function(x0){
  return(x0+runif(1,-1,1))
}
da_el_paso(5)

esta_entre <- function(x0,T0,T1){
  return(T0<x0 & T1>x0)
}
esta_entre(1,2,3)

camina<- function(x0,T0,T1){
  paso <- 0
  while (esta_entre(x0,T0,T1)) {
    x0 <-da_el_paso(x0)
    paso<-paso + 1
  }
    
  return(c(paso,x0>T1))
}
#uso replicate para simular las 1000 caminatas
experimento <- replicate(1000,camina(0,-1,10),simplify = F)

llegadosAT1 <- 0
#los que llegaron a t1
for(i in 1:1000){
  llegadosAT1 <- llegadosAT1 + experimento [[i]][2]
  
}
llegadosAT1
#los que llegaron a t0
llegadosAT0 <- 1000 - llegadosAT1

#para saber cuanto duran uso el promedio como método
sumatoriaT0 <- 0
sumatoriaT1 <- 0
for(i in 1:1000){
  if (experimento[[i]][2]==0){
    sumatoriaT0 <- sumatoriaT0 + experimento [[i]][1]
  }else{
    sumatoriaT1 <- sumatoriaT1 + experimento [[i]][1]
  }
  
  
}
promedioT0 <- sumatoriaT0/llegadosAT0
promedioT1 <- sumatoriaT1/llegadosAT1

calcular_recorrido <- function(x0,T0,T1){
  paso <- 2
  recorrido <- c(x0)
  while (esta_entre(x0,T0,T1)) {
    x0 <-da_el_paso(x0)
    recorrido[paso] <- x0
    paso<-paso + 1
  }
  
  return(recorrido)
}
recorrido <- calcular_recorrido(0,-1,10)
plot(recorrido)
#se puede ver que, como en el item anterior, en promedio los caminos
#mas largos son los de t1 y los más cortos los de t0


