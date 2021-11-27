library(lifecontingencies)
library(tidyverse)
library(lubridate)
library(readxl)
library(kableExtra)



Afiliados_31_12_2020 <- read_csv("afiliados.csv")
Pensionados_31_12_2020 <- read_csv("pensionados.csv")
tabla_mortalidad_mujeres<-read_excel("tabla_mortalidad_mujeres.xlsx")
tabla_mortalidad_hombres<-read_excel("tabla_mortalidad_hombres.xlsx")






























































## Estocastico

#La idea del codigo es basarse en el paradigma de divide y venceras
#Se procede a programar el modelo estocasticos
##Primero se procede a programar una funcion la cual determina
##la probabilidad de que la persona muera en ese año
##La idea sería ver cual es la probabilidad de que la persona
##en esa edad x se muera en ese mismo año, para pasar
##este parametro a otra funcion la cual simula si la persona
##muere o no
###Recibe la parámetros
###edad puede ser un vector
###ano este es el año en el que se quiere hacer la evaluacion
###tabla es la tabla de mortalidad, debe ser dinámica
###j tiempo posterior al año de partida
qx<-function(edad,anho,tabla,tiempo){
  entrada<-which(as.numeric(colnames(tabla)[-1])==anho)
  prob_muerte<-tabla[edad+tiempo,entrada+tiempo]
  return(as.numeric(prob_muerte))
}

antiguedad<-function(n){
  return(c(
    (n>10 & n<=20)*0.45+ ##Rettonra el porcentaje de pension
      (n>20 & n<=30)*0.65+
      (n>30 & n<=40)*0.85+
      (n>40),
    1+(n>10 & n<=20)+ #Retorna el monto lumpsum
      (n>20 & n<=30)*2+
      (n>30 & n<=40)*3+
      (n>40)*4
  )
  )
}

#Se programa una funcion la cual se encarga de calcular el 
#monto de cada persona e irla sumando a la cantidad para ver
#costo de una simulación
#Se asume distribucion uniforma de muertes
#Aplicar la funcion para hombres y otra para 
#mujeres
#base_de_datos corresponde a la base de 
#afiliados la cual se otoroga
#t_descuento corresponde a la tasa nominal de 
#de descuento
#inflacion es la inflacion 
#La jubilacion va por persona
#A pesar de que esto es el seguro de costos 
#funerarios, se programa una funcion a parte para la pension
costo_afiliados_seguro<-function(anho,tabla,base_de_datos,t_descuento,inflacion,aumento,jubilacion,simulaciones,m){
  edades<-base_de_datos$Edad
  antiguedades<-base_de_datos$Antiguedad
  devengados<-base_de_datos$DEVENGADO
  
  v<-1/(1+t_descuento)
  
  aumento_nominal<-(1+inflacion+aumento)
  fn1<-function(edad,Antiguedad,devengado){###Me quede aqui
    
    aux<-0
    
    edad<-edad
    
    simulacion<-simulaciones
    
    vivos<-0
    
    j<-1
    
    Antiguedad<-Antiguedad+(jubilacion-edad)
    
    while(simulacion>0 & edad<jubilacion){
      vivos<-sum(rbinom(simulacion,1,1-qx(edad,anho,tabla,j)))
      
      muertos<-simulacion-vivos
      
      simulacion<-vivos
      
      aux<-aux+v^j*muertos
      
      edad<-edad+1
      
      
      j<-j+1
      
    } 
    #print(edad)
    aux<-aux*2000000
    ultimo_salario<-devengado*aumento_nominal^j
    antiguedad<-antiguedad(Antiguedad)
    pension<-antiguedad[1]*ultimo_salario
    aux<-aux+antiguedad[2]*ultimo_salario*simulacion*v^j
    +v^j*pension(anho,edad,tabla,t_descuento,j,pension,m,simulacion,seguro=1000000,inflacion)
    return(aux/simulaciones)
  }
  valores<-mapply(FUN=fn1,edad=edades,Antiguedad=antiguedades,devengado=devengados)
  suma<-sum(valores)
  
}

##Se procede a programar una funcion la cual permite calcular el monto de la pension
##la idea esta funcion es que la funcion costos_afiliados_envie la cantidad de la poblacion
##ha sobrevivido hasta el momento
##Se le deben pasar el parametro simulacion que indica la cantidad de sobrevivientes hasta
##ese momento
##El momento es el momento j
##El anhno tambien debe ser pasado, para poder ubicarse en la tabla de mortalidad
##Tambien se le deben pasar la inflacion y tambien tasa de descuento
pension<-function(anho,edad,tabla,t_descuento,j,salario,m,simulaciones,seguro=1000000,inflacion){
  simulacion<-simulaciones
  
  meses<-1
  
  suma<-0
  
  k<-0
  
  salario<-salario
  
  muertos<-0
  
  v<-1/((1+t_descuento)^(1/m))
  v1<-1/((1+t_descuento))
  
  while(simulacion>0){
    vivos<-sum(rbinom(simulacion,1,1-meses/12*qx(edad,anho,tabla,j)))
    
    muertos<-muertos+simulacion-vivos
    
    simulacion<-vivos
    
    suma<-suma+salario*v^(meses+m*k)*vivos
    
    suma<-suma+muertos*v1^(k+1)*seguro*(meses==12 | simulacion==0)
    
    salario<-salario*(1+inflacion)^(meses==12)
    
    muertos<-muertos*(meses<12)+0
    
    k<-k+(meses==12)
    
    j<-j+(meses==12)
    
    
    edad<-edad+(meses==12)
    
    meses<-(meses==12)+(meses<12)*(1+meses)
  }
  #print("Aqui")
  return(suma)
}


##Se procede a programar una funcion para la base de pensionados que usa lo programado
#en la funcion programada 
pensionados<-function(anho,tabla,base_de_datos,t_descuento,
                      j,m,simulaciones,seguro=1000000,inflacion){
  
  edades<-base_de_datos$Edad
  
  salarios<-base_de_datos$MON_PENSION #Consulta hay que ajustar la pension o no
  
  fn2<-function(edad,salario){
    monto<-pension(anho,edad,tabla,t_descuento,j,salario=salario,m,simulaciones,seguro=1000000,inflacion)/simulaciones
    return(monto)
  }
  
  valores<-mapply(FUN=fn2,edad=edades,salario=salarios)
  
  suma<-sum(valores)
  
  return(suma)
}






#A partir de lo anterior se procede a programar una funcion la cual 
#calcula el costo de la pension a apartir de las dos funciones anteriores
#debe recibir las dos bases de datos de afiliados y la base de pensionados
#y todos los parametros que recibe la pension de costo de afiliados

costo_plan<-function(anho,mujeres,hombres,t_descuento,inflacion,aumento,afiliados,pensionados,simulaciones,m){
  afil_mujeres<-afiliados%>%filter(SEXO=="F")
  
  afil_hombres<-afiliados%>%filter(SEXO=="M")
  
  pen_mujeres<-pensionados%>%filter(SEX=="F")
  
  pen_hombres<-pensionados%>%filter(SEX=="M")
  
  sumar_mujeres<-costo_afiliados_seguro(anho,mujeres,afil_mujeres,t_descuento,inflacion,aumento,62,simulaciones,m)+pensionados(anho,mujeres,pen_mujeres,t_descuento,j=1,m,simulaciones,seguro=1000000,inflacion)
  sumar_hombres<-costo_afiliados_seguro(anho,hombres,afil_hombres,t_descuento,inflacion,aumento,65,simulaciones,m)+pensionados(anho,hombres,pen_hombres,t_descuento,j=1,m,simulaciones,seguro=1000000,inflacion)
  return(sumar_mujeres+sumar_hombres)
}





##Prueba de simulacion se ecirbe para evitar perdida de datos
# prueba<-Afiliados_31_12_2020[1,]


calculo_prueba<-costo_plan(2021,tabla_mortalidad_mujeres,tabla_mortalidad_hombres,0.06,0.03,0.02,Afiliados_31_12_2020,
                           Pensionados_31_12_2020,100000,12)

write.table(calculo_prueba,file="prueba.txt",sep="")


