# Análisis de la notas de la asignatura Ftos. De Técnología de Computadores
Realizado por Rosa Estriégana Valdehita

### Introducción

> Este trabajo realiza un análisis de los resultados obtenidos en los cursos 2009-2010, 2010-2011, 2011-2012, 2012-2013 y 2013-2014 en la asignatura de Fundamentos de Tecnología de Computadores, asignatura del primer cuatrimestre de los grados de Ingeniería Informática, Ingeniería de Computadores y Sistemas de Información. 
El objetivo es tratar de responder a una serie de preguntas.

> ¿Son mejores los resultados generales (notas) de los alumnos cuando se hace que estos participen más activamente en su aprendizaje? ¿Son efectivas las técnicas de aprendizaje activo? 
Para ello se realizará una comparativa entre los resultados de diferentes grupos  de alumnos en diferentes años empleando metodología "tradicional" frente a este último año 2013-2014 en el que se han empleado técnicas de aprendizaje activo en dos de los grupos.

### Librerías

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 2.15.3
```

```r
library(grid)
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 2.15.3
```

```r
require(gridExtra)
```


### Obtención de los datos

```r
setwd("C:/Analisis Ftos")
notas <- read.csv("Datos/processed/dataFtos.csv")
```


### Exploración de datos

```r
head(notas, n = 1)
```

```
##       AÑO CONVOCATORIA PLAN
## 1 2009-10          ENE G58 
##                                                                      NOM.PLAN
## 1 GRADO EN SISTEMAS DE INFORMACIÓN                                           
##     ASIG
## 1 780002
##                                                                                               NOM.ASIG
## 1 FUNDAMENTOS DE TECNOLOGÍA DE COMPUTADORES                                                           
##                     CALIFIC    DEPTO
## 1 Aprobado                  Z029    
##                                                                     NOM.DEPTO
## 1 Automática                                                                 
##   TOTAL.CALIF GRUPO_ASSIG PROFESOR USOVIDEOS
## 1          22           0        1         0
```


### Preparación de los datos
> Eliminamos espacios en blanco en la variable CALIFIC
Añadimos una columna en la que asignaremos los valores numéricos correspondientes a las notas.  
Equivalente numérico de la escala 0-10 ponderada: 
SUSPENSO = 2.5
APROBADO = 5.5 
NOTABLE = 7.5 
SOBRESALIENTE = 9.0 
MATRÍCULA DE HONOR = 10 
NO PRESENTADO = 1.0 
> En el caso de "NO PRESENTADO" es difícil asignar un valor numérico, pero se ha considerado que sí debe ser tenido en cuenta y que es el caso menos deseado. Vamos a asignarle un valor de 1. 

> Añadimos también la columna TOTAL que es el nº de alumnos que sacan una calificación (CALIFIC) por el valor de dicha calificación


```r
notas$CALIFIC <- as.character(notas$CALIFIC)
notas$CALIFIC <- (gsub(" ", "", notas$CALIFIC))

convierteNUM <- function(d) {
    
    for (i in 1:length(d$CALIFIC)) {
        d$CALIFIC[i] <- as.character(d$CALIFIC[i])
        
        
        if (d$CALIFIC[i] == "Aprobado") 
            d$CALIFIC.NUM[i] <- 5.5 else if (d$CALIFIC[i] == "Matríc.Honor") 
            d$CALIFIC.NUM[i] <- 10 else if (d$CALIFIC[i] == "NoPresentado") 
            d$CALIFIC.NUM[i] <- 1 else if (d$CALIFIC[i] == "Notable") 
            d$CALIFIC.NUM[i] <- 7.5 else if (d$CALIFIC[i] == "Sobresaliente") 
            d$CALIFIC.NUM[i] <- 9 else d$CALIFIC.NUM[i] <- 2.5
        
    }
    return(d$CALIFIC.NUM)
}

notas$CALIFIC.NUM <- convierteNUM(d <- as.data.frame(notas))

notas$TOTAL <- notas$CALIFIC.NUM
for (i in 1:length(notas$CALIFIC)) {
    notas$TOTAL[i] <- notas$CALIFIC.NUM[i] * notas$TOTAL.CALIF[i]
}
```



## PRIMER ANALISIS: NOTAS POR GRADOS

Comparamos datos de los diferentes grados: Ing. Informática, Ing. de Computadores, Sistemas de Información.


### Preparacion de los datos
> 1. Eliminar columnas superfluas
2. Agrupar por PLAN
3. Añadir el total numérico de la suma de notas

```r

notasGrados <- as.data.frame(notas[, c(1:3, 7, 10, 14)])
nGrad <- aggregate(TOTAL.CALIF ~ CALIFIC.NUM + PLAN + CALIFIC, notasGrados, 
    sum)

nGrad$TOTAL <- nGrad$CALIFIC.NUM
for (i in 1:length(nGrad$CALIFIC)) {
    nGrad$TOTAL[i] <- nGrad$CALIFIC.NUM[i] * nGrad$TOTAL.CALIF[i]
}
```


### Gráficas

```r
GradPlot <- qplot(CALIFIC, data = nGrad, weight = TOTAL, geom = "bar", binwidth = 1, 
    fill = PLAN) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("CALIFICACIONES POR GRADOS TODOS LOS AÑOS") + xlab("CALIFICACIONES") + 
    ylab("TOTAL CALIFICACIONES")
GradPlot
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


El número de alumnos de cada grado es muy dispar por lo que vamos a realizar unos cálculos para realizar una comparativa de las medias.

### Cálculos
Calculamos el total de alumnos por grado y calculamos la media numérica de notas por grado 

```r
Total_alum <- aggregate(TOTAL.CALIF ~ PLAN, nGrad, sum)
Total_Nota <- aggregate(TOTAL ~ PLAN, nGrad, sum)
Total_Nota$NAlumnos <- Total_alum$TOTAL.CALIF

for (i in 1:length(Total_Nota$TOTAL)) {
    Total_Nota$Media[i] <- Total_Nota$TOTAL[i]/Total_Nota$NAlumnos[i]
}
Total_Nota
```

```
##   PLAN TOTAL NAlumnos Media
## 1 G58   1913      583 3.281
## 2 G59   1550      479 3.235
## 3 G780  2907      702 4.141
```

Asignando valor 0 a "NO PRESENTADO" no hay una gran diferencia. Los valores de las medias en ese caso son:
Media del Grado Sistemas de Información (G58) = 2.948542
Media del Grado Ing. de Computadores (G59) = 2.917537
Media del Grado de Ing.Informática (G780) = 3.913105

### Gráficas

```r
qplot(PLAN, data = Total_Nota, weight = Media, geom = "bar", binwidth = 1, fill = PLAN) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Media total de Notas en los diferentes Grados") + 
    xlab("Grados") + ylab("media de notas")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 





### Conclusiones
Se aprecia que las notas de los alumnos del Grado Sistemas de Información (G58) y las del Grado Ing. de Computadores (G59) son muy similares y claramente inferiores a las de los alumnos del Grado de Ing. Informática (G780).


## SEGUNDO ANALISIS: NOTAS DE LOS DIFERENTES GRUPOS EN ENERO(2013-2014)

Comparamos solo datos del curso 13-14. Por Plan, Por Profesor y Por uso de Videos.

### Preparacion de los datos
> 1. Seleccionar las notas del año 13-14
3. Agrupar por plan, profesor, uso de videos
4. Añadir las medias por plan, profesor, uso de videos

```r

s <- split(notas, notas$AÑO == "2013-14")
n1314 <- s$"TRUE"

# Agrupar por USOVIDEOS AÑO 13-14
CalificVideo <- aggregate(TOTAL ~ CALIFIC + USOVIDEOS, n1314, sum)
CalificVideo
```

```
##          CALIFIC USOVIDEOS TOTAL
## 1       Aprobado         0 253.0
## 2   Matríc.Honor         0  10.0
## 3   NoPresentado         0   6.0
## 4        Notable         0 240.0
## 5  Sobresaliente         0  27.0
## 6       Suspenso         0 142.5
## 7       Aprobado         1  82.5
## 8   Matríc.Honor         1  30.0
## 9   NoPresentado         1   7.0
## 10       Notable         1 195.0
## 11 Sobresaliente         1  18.0
## 12      Suspenso         1 110.0
```

```r

alum_video <- aggregate(TOTAL.CALIF ~ USOVIDEOS, n1314, sum)
nVideo <- aggregate(TOTAL ~ USOVIDEOS, n1314, sum)
nVideo$NAlumnos <- alum_video$TOTAL.CALIF
for (i in 1:length(nVideo$TOTAL)) {
    nVideo$Media[i] <- nVideo$TOTAL[i]/nVideo$NAlumnos[i]
}
nVideo$USOVIDEOS <- as.factor(nVideo$USOVIDEOS)
nVideo
```

```
##   USOVIDEOS TOTAL NAlumnos Media
## 1         0 678.5      145 4.679
## 2         1 442.5       97 4.562
```

```r


# Agrupar por PROFESOR AÑO 13-14
CalificProf <- aggregate(TOTAL ~ CALIFIC + PROFESOR, n1314, sum)
CalificProf
```

```
##          CALIFIC PROFESOR TOTAL
## 1       Aprobado        1 170.5
## 2   Matríc.Honor        1  30.0
## 3   NoPresentado        1   7.0
## 4        Notable        1 195.0
## 5  Sobresaliente        1  18.0
## 6       Suspenso        1 110.0
## 7       Aprobado        2  44.0
## 8   Matríc.Honor        2  10.0
## 9   NoPresentado        2   6.0
## 10       Notable        2 120.0
## 11      Suspenso        2  50.0
## 12      Aprobado        3 121.0
## 13       Notable        3 120.0
## 14 Sobresaliente        3  27.0
## 15      Suspenso        3  92.5
```

```r

alum_prof <- aggregate(TOTAL.CALIF ~ PROFESOR, n1314, sum)
nProf <- aggregate(TOTAL ~ PROFESOR, n1314, sum)
nProf$NAlumnos <- alum_prof$TOTAL.CALIF
for (i in 1:length(nProf$TOTAL)) {
    nProf$Media[i] <- nProf$TOTAL[i]/nProf$NAlumnos[i]
}
nProf$PROFESOR <- as.factor(nProf$PROFESOR)
nProf
```

```
##   PROFESOR TOTAL NAlumnos Media
## 1        1 530.5      113 4.695
## 2        2 230.0       51 4.510
## 3        3 360.5       78 4.622
```

```r


# Agrupar por GRADO AÑO 13-14
CalificPlan <- aggregate(TOTAL ~ CALIFIC + PLAN, n1314, sum)
CalificPlan
```

```
##          CALIFIC PLAN TOTAL
## 1       Aprobado G58  143.0
## 2   NoPresentado G58    5.0
## 3        Notable G58  150.0
## 4       Suspenso G58   65.0
## 5       Aprobado G59   66.0
## 6   Matríc.Honor G59   40.0
## 7   NoPresentado G59    5.0
## 8        Notable G59  150.0
## 9  Sobresaliente G59   18.0
## 10      Suspenso G59   92.5
## 11      Aprobado G780 126.5
## 12  NoPresentado G780   3.0
## 13       Notable G780 135.0
## 14 Sobresaliente G780  27.0
## 15      Suspenso G780  95.0
```

```r

alum_plan <- aggregate(TOTAL.CALIF ~ PLAN, n1314, sum)
nPlan <- aggregate(TOTAL ~ PLAN, n1314, sum)
nPlan$NAlumnos <- alum_plan$TOTAL.CALIF
for (i in 1:length(nPlan$TOTAL)) {
    nPlan$Media[i] <- nPlan$TOTAL[i]/nPlan$NAlumnos[i]
}
nPlan
```

```
##   PLAN TOTAL NAlumnos Media
## 1 G58  363.0       77 4.714
## 2 G59  371.5       80 4.644
## 3 G780 386.5       85 4.547
```


### Gráficas

```r
videoPlot <- qplot(USOVIDEOS, data = nVideo, weight = Media, geom = "bar", binwidth = 1, 
    fill = USOVIDEOS) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("CALIFICACIONES SEGUN USO VIDEO CURSO 13-14") + xlab("Uso Video (0=No, 1=Si)") + 
    ylab("Media de Notas")

ProfPlot <- qplot(PROFESOR, data = nProf, weight = Media, geom = "bar", binwidth = 1, 
    fill = PROFESOR) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("CALIFICACIONES SEGUN PROFESOR CURSO 13-14") + xlab("Profesor") + 
    ylab("Media de Notas")

PlanPlot <- qplot(PLAN, data = nPlan, weight = Media, geom = "bar", binwidth = 1, 
    fill = PLAN) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("CALIFICACIONES SEGUN GRADOS CURSO 13-14") + xlab("Plan") + ylab("Media de Notas")

grid.arrange(videoPlot, ProfPlot, PlanPlot, as.table = FALSE, main = "Notas Curso 13-14", 
    ncol = 1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 






### Conclusiones
Los resultados no muestran una gran diferencia entre los diferentes grupos, la razón principal es que los grupos en los que se han utilizado videos y técnicas de aprendizaje activo, que a su vez son alumnos del profesor 1, son grupos que tradicionalmente sacan peores resultados (como se puede apreciar en el primer análisis): Alumnos del grado de Sistemas de Información y los grupos que se incorporan tarde tras la selectividad de septiembre (Sistemas de Información e Ing. de Computadores).

## TERCER ANALISIS: COMPARATIVA DE NOTAS DEL GRADO SISTEMAS DE INFORMACIÓN EN LOS DIFERENTES AÑOS 

Comparamos ahora solo las notas del Grado en Sistemas de Información, grupo que solamente ha sido impartido la profesora 1 en los diferentes años:
2009-2010, 2010-2011, 2011-2012, 2012-2013 y 2013-2014 (solo en este último curso se ha utilizado el video)

### Preparacion de los datos
> 1. Eliminar columnas no útiles
2. Seleccionar datos del GRADO EN SISTEMAS DE INFORMACIÓN y Profesor 1 y solo de ENERO (ya que no se disponen de los datos de junio Año 13-14)
3. Añadir columna TOTAL (nº de alumnos que sacan una calificación por el valor de dicha calificación)

```r

notasP1 <- as.data.frame(notas[, c(1, 2, 4, 7, 10:14)])

for (i in 1:length(notasP1$CALIFIC)) {
    notasP1$NOM.PLAN[i] <- as.character(notasP1$NOM.PLAN[i])
}

s <- split(notasP1, gsub(" ", "", notasP1$NOM.PLAN) == "GRADOENSISTEMASDEINFORMACIÓN" & 
    notasP1$CONVOCATORIA == "ENE" & notasP1$PROFESOR == 1)
nP1 <- s$"TRUE"
nP1$TOTAL <- nP1$CALIFIC.NUM
for (i in 1:length(nP1$CALIFIC)) {
    nP1$TOTAL[i] <- nP1$CALIFIC.NUM[i] * nP1$TOTAL.CALIF[i]
}
```


### Cálculos
Calculamos el total de alumnos por año y calculamos la media numérica de notas por Año 

```r
Total_alum_sis <- aggregate(TOTAL.CALIF ~ AÑO, nP1, sum)
Total_Nota_sis <- aggregate(TOTAL ~ AÑO, nP1, sum)
Total_Nota_sis$NAlumnos <- Total_alum_sis$TOTAL.CALIF

for (i in 1:length(Total_Nota_sis$TOTAL)) {
    Total_Nota_sis$Media[i] <- Total_Nota_sis$TOTAL[i]/Total_Nota_sis$NAlumnos[i]
}
Total_Nota_sis
```

```
##       AÑO TOTAL NAlumnos Media
## 1 2009-10 229.0       63 3.635
## 2 2010-11 309.5       86 3.599
## 3 2011-12 312.5       81 3.858
## 4 2012-13 350.0       92 3.804
## 5 2013-14 355.5       76 4.678
```


### Gráficas

```r
qplot(AÑO, data = Total_Nota_sis, weight = Media, geom = "bar", binwidth = 1, 
    fill = AÑO) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("Media total de Notas de Sistemas en de los diferentes Años") + 
    xlab("Años") + ylab("media de notas")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

En la gráfica anterior se aprecia una clara mejoría de las notas del grupo de Sistemas de Informática en el último año, en el cual se han empleado videos docentes y técnicas de aprendizaje activo.




### Calificaciones por año
> 1. Agrupamos por año
2. Calculamos los porcentajes por calificaciones de cada año 

```r
s <- split(nP1, nP1$AÑO == "2009-10")
notas0910 <- s$"TRUE"

s <- split(nP1, nP1$AÑO == "2010-11")
notas1011 <- s$"TRUE"

s <- split(nP1, nP1$AÑO == "2011-12")
notas1112 <- s$"TRUE"

s <- split(nP1, nP1$AÑO == "2012-13")
notas1213 <- s$"TRUE"


s <- split(nP1, nP1$AÑO == "2013-14")
notas1314 <- s$"TRUE"
notas1314$CALIFIC <- as.factor(notas1314$CALIFIC)

# En el año 13-14 hay 3 grupos por lo que hay que agrupar por calific de
# nuevo
notas1314 <- aggregate(TOTAL.CALIF ~ CALIFIC, notas1314, sum)
notas1314$AÑO <- "2013-14"
```

# Calculamos los porcentajes de calificaciones por año

```r

calculaPorcentaje <- function(d) {
    
    for (i in 1:length(d$TOTAL)) d$Porcentaje[i] <- d$TOTAL.CALIF[i] * 100/sum(d$TOTAL.CALIF)
    return(d$Porcentaje)
}
notas0910$Porcentaje <- calculaPorcentaje(d <- as.data.frame(notas0910))
notas1011$Porcentaje <- calculaPorcentaje(d <- as.data.frame(notas1011))
notas1112$Porcentaje <- calculaPorcentaje(d <- as.data.frame(notas1112))
notas1213$Porcentaje <- calculaPorcentaje(d <- as.data.frame(notas1213))
notas1314$Porcentaje <- calculaPorcentaje(d <- as.data.frame(notas1314))

n0910porcent <- as.data.frame(notas0910[, c(1, 4, 11)])
n1011porcent <- as.data.frame(notas1011[, c(1, 4, 11)])
n1112porcent <- as.data.frame(notas1112[, c(1, 4, 11)])
n1213porcent <- as.data.frame(notas1213[, c(1, 4, 11)])
n1314porcent <- as.data.frame(notas1314[, c(3, 1, 4)])

n0910porcent
```

```
##       AÑO      CALIFIC Porcentaje
## 1 2009-10     Aprobado      34.92
## 2 2009-10 NoPresentado      36.51
## 3 2009-10      Notable      12.70
## 4 2009-10     Suspenso      15.87
```

```r
n1011porcent
```

```
##        AÑO       CALIFIC Porcentaje
## 17 2010-11      Aprobado     24.419
## 18 2010-11  NoPresentado     34.884
## 19 2010-11       Notable     16.279
## 20 2010-11 Sobresaliente      1.163
## 21 2010-11      Suspenso     23.256
```

```r
n1112porcent
```

```
##        AÑO       CALIFIC Porcentaje
## 45 2011-12      Aprobado     22.222
## 46 2011-12  NoPresentado      8.642
## 47 2011-12       Notable     14.815
## 48 2011-12 Sobresaliente      1.235
## 49 2011-12      Suspenso     53.086
```

```r
n1213porcent
```

```
##        AÑO      CALIFIC Porcentaje
## 72 2012-13     Aprobado      27.17
## 73 2012-13 NoPresentado      10.87
## 74 2012-13      Notable      13.04
## 75 2012-13     Suspenso      48.91
```

```r
n1314porcent
```

```
##       AÑO      CALIFIC Porcentaje
## 1 2013-14     Aprobado     34.211
## 2 2013-14 NoPresentado      6.579
## 3 2013-14      Notable     25.000
## 4 2013-14     Suspenso     34.211
```



### Gráficas

```r
dibuja <- function(d, title) {
    qplot(CALIFIC, data = d, weight = Porcentaje, geom = "bar", binwidth = 1) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle(title) + 
        xlab("Calificaciones") + ylab("Porcentajes")
}

notas0910Plot <- dibuja(d <- as.data.frame(notas0910), "CALIFICACIONES 09-10")
notas1011Plot <- dibuja(d <- as.data.frame(notas1011), "CALIFICACIONES 10-11")
notas1112Plot <- dibuja(d <- as.data.frame(notas1112), "CALIFICACIONES 11-12")
notas1213Plot <- dibuja(d <- as.data.frame(notas1213), "CALIFICACIONES 12-13")
notas1314Plot <- dibuja(d <- as.data.frame(notas1314), "CALIFICACIONES 13-14")

grid.arrange(notas0910Plot, notas1011Plot, notas1112Plot, notas1213Plot, notas1314Plot, 
    as.table = FALSE, main = "Porcentajes de calificaciones de Sistemas de Información", 
    ncol = 2)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 


En las gráficas anteriores, el número de Suspensos en los 2 primeros años es inferior al último año pero hay un elevadísimo número de alumnos No presentados, En el curso 13-14 en el cual se han utilizado videos docentes y técnicas de aprendizaje activo, además de reflejarse la mejoría general de notas y una diferencia mayor entre el número de **aprobados** respecto a los **suspensos** y **No presentados**, es importante apreciar que el número de **No presentados** es sustancialmente inferior. 





## CONCLUSIONES
Tras los análisis realizados podemos afirmar que **sí es significativa la mejora que se ha producido en este último año en aquellos grupos en los que se han empleado videos docentes y técnicas de aprendizaje activo**.
