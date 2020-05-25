# Analyze the information of the ToothGrowth data in the R datasets package.
# Then, define an expression to obtain the tooths' mean size with a dosis of 0.5 mg, group by the supplement type (VC or OJ).

datasets::ToothGrowth

# To see dataset description
?ToothGrowth

tapply(ToothGrowth$dose == 0.5, ToothGrowth$supp, mean)


require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

# Create a pie chart to show tooth length values for the experiments
# consisting of providing 1 mg of orange juice.  
data1mg = subset(ToothGrowth, ToothGrowth$dose>=1)
data1mg
pie(data1mg$len, main = "Toohth size from 1mg", cex.lab=0.2)


# Define an expression to create a pie chart which shows the proportion of tooth sizes smaller than 10, between 10 and 20, and bigger than 20. 

# These two expressions are just to know the smallest and biggest tooth size.
max(ToothGrowth$len) # Result: 33.9
min(ToothGrowth$len) # Result:  4.2
intOne  = ToothGrowth[ToothGrowth[,1] < 10, ]
intOne
intTwo  = ToothGrowth[ToothGrowth[,1] >= 10 & ToothGrowth[,1] < 20, ]
intTwo
intThree = ToothGrowth[ToothGrowth[,1] >= 20, ]
intThree

dataForChart = c(length(intOne$len), length(intTwo$len), length(intThree$len))
names(dataForChart) = c("<10","10-20",">20")
pie(dataForChart, main = "Tooth lengths considering three value intervals")


#_________________________
# The dataset PlantGrowth contains the daily plant growth for 3 treatments repeated on 10 years.
# First, analyze its information.
datasets::PlantGrowth
head(PlantGrowth)
# weight group
# 1   4.17  ctrl
# 2   5.58  ctrl
# 3   5.18  ctrl
# 4   6.11  ctrl
# 5   4.50  ctrl
# 6   4.61  ctrl
?

# a. Define an expression for calculating the weight mean of each group using tapply().
cleanData=na.omit(PlantGrowth) # For omitting null values.
WeightMean=tapply(cleanData$weight, cleanData$group, mean)
WeightMean

# b. Represent the information from item 'a' with a pie chart.
names(WeightMean)=c("Control","Traitement1","Traitement2")
PieColor=c(9,450,503)
pie(WeightMean,col=PieColor, main="Mean Weight by group")

######################################
# Ejercicio 8 ########################
######################################
# Ejercicio 8. "iris" es un conjunto de datos de R, que contiene distintas características de las plantas
# pertenecientes a la familia con ese nombre. A partir de los datos almacenados el en dataset "iris"
# escribir expresiones para:

datasets::iris
head(iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa
?iris


# a. Generar un objeto llamado longPetalos con los promedios de
# longitud de pétalo (Petal.Width) agrupados por especie (Species)
# (usar tapply()). Debe quedar así:  <un gráfito>
nuevo = na.omit(iris) # no elimina ningunra porque no es necesario
longPetalos=tapply(nuevo$Petal.Width,nuevo$Species,mean)
longPetalos
# > longPetalos
# setosa versicolor  virginica 
# 1.462      4.260      5.552


# b. Generar un gráfico de torta como el que se muestra a continuación
# representando los valores almacenados en el objeto longPetalos. 
colores=c(9,450,503)
names(longPetalos)=c("setosa","versicolor","virginica")
pie(longPetalos,col=colores,main="Longitud promedio de pétalos por especie")



######################################
# Ejercicio 9 ########################
######################################
# Ejercicio 9. Cargue el dataset airquality y analice la información contenida en el mismo. Luego escriba
# expresiones para:
datasets::airquality
head(airquality)
# Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6

?airquality
# A data frame with 153 observations on 6 variables.
# [,1]	Ozone	numeric	Ozone (ppb)
# [,2]	Solar.R	numeric	Solar R (lang)
# [,3]	Wind	numeric	Wind (mph)
# [,4]	Temp	numeric	Temperature (degrees F)
# [,5]	Month	numeric	Month (1--12)
# [,6]	Day	numeric	Day of month (1--31)

# a. Calcular cuantas veces la temperatura fue mayor a 75 grados Farenheit.
tempMayores75=airquality[,4] > 75
cantTempMayores75 = sum (tempMayores75)
cantTempMayores75

# b. Generar un subconjunto con los datos correspondientes al mes de junio.
datosJunio = subset(airquality, airquality[,5]==6)
datosJunio

# c. Generar un subconjunto con los datos correspondientes a los primeros 15 días del mes de
# agosto.
datosAgosto15dias = subset(airquality, airquality[,5]==8)[1:15,]
datosAgosto15dias

# d. Obtener el promedio de temperaturas agrupado por mes (usar tapply()).
promTemp = tapply(airquality$Temp, airquality$Month, mean)
promTemp

# e. Ilustrar la frecuencia de valores de temperaturas en un histograma como se muestra a
# continuación.
coloresHist = c(9,450,503)
hist(airquality[,4], freq = T, border = "black", xlim = c(55,100),
     xlab = "Temperatura", ylab = "Frecuencia", cex.lab=0.75,
     main = "Frecuencia de temperaturas", cex.main=1.5, col = coloresHist)


# Opcional: Buscar en la web si existe en R una función para convertir de grados Farenheit a
# grados Celsius.
# En caso de encontrarla, genere el gráfico con la temperatura en grados Celsius.

# https://www.stat.berkeley.edu/~s133/R-1a.html
#       C = 5/9 (F - 32)
deFahrACels = function(x){return((5 / 9) * (x - 32))}

hist(deFahrACels(airquality[,4]), freq = T, border = "black", xlim = c(10,40),
     xlab = "Temperatura", ylab = "Frecuencia", cex.lab=0.75,
     main = "Frecuencia de temperaturas", cex.main=1.5, col = coloresHist)
