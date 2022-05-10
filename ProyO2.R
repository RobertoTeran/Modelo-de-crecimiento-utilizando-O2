

library(gapminder)
library(ggplot2)
library(psych)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)


choose.files()
RT <- read_excel("RT.xlsx")
as.data.frame(RT)



###histograma de las variables###

#histograma para sgr#

hist(x = RT$sgr, main = "Histograma SGR", 
     xlab = "SGR", ylab = "Frecuencia",
     col = "magenta")

hist(x = RT$o2, main = "Histograma O2", 
     xlab = "O2", ylab = "Frecuencia",
     col = "cyan")

hist(x = RT$temp, main = "Histograma T?", 
     xlab = "T?", ylab = "Frecuencia",
     col = "blue")

head(RT)
summary(RT)

##pruebas de normalidad e independencia de los datos##

plot(density(RT$sgr))
plot(density(RT$o2))
plot(density(RT$temp))

plot(ecdf(RT$sgr))
plot(ecdf(RT$o2))
plot(ecdf(RT$temp))


#interaccion entre los datos##

interaction.plot(RT$temp, RT$sgr, RT$o2)



#correlacion###


pairs.panels(RT[,2:4], method = "pearson", hist.col = "magenta",  density = TRUE, font=2)
corPlot(RT, cex = 1.2, main = "Matriz de correlaci?n")

#buscamos relacion grafica###

ggplot(RT, aes(x=sgr, y=o2)) + 
  geom_point() + theme_light()


#obtendremos las estimaciones de los parametros estadisticos##

mod1 <- lm(sgr ~ o2, data=RT)
mod1 # Para imprimir el objeto mod1

#En la salida anterior se observan los valores estimados de  ??0 y  ??1
#pero no aparece la estimaci?n de  ??
#Para obtener una tabla de resumen con detalles del modelo ajustado,
#se usa la funci?n gen?rica summary

summary(mod1)


#Para incluir la recta de regresi?n que representa 
#el modelo ajustado anterior...

ggplot(RT, aes(x=sgr, y=o2)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light()



##la regresion lineal no se ajusta de buena manera a la nube  de datos#

#utilizaremos regresion multiple##

library(scatterplot3d)
attach(RT)
scatterplot3d(x=temp, y=o2, z=sgr, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='Temp (C?)',
              ylab='O2 (mg/l)', zlab='SGR')


#a medida que aumenta la temperatura, aumenta el SGR y en condiciones de mayor o2

library(plotly)
plot_ly(x=temp, y=o2, z=sgr, type="scatter3d", color=sgr) %>% 
  layout(scene = list(xaxis = list(title = 'Temp (C?)'),
                      yaxis = list(title = 'o2 (mg/l)'),
                      zaxis = list(title = 'SGR')))



#basandonos en el modelo 3d, la expresion que se ajusta es:

mod <- lm(sgr ~ o2 + temp, data=RT)
summary(mod)

#Para incluir el plano de regresi?n que representa el modelo ajustado anterior 

# Se crea el grafico 3d y se guarda en un objeto, por ejemplo mi_3d

mi_3d <- scatterplot3d(x=temp, y=o2, z=sgr, pch=16, cex.lab=1,
                       highlight.3d=TRUE, type="h", xlab='Temp (C?)',
                       ylab='O2 (mg/l)', zlab='SGR')



# -Para agregar el plano usamos $plane3d( ) con argumento modelo ajustado
mi_3d$plane3d(mod, lty.box="solid", col='mediumblue')
