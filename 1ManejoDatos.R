#########################################
#                                       #
#     ¿Cómo se siente el presidente?    # 
#                                       # 
#          Pacheco y Riquelme           #  
#                                       #
#         1. Manejo de datos            #
#########################################

# Ante cualquier error de código o duda, comunicarse con ariquelme@udesa.edu.ar

# Definimos la paleta de colores que vamos a usar.

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#FF7F32", "#edf71c", "#941cf7")

colores2 <- c("#f5fa7b", "#3cffb7", "#ffdaaa", "#fa66f3",
              "#f84a42","#9e9d9c", "#c3f842", "#3cff56")

options(scipen=999)

# Abrimos la base de datos bajándola del repositorio de Github.

data <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/data_csep.csv")

# Le damos el formato deseado.

data <- data[,-1]
rownames(data) <- data$time

# Construimos las nuevas variables.

# Las muertes en Argentina respecto de las muertes en el resto del mundo. 

data$muertes.arg.rel <- data$muertosarg/data$muertesmundo

# Ponemos cero donde hay NA (porque no había pandemia).

data$muertes.arg.rel <-ifelse(is.na(data$muertes.arg.rel),0,data$muertes.arg.rel)

# Los casos en Argentina respecto de los casos en el resto del mundo.

data$casos.arg.rel <- data$casosarg/data$casosmundo

# Ponemos cero donde hay NA.

data$casos.arg.rel<-ifelse(is.na(data$casos.arg.rel),0,data$casos.arg.rel)

# Borramos las variables 'nominales' del resto del mundo, el sentimiento de Alberto 
# Fernández sin el suavizado.

data <- data[, -c(2,15,16,17,18)]

# Ahora generamos un objeto de series de tiempo para cada una de las variables.

library(xts)

for (i in colnames(data)[-(1)]){
  assign(i, xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d")) )
}

# Graficamos nuestra serie de interés.

library(ggplot2)
library(ggfortify)
library(forecast)

autoplot(sentsmooth, ts.colour = colores[1]) + 
  ggtitle("Evolución del sentimiento de Alberto Fernández") + 
  xlab("Tiempo") + 
  ylab("Sentimiento") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Tiempo", 
       y = "Sentimiento", 
       title = "Evolución del sentimiento de Alberto Fernández",
       caption = "Fuente: elaboración propia")

ggsave(file="Sentimiento_AF.eps", width=6.5, height=4, dpi=300)

# Graficamos las demás variables que consideramos relevantes. 

autoplot(twfav, ts.colour = colores[2]) + 
  ggtitle("Evolución de los favoritos que recibe AF en Twitter") + 
  xlab("Tiempo") + 
  ylab("Cantidad de favoritos") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia") 

ggsave(file="FAVs.eps", width=6.5, height=4, dpi=300)

autoplot(twret, ts.colour = colores[3]) + 
  ggtitle("Evolución de los retweets que recibe AF en Twitter") + 
  xlab("Tiempo") + 
  ylab("Cantidad de retweets") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia") 

ggsave(file="RTs.eps", width=6.5, height=4, dpi=300)

autoplot(reservasbcra, ts.colour = colores2[7]) + 
  ggtitle("Evolución de las reservas del BCRA") + 
  xlab("Tiempo") + 
  ylab("Miles de millones de dólares") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del BCRA") 

ggsave(file="ReservasBCRA.eps", width=6.5, height=4, dpi=300)

autoplot(tasaint, ts.colour = colores[4]) + 
  ggtitle("Evolución de la tasa de interés") + 
  xlab("Tiempo") + 
  ylab("Tasa") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del BCRA") 

ggsave(file="TasaInt.eps", width=6.5, height=4, dpi=300)

autoplot(basemon, ts.colour = colores[5]) + 
  ggtitle("Evolución de la base monetaria") + 
  xlab("Tiempo") + 
  ylab("Miles de millones de pesos") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del BCRA") 

ggsave(file="BaseMon.eps", width=6.5, height=4, dpi=300)

autoplot(tcdolar, ts.colour = colores2[8]) + 
  ggtitle("Evolución del tipo de cambio") + 
  xlab("Tiempo") + 
  ylab("Pesos por dólar") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos de Yahoo Finance")

ggsave(file="Dolar.eps", width=6.5, height=4, dpi=300)

autoplot(casosarg[51:483], ts.colour = colores[6]) + 
  ggtitle("Evolución de los de casos de COVID-19 en Argentina") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud") 

ggsave(file="CasosArg.eps", width=6.5, height=4, dpi=300)

autoplot(muertosarg[51:483], ts.colour = colores2[1]) + 
  ggtitle("Evolución de las de muertes por COVID-19 en Argentina") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud") 

ggsave(file="MuertesArg.eps", width=6.5, height=4, dpi=300)

autoplot(vacunasarg[51:483], ts.colour = colores2[2]) + 
  ggtitle("Evolución de la cantidad de vacunas aplicadas en Argentina") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud") 

ggsave(file="Vacunas.eps", width=6.5, height=4, dpi=300)

autoplot(maxtemp, ts.colour = colores2[3]) + 
  ggtitle("Temperatura máxima en Buenos Aires") + 
  xlab("Tiempo") + 
  ylab("Grados centígrados") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos de World Weather Online") 

ggsave(file="TempMax.eps", width=6.5, height=4, dpi=300)

autoplot(mintemp, ts.colour = colores2[4]) + 
  ggtitle("Temperatura mínima en Buenos Aires") + 
  xlab("Tiempo") + 
  ylab("Grados centígrados") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos de World Weather Online")

ggsave(file="TempMin.eps", width=6.5, height=4, dpi=300)

autoplot(muertes.arg.rel[51:483], ts.colour = colores2[5]) + 
  ggtitle("Muertes por COVID-19 en Arg. relativas al resto del mundo") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud y Our World in Data") 

ggsave(file="MuertesRel.eps", width=6.5, height=4, dpi=300)

autoplot(casos.arg.rel[51:483], ts.colour = colores2[6]) + 
  ggtitle("Casos de COVID-19 en Arg. relativos al resto del mundo)")+ 
  xlab("Tiempo")+ 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud y Our World in Data") 

ggsave(file="CasosRel.eps", width=6.5, height=4, dpi=300)


# Creamos una función que devuelve el estadístico con su significatividad. 

stars <- function(estadistico, criticalvector){
  if (estadistico < criticalvector[1]){
    return(paste(round(estadistico,2), "***", sep = "")) 
  }else{
    if (estadistico < criticalvector[2]){
      return(paste(round(estadistico,2), "**", sep = ""))
    }else{
      if (estadistico < criticalvector[3]){
        return(paste(round(estadistico,2), "*", sep = ""))
      }else{
        return(paste(round(estadistico,2), "", sep = ""))
      }
    }
  }
}

stars2 <- function(estadistico, criticalvector){
  if (estadistico > criticalvector[1]){
    return(paste(round(estadistico,2), "***", sep = "")) 
  }else{
    if (estadistico > criticalvector[2]){
      return(paste(round(estadistico,2), "**", sep = ""))
    }else{
      if (estadistico > criticalvector[3]){
        return(paste(round(estadistico,2), "*", sep = ""))
      }else{
        return(paste(round(estadistico,2), "", sep = ""))
      }
    }
  }
}

# Hacemos un loop para calcular los test de raíz unitaria para cada una de las variables.

library(urca)
library(stargazer)

results <- matrix(nrow = 16,ncol = 4, NA)
r <- 1
for (i in colnames(data)[-(1)]){
  results[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.df(series, type = c("none"))
  results[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("trend"))
  results[r,3] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("drift"))
  results[r,4] <- stars(test@teststat[1], test@cval[1,])
  r <- r+1
}
colnames(results) <- c("Variable", "None","Trend","Drift")

# Agregamos a mano aquellas variables que tenemos que diferenciar.
r <- 15
for (i in c("reservasbcra", "tcdolar")){
  results[r,1] <- paste("L1", i, sep = " ")
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- diff(series)
  series <- na.omit(series)
  test <- ur.df(series, type = c("none"))
  results[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("trend"))
  results[r,3] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("drift"))
  results[r,4] <- stars(test@teststat[1], test@cval[1,])
  r <- r+1
}
# Exportamos la tabla.

stargazer(results , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

# Hacemos el test de Phillips-Perron.

results1 <- matrix(nrow = 16,ncol = 3, NA)
r <- 1
for (i in colnames(data)[-(1)]){
  results1[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.pp(series, type = c("Z-tau"), model=c("constant"), lags=c("long"))
  results1[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.pp(series, type = c("Z-tau"), model=c("trend"), lags=c("long"))
  results1[r,3] <- stars(test@teststat[1], test@cval[1,])
  r <- r+1
}
colnames(results1) <- c("Variable", "Constant", "Trend")

# Diferenciamos y agregamos a nuestra tabla.

r <- 15
for (i in c("reservasbcra", "tcdolar")){
  results1[r,1] <- paste("L1", i, sep = " ")
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- diff(series)
  series <- na.omit(series)
  test <- ur.pp(series, type = c("Z-tau"), model=c("constant"), lags=c("long"))
  results1[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.pp(series, type = c("Z-tau"), model=c("trend"), lags=c("long"))
  results1[r,3] <- stars(test@teststat[1], test@cval[1,])
  r <- r+1
}
# Exportamos la tabla.

stargazer(results1 , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

