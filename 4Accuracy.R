#########################################
#                                       #
#     ¿Cómo se siente el presidente?    # 
#                                       # 
#          Pacheco y Riquelme           #  
#                                       #
#            4. Accuracy                #
#########################################

# Ante cualquier error de código o duda, comunicarse con ariquelme@udesa.edu.ar

# Hacemos matrices en las cuales vamos a guardar estas medidas de desempeño, 
# acompañadas con el test de Diebold-Mariano.

# Usamos como benchmark un AR(1).

# Para esto generamos los pronósticos que resultan al utilizar el modelo AR(1).

pr.bench <- ts(matrix(nrow = 58, ncol = 1, 0), frequency = 365, start=c(2020,11))

for (i in 1:58) {
  temp <- window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  f2 <- arima(temp, order=c(1,0,0))
  forecast2 <- forecast(f2,h=1)
  pr.bench[i,1] <- forecast2$mean[1]
}

# Separamos las tablas en función de los pasos adelante de los pronósticos.

AC<-matrix(NA,34,6)
colnames(AC) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC2<-matrix(NA,34,1)

# TABLA 1

# Medidas de accuracy y test de DM de los pronósticos con esquema fijo y h=1.

AC[1,1]<-"AR(1)" 
AC2[1,1]<-"1"
AC[1,2:4]<-round(accuracy(pr.bench, out.of.sample[,2])[c(2:3,5)],4)
AC[2,1]<-"ARIMA fijo" 
AC2[2,1]<-"1"
AC[2,2:4]<-round(accuracy(pr.f.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[3,1]<-"ARIMAX fijo" 
AC2[3,1]<-"1"
AC[3,2:4]<-round(accuracy(pr.f.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[4,1]<-"ETS fijo" 
AC2[4,1]<-"1"
AC[4,2:4]<-round(accuracy(pr.f.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[5,1]<- "ADL fijo" 
AC2[5,1]<-"1"
AC[5,2:4]<-round(accuracy(pr.f.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[6,1]<- "VAR fijo"
AC2[6,1]<-"1"
AC[6,2:4]<-round(accuracy(pr.f.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[7,1]<- "FAVAR fijo" 
AC2[7,1]<-"1"
AC[7,2:4]<-round(accuracy(pr.f.h1[,6], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=1.

AC[8,1]<-"ARIMA recursivo" 
AC2[8,1]<-"1"
AC[8,2:4]<-round(accuracy(pr.rec.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[9,1]<-"ARIMAX recursivo" 
AC2[9,1]<-"1"
AC[9,2:4]<-round(accuracy(pr.rec.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[10,1]<-"ETS recursivo" 
AC2[10,1]<-"1"
AC[10,2:4]<-round(accuracy(pr.rec.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[11,1]<- "ADL recursivo" 
AC2[11,1]<-"1"
AC[11,2:4]<-round(accuracy(pr.rec.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[12,1]<- "VAR recursivo"
AC2[12,1]<-"1"
AC[12,2:4]<-round(accuracy(pr.rec.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[13,1]<- "FAVAR recursivo" 
AC2[13,1]<-"1"
AC[13,2:4]<-round(accuracy(pr.rec.h1[,6], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=1.

AC[14,1]<-"ARIMA rolling" 
AC2[14,1]<-"1"
AC[14,2:4]<-round(accuracy(pr.rol.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[15,1]<-"ARIMAX rolling" 
AC2[15,1]<-"1"
AC[15,2:4]<-round(accuracy(pr.rol.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[16,1]<-"ETS rolling" 
AC2[16,1]<-"1"
AC[16,2:4]<-round(accuracy(pr.rol.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[17,1]<- "ADL rolling" 
AC2[17,1]<-"1"
AC[17,2:4]<-round(accuracy(pr.rol.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[18,1]<- "VAR rolling"
AC2[18,1]<-"1"
AC[18,2:4]<-round(accuracy(pr.rol.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[19,1]<- "FAVAR rolling" 
AC2[19,1]<-"1"
AC[19,2:4]<-round(accuracy(pr.rol.h1[,6], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 1 y series boots.

AC[20,1]<-"ARIMA fijo bagged" 
AC2[20,1]<-"1"
AC[20,2:4]<-round(accuracy(pr.f.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[21,1]<-"ARIMAX fijo bagged" 
AC2[21,1]<-"1"
AC[21,2:4]<-round(accuracy(pr.f.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[22,1]<-"ETS fijo bagged" 
AC2[22,1]<-"1"
AC[22,2:4]<-round(accuracy(pr.f.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[23,1]<- "VAR fijo bagged" 
AC2[23,1]<-"1"
AC[23,2:4]<-round(accuracy(pr.f.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[24,1]<- "FAVAR fijo bagged"
AC2[24,1]<-"1"
AC[24,2:4]<-round(accuracy(pr.rec.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC[25,1]<-"ARIMA recursivo bagged" 
AC2[25,1]<-"1"
AC[25,2:4]<-round(accuracy(pr.rec.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[26,1]<-"ARIMAX recursivo bagged" 
AC2[26,1]<-"1"
AC[26,2:4]<-round(accuracy(pr.rec.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[27,1]<-"ETS recursivo bagged" 
AC2[27,1]<-"1"
AC[27,2:4]<-round(accuracy(pr.rec.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[28,1]<- "VAR recursivo bagged" 
AC2[28,1]<-"1"
AC[28,2:4]<-round(accuracy(pr.rec.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[29,1]<- "FAVAR recursivo bagged"
AC2[29,1]<-"1"
AC[29,2:4]<-round(accuracy(pr.rec.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 1 y series boots.

AC[30,1]<-"ARIMA rolling bagged" 
AC2[30,1]<-"1"
AC[30,2:4]<-round(accuracy(pr.rol.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[31,1]<-"ARIMAX rolling bagged" 
AC2[31,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[32,1]<-"ETS rollingbagged" 
AC2[32,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[33,1]<- "VAR rolling bagged" 
AC2[33,1]<-"1"
AC[33,2:4]<-round(accuracy(pr.rol.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[34,1]<- "FAVAR rolling bagged"
AC2[34,1]<-"1"
AC[34,2:4]<-round(accuracy(pr.rol.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)

# TABLA 2 

AC1<-matrix(NA,34,6)
colnames(AC1) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC3<-matrix(NA,34,1)


AC1[2,1]<-"ARIMA fijo" 
AC3[2,1]<-"2"
AC1[2,2:4]<-round(accuracy(pr.f.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[3,1]<-"ARIMAX fijo" 
AC3[3,1]<-"2"
AC1[3,2:4]<-round(accuracy(pr.f.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[4,1]<-"ETS fijo" 
AC3[4,1]<-"2"
AC1[4,2:4]<-round(accuracy(pr.f.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[5,1]<- "ADL fijo" 
AC3[5,1]<-"2"
AC1[5,2:4]<-round(accuracy(pr.f.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[6,1]<- "VAR fijo"
AC3[6,1]<-"2"
AC1[6,2:4]<-round(accuracy(pr.f.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[7,1]<- "FAVAR fijo" 
AC3[7,1]<-"2"
AC1[7,2:4]<-round(accuracy(pr.f.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=2.

AC1[8,1]<-"ARIMA recursivo" 
AC3[8,1]<-"2"
AC1[8,2:4]<-round(accuracy(pr.rec.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[9,1]<-"ARIMAX recursivo" 
AC3[9,1]<-"2"
AC1[9,2:4]<-round(accuracy(pr.rec.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[10,1]<-"ETS recursivo" 
AC3[10,1]<-"2"
AC1[10,2:4]<-round(accuracy(pr.rec.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[11,1]<- "ADL recursivo" 
AC3[11,1]<-"2"
AC1[11,2:4]<-round(accuracy(pr.rec.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[12,1]<- "VAR recursivo"
AC3[12,1]<-"2"
AC1[12,2:4]<-round(accuracy(pr.rec.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[13,1]<- "FAVAR recursivo" 
AC3[13,1]<-"2"
AC1[13,2:4]<-round(accuracy(pr.rec.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=2.

AC1[14,1]<-"ARIMA rolling" 
AC3[14,1]<-"2"
AC1[14,2:4]<-round(accuracy(pr.rol.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[15,1]<-"ARIMAX rolling" 
AC3[15,1]<-"2"
AC1[15,2:4]<-round(accuracy(pr.rol.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[16,1]<-"ETS rolling" 
AC3[16,1]<-"2"
AC1[16,2:4]<-round(accuracy(pr.rol.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[17,1]<- "ADL rolling" 
AC3[17,1]<-"2"
AC1[17,2:4]<-round(accuracy(pr.rol.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[18,1]<- "VAR rolling"
AC3[18,1]<-"2"
AC1[18,2:4]<-round(accuracy(pr.rol.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[19,1]<- "FAVAR rolling" 
AC3[19,1]<-"2"
AC1[19,2:4]<-round(accuracy(pr.rol.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC1[20,1]<-"ARIMA fijo bagged" 
AC3[20,1]<-"2"
AC1[20,2:4]<-round(accuracy(pr.f.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[21,1]<-"ARIMAX fijo bagged" 
AC3[21,1]<-"2"
AC1[21,2:4]<-round(accuracy(pr.f.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[22,1]<-"ETS fijo bagged" 
AC3[22,1]<-"2"
AC1[22,2:4]<-round(accuracy(pr.f.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[23,1]<- "VAR fijo bagged" 
AC3[23,1]<-"2"
AC1[23,2:4]<-round(accuracy(pr.f.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[24,1]<- "FAVAR fijo bagged"
AC3[24,1]<-"2"
AC1[24,2:4]<-round(accuracy(pr.rec.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC1[25,1]<-"ARIMA recursivo bagged" 
AC3[25,1]<-"2"
AC1[25,2:4]<-round(accuracy(pr.rec.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[26,1]<-"ARIMAX recursivo bagged" 
AC3[26,1]<-"2"
AC1[26,2:4]<-round(accuracy(pr.rec.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[27,1]<-"ETS recursivo bagged" 
AC3[27,1]<-"2"
AC1[27,2:4]<-round(accuracy(pr.rec.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[28,1]<- "VAR recursivo bagged" 
AC3[28,1]<-"2"
AC1[28,2:4]<-round(accuracy(pr.rec.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[29,1]<- "FAVAR recursivo bagged"
AC3[29,1]<-"2"
AC1[29,2:4]<-round(accuracy(pr.rec.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC1[30,1]<-"ARIMA rolling bagged" 
AC3[30,1]<-"2"
AC1[30,2:4]<-round(accuracy(pr.rol.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[31,1]<-"ARIMAX rolling bagged" 
AC3[31,1]<-"2"
AC1[31,2:4]<-round(accuracy(pr.rol.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[32,1]<-"ETS rollingbagged" 
AC3[32,1]<-"2"
AC1[32,2:4]<-round(accuracy(pr.rol.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[33,1]<- "VAR rolling bagged" 
AC3[33,1]<-"2"
AC1[33,2:4]<-round(accuracy(pr.rol.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[34,1]<- "FAVAR rolling bagged"
AC3[34,1]<-"2"
AC1[34,2:4]<-round(accuracy(pr.rol.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# TABLA 3 

AC4<-matrix(NA,34,6)
colnames(AC1) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC5<-matrix(NA,34,1)

AC4[2,1]<-"ARIMA fijo" 
AC5[2,1]<-"2"
AC4[2,2:4]<-round(accuracy(pr.f.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[3,1]<-"ARIMAX fijo" 
AC5[3,1]<-"2"
AC4[3,2:4]<-round(accuracy(pr.f.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[4,1]<-"ETS fijo" 
AC5[4,1]<-"2"
AC4[4,2:4]<-round(accuracy(pr.f.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[5,1]<- "ADL fijo" 
AC5[5,1]<-"2"
AC4[5,2:4]<-round(accuracy(pr.f.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[6,1]<- "VAR fijo"
AC5[6,1]<-"2"
AC4[6,2:4]<-round(accuracy(pr.f.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[7,1]<- "FAVAR fijo" 
AC5[7,1]<-"2"
AC4[7,2:4]<-round(accuracy(pr.f.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=2.

AC4[8,1]<-"ARIMA recursivo" 
AC5[8,1]<-"2"
AC4[8,2:4]<-round(accuracy(pr.rec.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[9,1]<-"ARIMAX recursivo" 
AC5[9,1]<-"2"
AC4[9,2:4]<-round(accuracy(pr.rec.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[10,1]<-"ETS recursivo" 
AC5[10,1]<-"2"
AC4[10,2:4]<-round(accuracy(pr.rec.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[11,1]<- "ADL recursivo" 
AC5[11,1]<-"2"
AC4[11,2:4]<-round(accuracy(pr.rec.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[12,1]<- "VAR recursivo"
AC5[12,1]<-"2"
AC4[12,2:4]<-round(accuracy(pr.rec.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[13,1]<- "FAVAR recursivo" 
AC5[13,1]<-"2"
AC4[13,2:4]<-round(accuracy(pr.rec.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=2.

AC4[14,1]<-"ARIMA rolling" 
AC5[14,1]<-"2"
AC4[14,2:4]<-round(accuracy(pr.rol.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[15,1]<-"ARIMAX rolling" 
AC5[15,1]<-"2"
AC4[15,2:4]<-round(accuracy(pr.rol.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[16,1]<-"ETS rolling" 
AC5[16,1]<-"2"
AC4[16,2:4]<-round(accuracy(pr.rol.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[17,1]<- "ADL rolling" 
AC5[17,1]<-"2"
AC4[17,2:4]<-round(accuracy(pr.rol.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[18,1]<- "VAR rolling"
AC5[18,1]<-"2"
AC4[18,2:4]<-round(accuracy(pr.rol.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[19,1]<- "FAVAR rolling" 
AC5[19,1]<-"2"
AC4[19,2:4]<-round(accuracy(pr.rol.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC4[20,1]<-"ARIMA fijo bagged" 
AC5[20,1]<-"2"
AC4[20,2:4]<-round(accuracy(pr.f.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[21,1]<-"ARIMAX fijo bagged" 
AC5[21,1]<-"2"
AC4[21,2:4]<-round(accuracy(pr.f.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[22,1]<-"ETS fijo bagged" 
AC5[22,1]<-"2"
AC4[22,2:4]<-round(accuracy(pr.f.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[23,1]<- "VAR fijo bagged" 
AC5[23,1]<-"2"
AC4[23,2:4]<-round(accuracy(pr.f.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[24,1]<- "FAVAR fijo bagged"
AC5[24,1]<-"2"
AC4[24,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC4[25,1]<-"ARIMA recursivo bagged" 
AC5[25,1]<-"2"
AC4[25,2:4]<-round(accuracy(pr.rec.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[26,1]<-"ARIMAX recursivo bagged" 
AC5[26,1]<-"2"
AC4[26,2:4]<-round(accuracy(pr.rec.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[27,1]<-"ETS recursivo bagged" 
AC5[27,1]<-"2"
AC4[27,2:4]<-round(accuracy(pr.rec.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[28,1]<- "VAR recursivo bagged" 
AC5[28,1]<-"2"
AC4[28,2:4]<-round(accuracy(pr.rec.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[29,1]<- "FAVAR recursivo bagged"
AC5[29,1]<-"2"
AC4[29,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC4[30,1]<-"ARIMA rolling bagged" 
AC5[30,1]<-"2"
AC4[30,2:4]<-round(accuracy(pr.rol.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[31,1]<-"ARIMAX rolling bagged" 
AC5[31,1]<-"2"
AC4[31,2:4]<-round(accuracy(pr.rol.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[32,1]<-"ETS rolling bagged" 
AC5[32,1]<-"2"
AC4[32,2:4]<-round(accuracy(pr.rol.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[33,1]<- "VAR rolling bagged" 
AC5[33,1]<-"2"
AC4[33,2:4]<-round(accuracy(pr.rol.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[34,1]<- "FAVAR rolling bagged"
AC5[34,1]<-"2"
AC4[34,2:4]<-round(accuracy(pr.rol.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)


### Test de Diebold Mariano ###

# Agregamos a la tabla el test de Diebold Mariano. 

# Generamos los errores de pronósticos de todos los modelos utilizados. 

# Separamos los casos según la cantidad de pasos adelante utilizados en cada pronóstico.

### h=1 ###

# Esquema fijo.

error.bench <- out.of.sample[,2]- pr.bench

error.arima.f.h1 <- out.of.sample[,2]-pr.f.h1[,1]

error.arimax.f.h1 <- out.of.sample[,2]-pr.f.h1[,2]

error.ets.f.h1 <- out.of.sample[,2]-pr.f.h1[,3]

error.adl.f.h1 <- out.of.sample[,2]-pr.f.h1[,4]

error.var.f.h1 <- out.of.sample[,2]-pr.f.h1[,5]

error.favar.f.h1 <- out.of.sample[,2]-pr.f.h1[,6]

# Esquema recursivo.

error.arima.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,1]

error.arimax.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,2]

error.ets.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,3]

error.adl.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,4]

error.var.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,5]

error.favar.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,6]

# Esquema rolling.

error.arima.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,1]

error.arimax.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,2]

error.ets.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,3]

error.adl.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,4]

error.var.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,5]

error.favar.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,6]

# Bagging 

# Esquema fijo bagged.

error.arima.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,1]

error.arimax.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,2]

error.ets.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,3]

error.var.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,4]

error.favar.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,5]

# Esquema recursivo bagged.

error.arima.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,1]

error.arimax.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,2]

error.ets.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,3]

error.var.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,4]

error.favar.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,5]

# Esquema rolling bagged.

error.arima.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,1]

error.arimax.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,2]

error.ets.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,3]

error.var.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,4]

error.favar.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,5]

### h=2 

error.arima.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,1]

error.arimax.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,2]

error.ets.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,3]

error.adl.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,4]

error.var.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,5]

error.favar.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,6]

# Esquema recursivo.

error.arima.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,1]

error.arimax.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,2]

error.ets.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,3]

error.adl.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,4]

error.var.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,5]

error.favar.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,6]

# Esquema rolling. 

error.arima.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,1]

error.arimax.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,2]

error.ets.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,3]

error.adl.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,4]

error.var.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,5]

error.favar.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,6]

# Bagging 

# Esquema fijo bagged.

error.arima.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,1]

error.arimax.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,2]

error.ets.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,3]

error.var.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,4]

error.favar.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,5]

# Esquema recursivo bagged.

error.arima.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,1]

error.arimax.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,2]

error.ets.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,3]

error.var.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,4]

error.favar.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,5]

# Esquema rolling.

error.arima.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,1]

error.arimax.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,2]

error.ets.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,3]

error.var.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,4]

error.favar.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,5]

### h=7

error.arima.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,1]

error.arimax.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,2]

error.ets.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,3]

error.adl.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,4]

error.var.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,5]

error.favar.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,6]

# Esquema recursivo.

error.arima.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,1]

error.arimax.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,2]

error.ets.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,3]

error.adl.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,4]

error.var.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,5]

error.favar.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,6]

# Esquema rolling. 

error.arima.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,1]

error.arimax.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,2]

error.ets.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,3]

error.adl.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,4]

error.var.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,5]

error.favar.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,6]

# Bagging. 

# Esquema fijo bagged.

error.arima.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,1]

error.arimax.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,2]

error.ets.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,3]

error.var.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,5]

error.favar.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,4]

# Esquema recursivo bagged.

error.arima.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,1]

error.arimax.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,2]

error.ets.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,3]

error.var.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,5]

error.favar.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,4]

# Esquema rolling bagged.

error.arima.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,1]

error.arimax.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,2]

error.ets.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,3]

error.var.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,5]

error.favar.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,4]

# Funciones de pérdida.

DL.arima.f.h1 <- error.arima.f.h1^{2} - error.bench^{2}

DL.arimax.f.h1 <- error.arimax.f.h1^{2} - error.bench^{2}

DL.ets.f.h1 <- error.ets.f.h1^{2} - error.bench^{2}

DL.adl.f.h1 <- error.adl.f.h1^{2} - error.bench^{2}

DL.var.f.h1 <- error.var.f.h1^{2} - error.bench^{2}

DL.favar.f.h1 <- error.favar.f.h1^{2} - error.bench^{2}

# Esquema recursivo.

DL.arima.rec.h1 <- error.arima.rec.h1^{2} - error.bench^{2}

DL.arimax.rec.h1 <- error.arimax.rec.h1^{2} - error.bench^{2}

DL.ets.rec.h1 <- error.ets.rec.h1^{2} - error.bench^{2}

DL.adl.rec.h1 <- error.adl.rec.h1^{2} - error.bench^{2}

DL.var.rec.h1 <- error.var.rec.h1^{2} - error.bench^{2}

DL.favar.rec.h1 <- error.favar.rec.h1^{2} - error.bench^{2}

# Esquema rolling.

DL.arima.rol.h1 <- error.arima.rol.h1^{2} - error.bench^{2}

DL.arimax.rol.h1 <- error.arimax.rol.h1^{2} - error.bench^{2}

DL.ets.rol.h1 <- error.ets.rol.h1^{2} - error.bench^{2}

DL.adl.rol.h1 <- error.adl.rol.h1^{2} - error.bench^{2}

DL.var.rol.h1 <- error.var.rol.h1^{2} - error.bench^{2}

DL.favar.rol.h1 <- error.favar.rol.h1^{2} - error.bench^{2}

# Bagging  

DL.arima.f.h1.b <- error.arima.f.h1.b^{2} - error.bench^{2}

DL.arimax.f.h1.b <- error.arimax.f.h1.b^{2} - error.bench^{2}

DL.ets.f.h1.b <- error.ets.f.h1.b^{2} - error.bench^{2}

DL.var.f.h1.b <- error.var.f.h1.b^{2} - error.bench^{2}

DL.favar.f.h1.b <- error.favar.f.h1.b^{2} - error.bench^{2}

# Esquema recursivo.

DL.arima.rec.h1.b <- error.arima.rec.h1.b^{2} - error.bench^{2}

DL.arimax.rec.h1.b <- error.arimax.rec.h1.b^{2} - error.bench^{2}

DL.ets.rec.h1.b <- error.ets.rec.h1.b^{2} - error.bench^{2}

DL.var.rec.h1.b <- error.var.rec.h1.b^{2} - error.bench^{2}

DL.favar.rec.h1.b <- error.favar.rec.h1.b^{2} - error.bench^{2}

# Esquema rolling. 

DL.arima.rol.h1.b <- error.arima.rol.h1.b^{2} - error.bench^{2}

DL.arimax.rol.h1.b <- error.arimax.rol.h1.b^{2} - error.bench^{2}

DL.ets.rol.h1.b <- error.ets.rol.h1.b^{2} - error.bench^{2}

DL.var.rol.h1.b <- error.var.rol.h1.b^{2} - error.bench^{2}

DL.favar.rol.h1.b <- error.favar.rol.h1.b^{2} - error.bench^{2}

### h=2 

# Esquema fijo.

DL.arima.f.h2 <- error.arima.f.h2^{2} - error.bench[-1,]^{2}

DL.arimax.f.h2 <- error.arimax.f.h2^{2} - error.bench[-1,]^{2}

DL.ets.f.h2 <- error.ets.f.h2^{2} - error.bench[-1,]^{2}

DL.adl.f.h2 <- error.adl.f.h2^{2} - error.bench[-1,]^{2}

DL.var.f.h2 <- error.var.f.h2^{2} - error.bench[-1,]^{2}

DL.favar.f.h2 <- error.favar.f.h2^{2} - error.bench[-1,]^{2}

# Esquema recursivo.

DL.arima.rec.h2 <- error.arima.rec.h2^{2} - error.bench[-1,]^{2}

DL.arimax.rec.h2 <- error.arimax.rec.h2^{2} - error.bench[-1,]^{2}

DL.ets.rec.h2 <- error.ets.rec.h2^{2} - error.bench[-1,]^{2}

DL.adl.rec.h2 <- error.adl.rec.h2^{2} - error.bench[-1,]^{2}

DL.var.rec.h2 <- error.var.rec.h2^{2} - error.bench[-1,]^{2}

DL.favar.rec.h2 <- error.favar.rec.h2^{2} - error.bench[-1,]^{2}

# Esquema rolling. 

DL.arima.rol.h2 <- error.arima.rol.h2^{2} - error.bench[-1,]^{2}

DL.arimax.rol.h2 <- error.arimax.rol.h2^{2} - error.bench[-1,]^{2}

DL.ets.rol.h2 <- error.ets.rol.h2^{2} - error.bench[-1,]^{2}

DL.adl.rol.h2 <- error.adl.rol.h2^{2} - error.bench[-1,]^{2}

DL.var.rol.h2 <- error.var.rol.h2^{2} - error.bench[-1,]^{2}

DL.favar.rol.h2 <- error.favar.rol.h2^{2} - error.bench[-1,]^{2}

# Bagging 

# Esquema fijo.

DL.arima.f.h2.b <- error.arima.f.h2.b^{2} - error.bench[-1,]^{2}

DL.arimax.f.h2.b <- error.arimax.f.h2.b^{2} - error.bench[-1,]^{2}

DL.ets.f.h2.b <- error.ets.f.h2.b^{2} - error.bench[-1,]^{2}

DL.var.f.h2.b <- error.var.f.h2.b^{2} - error.bench[-1,]^{2}

DL.favar.f.h2.b <- error.favar.f.h2.b^{2} - error.bench[-1,]^{2}

# Esquema recursivo. 

DL.arima.rec.h2.b <- error.arima.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.arimax.rec.h2.b <- error.arimax.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.ets.rec.h2.b <- error.ets.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.var.rec.h2.b <- error.var.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.favar.rec.h2.b <- error.favar.rec.h2.b^{2} - error.bench[-1,]^{2}

# Esquema rolling. 

DL.arima.rol.h2.b <- error.arima.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.arimax.rol.h2.b <- error.arimax.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.ets.rol.h2.b <- error.ets.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.var.rol.h2.b <- error.var.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.favar.rol.h2.b <- error.favar.rol.h2.b^{2} - error.bench[-1,]^{2}

### h = 7 

# Esquema fijo.

DL.arima.f.h7 <- error.arima.f.h7^{2}  - error.bench^{2}

DL.arimax.f.h7 <- error.arimax.f.h7^{2}  - error.bench[7:58,]^{2}

DL.ets.f.h7 <- error.ets.f.h7^{2}  - error.bench[7:58,]^{2}

DL.adl.f.h7 <- error.adl.f.h7^{2}  - error.bench[7:58,]^{2}

DL.var.f.h7 <- error.var.f.h7^{2}  - error.bench[7:58,]^{2}

DL.favar.f.h7 <- error.favar.f.h7^{2}  - error.bench[7:58,]^{2}

# Esquema recursivo.

DL.arima.rec.h7 <- error.arima.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.arimax.rec.h7 <- error.arimax.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.ets.rec.h7 <- error.ets.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.adl.rec.h7 <- error.adl.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.var.rec.h7 <- error.var.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.favar.rec.h7 <- error.favar.rec.h7^{2}  - error.bench[7:58,]^{2}

# Esquema rolling. 

DL.arima.rol.h7 <- error.arima.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.arimax.rol.h7 <- error.arimax.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.ets.rol.h7 <- error.ets.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.adl.rol.h7 <- error.adl.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.var.rol.h7 <- error.var.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.favar.rol.h7 <- error.favar.rol.h7^{2}  - error.bench[7:58,]^{2}

# Bagging 

# Esquema fijo.

DL.arima.f.h7.b <- error.arima.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.arimax.f.h7.b <- error.arimax.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.ets.f.h7.b <- error.ets.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.var.f.h7.b <- error.var.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.favar.f.h7.b <- error.favar.f.h7.b^{2}  - error.bench[7:58,]^{2}

# Esquema recursivo. 

DL.arima.rec.h7.b <- error.arima.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.arimax.rec.h7.b <- error.arimax.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.ets.rec.h7.b <- error.ets.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.var.rec.h7.b <- error.var.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.favar.rec.h7.b <- error.favar.rec.h7.b^{2}  - error.bench[7:58,]^{2}

# Esquema rolling.

DL.arima.rol.h7.b <- error.arima.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.arimax.rol.h7.b <- error.arimax.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.ets.rol.h7.b <- error.ets.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.var.rol.h7.b <- error.var.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.favar.rol.h7.b <- error.favar.rol.h7.b^{2}  - error.bench[7:58,]^{2}

# Realizamos el test de Diebold Mariano, ajustamos por NW para cuando h>1.

#### h = 1 

# Estadístico.

AC[2,5]<-round(summary(lm(DL.arima.f.h1~1))$coefficients[3],4)
AC[3,5]<-round(summary(lm(DL.arimax.f.h1~1))$coefficients[3],4)
AC[4,5]<-round(summary(lm(DL.ets.f.h1~1))$coefficients[3],4)
AC[5,5]<-round(summary(lm(DL.adl.f.h1~1))$coefficients[3],4)
AC[6,5]<-round(summary(lm(DL.var.f.h1~1))$coefficients[3],4)
AC[7,5]<-round(summary(lm(DL.favar.f.h1~1))$coefficients[3],4)

AC[8,5]<-round(summary(lm(DL.arima.rec.h1~1))$coefficients[3],4)
AC[9,5]<-round(summary(lm(DL.arimax.rec.h1~1))$coefficients[3],4)
AC[10,5]<-round(summary(lm(DL.ets.rec.h1~1))$coefficients[3],4)
AC[11,5]<-round(summary(lm(DL.adl.rec.h1~1))$coefficients[3],4)
AC[12,5]<-round(summary(lm(DL.var.rec.h1~1))$coefficients[3],4)
AC[13,5]<-round(summary(lm(DL.favar.rec.h1~1))$coefficients[3],4)

AC[14,5]<-round(summary(lm(DL.arima.rol.h1~1))$coefficients[3],4)
AC[15,5]<-round(summary(lm(DL.arimax.rol.h1~1))$coefficients[3],4)
AC[16,5]<-round(summary(lm(DL.ets.rol.h1~1))$coefficients[3],4)
AC[17,5]<-round(summary(lm(DL.adl.rol.h1~1))$coefficients[3],4)
AC[18,5]<-round(summary(lm(DL.var.rol.h1~1))$coefficients[3],4)
AC[19,5]<-round(summary(lm(DL.favar.rol.h1~1))$coefficients[3],4)

# Bagging.

AC[20,5]<-round(summary(lm(DL.arima.f.h1.b~1))$coefficients[3],4)
AC[21,5]<-round(summary(lm(DL.arimax.f.h1.b~1))$coefficients[3],4)
AC[22,5]<-round(summary(lm(DL.ets.f.h1.b~1))$coefficients[3],4)
AC[23,5]<-round(summary(lm(DL.var.f.h1.b~1))$coefficients[3],4)
AC[24,5]<-round(summary(lm(DL.favar.f.h1.b~1))$coefficients[3],4)

AC[25,5]<-round(summary(lm(DL.arima.rec.h1.b~1))$coefficients[3],4)
AC[26,5]<-round(summary(lm(DL.arimax.rec.h1.b~1))$coefficients[3],4)
AC[27,5]<-round(summary(lm(DL.ets.rec.h1.b~1))$coefficients[3],4)
AC[28,5]<-round(summary(lm(DL.var.rec.h1.b~1))$coefficients[3],4)
AC[29,5]<-round(summary(lm(DL.favar.rec.h1.b~1))$coefficients[3],4)

AC[30,5]<-round(summary(lm(DL.arima.rol.h1.b~1))$coefficients[3],4)
AC[31,5]<-round(summary(lm(DL.arimax.rol.h1.b~1))$coefficients[3],4)
AC[32,5]<-round(summary(lm(DL.ets.rol.h1.b~1))$coefficients[3],4)
AC[33,5]<-round(summary(lm(DL.var.rol.h1.b~1))$coefficients[3],4)
AC[34,5]<-round(summary(lm(DL.favar.rol.h1.b~1))$coefficients[3],4)

# P-value.

AC[2,6]<-round(summary(lm(DL.arima.f.h1~1))$coefficients[4],4)
AC[3,6]<-round(summary(lm(DL.arimax.f.h1~1))$coefficients[4],4)
AC[4,6]<-round(summary(lm(DL.ets.f.h1~1))$coefficients[4],4)
AC[5,6]<-round(summary(lm(DL.adl.f.h1~1))$coefficients[4],4)
AC[6,6]<-round(summary(lm(DL.var.f.h1~1))$coefficients[4],4)
AC[7,6]<-round(summary(lm(DL.favar.f.h1~1))$coefficients[4],4)

AC[8,6]<-round(summary(lm(DL.arima.rec.h1~1))$coefficients[4],4)
AC[9,6]<-round(summary(lm(DL.arimax.rec.h1~1))$coefficients[4],4)
AC[10,6]<-round(summary(lm(DL.ets.rec.h1~1))$coefficients[4],4)
AC[11,6]<-round(summary(lm(DL.adl.rec.h1~1))$coefficients[4],4)
AC[12,6]<-round(summary(lm(DL.var.rec.h1~1))$coefficients[4],4)
AC[13,6]<-round(summary(lm(DL.favar.rec.h1~1))$coefficients[4],4)

AC[14,6]<-round(summary(lm(DL.arima.rol.h1~1))$coefficients[4],4)
AC[15,6]<-round(summary(lm(DL.arimax.rol.h1~1))$coefficients[4],4)
AC[16,6]<-round(summary(lm(DL.ets.rol.h1~1))$coefficients[4],4)
AC[17,6]<-round(summary(lm(DL.adl.rol.h1~1))$coefficients[4],4)
AC[18,6]<-round(summary(lm(DL.var.rol.h1~1))$coefficients[4],4)
AC[19,6]<-round(summary(lm(DL.arima.rol.h1.b~1))$coefficients[4],4)

# Bagging. 

AC[20,6]<-round(summary(lm(DL.arima.f.h1.b~1))$coefficients[4],4)
AC[21,6]<-round(summary(lm(DL.arimax.f.h1.b~1))$coefficients[4],4)
AC[22,6]<-round(summary(lm(DL.ets.f.h1.b~1))$coefficients[4],4)
AC[23,6]<-round(summary(lm(DL.var.f.h1.b~1))$coefficients[4],4)
AC[24,6]<-round(summary(lm(DL.favar.f.h1.b~1))$coefficients[4],4)

AC[25,6]<-round(summary(lm(DL.arima.rec.h1.b~1))$coefficients[4],4)
AC[26,6]<-round(summary(lm(DL.arimax.rec.h1.b~1))$coefficients[4],4)
AC[27,6]<-round(summary(lm(DL.ets.rec.h1.b~1))$coefficients[4],4)
AC[28,6]<-round(summary(lm(DL.var.rec.h1.b~1))$coefficients[4],4)
AC[29,6]<-round(summary(lm(DL.favar.rec.h1.b~1))$coefficients[4],4)

AC[30,6]<-round(summary(lm(DL.arima.rol.h1.b~1))$coefficients[4],4)
AC[31,6]<-round(summary(lm(DL.arimax.rol.h1.b~1))$coefficients[4],4)
AC[32,6]<-round(summary(lm(DL.ets.rol.h1.b~1))$coefficients[4],4)
AC[33,6]<-round(summary(lm(DL.var.rol.h1.b~1))$coefficients[4],4)
AC[34,6]<-round(summary(lm(DL.favar.rol.h1.b~1))$coefficients[4],4)

### h = 2

# Estadístico. 

AC1[2,5]<-round(summary(lm(DL.arima.f.h2~1))$coefficients[3],4)
AC1[3,5]<-round(summary(lm(DL.arimax.f.h2~1))$coefficients[3],4)
AC1[4,5]<-round(summary(lm(DL.ets.f.h2~1))$coefficients[3],4)
AC1[5,5]<-round(summary(lm(DL.adl.f.h2~1))$coefficients[3],4)
AC1[6,5]<-round(summary(lm(DL.var.f.h2~1))$coefficients[3],4)
AC1[7,5]<-round(summary(lm(DL.favar.f.h2~1))$coefficients[3],4)

AC1[8,5]<-round(summary(lm(DL.arima.rec.h2~1))$coefficients[3],4)
AC1[9,5]<-round(summary(lm(DL.arimax.rec.h2~1))$coefficients[3],4)
AC1[10,5]<-round(summary(lm(DL.ets.rec.h2~1))$coefficients[3],4)
AC1[11,5]<-round(summary(lm(DL.adl.rec.h2~1))$coefficients[3],4)
AC1[12,5]<-round(summary(lm(DL.var.rec.h2~1))$coefficients[3],4)
AC1[13,5]<-round(summary(lm(DL.favar.rec.h2~1))$coefficients[3],4)

AC1[14,5]<-round(summary(lm(DL.arima.rol.h2~1))$coefficients[3],4)
AC1[15,5]<-round(summary(lm(DL.arimax.rol.h2~1))$coefficients[3],4)
AC1[16,5]<-round(summary(lm(DL.ets.rol.h2~1))$coefficients[3],4)
AC1[17,5]<-round(summary(lm(DL.adl.rol.h2~1))$coefficients[3],4)
AC1[18,5]<-round(summary(lm(DL.var.rol.h2~1))$coefficients[3],4)
AC1[19,5]<-round(summary(lm(DL.favar.rol.h2~1))$coefficients[3],4)

# Bagging.

AC1[20,5]<-round(summary(lm(DL.arima.f.h2.b~1))$coefficients[3],4)
AC1[21,5]<-round(summary(lm(DL.arimax.f.h2.b~1))$coefficients[3],4)
AC1[22,5]<-round(summary(lm(DL.ets.f.h2.b~1))$coefficients[3],4)
AC1[23,5]<-round(summary(lm(DL.var.f.h2.b~1))$coefficients[3],4)
AC1[24,5]<-round(summary(lm(DL.favar.f.h2.b~1))$coefficients[3],4)

AC1[25,5]<-round(summary(lm(DL.arima.rec.h2.b~1))$coefficients[3],4)
AC1[26,5]<-round(summary(lm(DL.arimax.rec.h2.b~1))$coefficients[3],4)
AC1[27,5]<-round(summary(lm(DL.ets.rec.h2.b~1))$coefficients[3],4)
AC1[28,5]<-round(summary(lm(DL.var.rec.h2.b~1))$coefficients[3],4)
AC1[29,5]<-round(summary(lm(DL.favar.rec.h2.b~1))$coefficients[3],4)

AC1[30,5]<-round(summary(lm(DL.arima.rol.h2.b~1))$coefficients[3],4)
AC1[31,5]<-round(summary(lm(DL.arimax.rol.h2.b~1))$coefficients[3],4)
AC1[32,5]<-round(summary(lm(DL.ets.rol.h2.b~1))$coefficients[3],4)
AC1[33,5]<-round(summary(lm(DL.var.rol.h2.b~1))$coefficients[3],4)
AC1[34,5]<-round(summary(lm(DL.favar.rol.h2.b~1))$coefficients[3],4)

# P-value.

# Esquema fijo.

AC1[2,6]<-round(coeftest(lm(DL.arima.f.h2~1), vcov = NeweyWest(lm(DL.arima.f.h2~1)))[4],4)
AC1[3,6]<-round(coeftest(lm(DL.arimax.f.h2~1), vcov = NeweyWest(lm(DL.arimax.f.h2~1)))[4],4)
AC1[4,6]<-round(coeftest(lm(DL.ets.f.h2~1), vcov = NeweyWest(lm(DL.ets.f.h2~1)))[4],4)
AC1[5,6]<-round(coeftest(lm(DL.adl.f.h2~1), vcov = NeweyWest(lm(DL.arimax.f.h2~1)))[4],4)
AC1[6,6]<-round(coeftest(lm(DL.var.f.h2~1), vcov = NeweyWest(lm(DL.var.f.h2~1)))[4],4)
AC1[7,6]<-round(coeftest(lm(DL.favar.f.h2~1), vcov = NeweyWest(lm(DL.favar.f.h2~1)))[4],4)

# Esquema recursivo.

AC1[8,6]<-round(coeftest(lm(DL.arima.rec.h2~1), vcov = NeweyWest(lm(DL.arima.rec.h2~1)))[4],4)
AC1[9,6]<-round(coeftest(lm(DL.arimax.rec.h2~1), vcov = NeweyWest(lm(DL.arimax.rec.h2~1)))[4],4)
AC1[10,6]<-round(coeftest(lm(DL.ets.rec.h2~1), vcov = NeweyWest(lm(DL.ets.rec.h2~1)))[4],4)
AC1[11,6]<-round(coeftest(lm(DL.adl.rec.h2~1), vcov = NeweyWest(lm(DL.arimax.rec.h2~1)))[4],4)
AC1[12,6]<-round(coeftest(lm(DL.var.rec.h2~1), vcov = NeweyWest(lm(DL.var.rec.h2~1)))[4],4)
AC1[13,6]<-round(coeftest(lm(DL.favar.rec.h2~1), vcov = NeweyWest(lm(DL.favar.rec.h2~1)))[4],4)

# Esquema rolling. 

AC1[14,6]<-round(coeftest(lm(DL.arima.rol.h2~1), vcov = NeweyWest(lm(DL.arima.rol.h2~1)))[4],4)
AC1[15,6]<-round(coeftest(lm(DL.arimax.rol.h2~1), vcov = NeweyWest(lm(DL.arimax.rol.h2~1)))[4],4)
AC1[16,6]<-round(coeftest(lm(DL.ets.rol.h2~1), vcov = NeweyWest(lm(DL.ets.rol.h2~1)))[4],4)
AC1[17,6]<-round(coeftest(lm(DL.adl.rol.h2~1), vcov = NeweyWest(lm(DL.arimax.rol.h2~1)))[4],4)
AC1[18,6]<-round(coeftest(lm(DL.var.rol.h2~1), vcov = NeweyWest(lm(DL.var.rol.h2~1)))[4],4)
AC1[19,6]<-round(coeftest(lm(DL.favar.rol.h2~1), vcov = NeweyWest(lm(DL.favar.rol.h2~1)))[4],4)

# Bagging.

AC1[20,6]<-round(coeftest(lm(DL.arima.f.h2.b~1), vcov = NeweyWest(lm(DL.arima.f.h2.b~1)))[4],4)
AC1[21,6]<-round(coeftest(lm(DL.arimax.f.h2.b~1), vcov = NeweyWest(lm(DL.arimax.f.h2.b~1)))[4],4)
AC1[22,6]<-round(coeftest(lm(DL.ets.f.h2.b~1), vcov = NeweyWest(lm(DL.ets.f.h2.b~1)))[4],4)
AC1[23,6]<-round(coeftest(lm(DL.var.f.h2.b~1), vcov = NeweyWest(lm(DL.var.f.h2.b~1)))[4],4)
AC1[24,6]<-round(coeftest(lm(DL.favar.f.h2.b~1), vcov = NeweyWest(lm(DL.favar.f.h2.b~1)))[4],4)

AC1[25,6]<-round(coeftest(lm(DL.arima.rec.h2.b~1), vcov = NeweyWest(lm(DL.arima.rec.h2.b~1)))[4],4)
AC1[26,6]<-round(coeftest(lm(DL.arimax.rec.h2.b~1), vcov = NeweyWest(lm(DL.arimax.rec.h2.b~1)))[4],4)
AC1[27,6]<-round(coeftest(lm(DL.ets.rec.h2.b~1), vcov = NeweyWest(lm(DL.ets.rec.h2.b~1)))[4],4)
AC1[28,6]<-round(coeftest(lm(DL.var.rec.h2.b~1), vcov = NeweyWest(lm(DL.var.rec.h2.b~1)))[4],4)
AC1[29,6]<-round(coeftest(lm(DL.favar.rec.h2.b~1), vcov = NeweyWest(lm(DL.favar.rec.h2~1)))[4],4)

AC1[30,6]<-round(coeftest(lm(DL.arima.rol.h2.b~1), vcov = NeweyWest(lm(DL.arima.rol.h2.b~1)))[4],4)
AC1[31,6]<-round(coeftest(lm(DL.arimax.rol.h2.b~1), vcov = NeweyWest(lm(DL.arimax.rol.h2.b~1)))[4],4)
AC1[32,6]<-round(coeftest(lm(DL.ets.rol.h2.b~1), vcov = NeweyWest(lm(DL.ets.rol.h2.b~1)))[4],4)
AC1[33,6]<-round(coeftest(lm(DL.var.rol.h2.b~1), vcov = NeweyWest(lm(DL.var.rol.h2.b~1)))[4],4)
AC1[34,6]<-round(coeftest(lm(DL.favar.rol.h2.b~1), vcov = NeweyWest(lm(DL.favar.rol.h2.b~1)))[4],4)

### h= 7 

# Estadístico.

AC4[2,5]<-round(summary(lm(DL.arima.f.h7~1))$coefficients[3],4)
AC4[3,5]<-round(summary(lm(DL.arimax.f.h7~1))$coefficients[3],4)
AC4[4,5]<-round(summary(lm(DL.ets.f.h7~1))$coefficients[3],4)
AC4[5,5]<-round(summary(lm(DL.adl.f.h7~1))$coefficients[3],4)
AC4[6,5]<-round(summary(lm(DL.var.f.h7~1))$coefficients[3],4)
AC4[7,5]<-round(summary(lm(DL.favar.f.h7~1))$coefficients[3],4)

AC4[8,5]<-round(summary(lm(DL.arima.rec.h7~1))$coefficients[3],4)
AC4[9,5]<-round(summary(lm(DL.arimax.rec.h7~1))$coefficients[3],4)
AC4[10,5]<-round(summary(lm(DL.ets.rec.h7~1))$coefficients[3],4)
AC4[11,5]<-round(summary(lm(DL.adl.rec.h7~1))$coefficients[3],4)
AC4[12,5]<-round(summary(lm(DL.var.rec.h7~1))$coefficients[3],4)
AC4[13,5]<-round(summary(lm(DL.favar.rec.h7~1))$coefficients[3],4)

AC4[14,5]<-round(summary(lm(DL.arima.rol.h7~1))$coefficients[3],4)
AC4[15,5]<-round(summary(lm(DL.arimax.rol.h7~1))$coefficients[3],4)
AC4[16,5]<-round(summary(lm(DL.ets.rol.h7~1))$coefficients[3],4)
AC4[17,5]<-round(summary(lm(DL.adl.rol.h7~1))$coefficients[3],4)
AC4[18,5]<-round(summary(lm(DL.var.rol.h7~1))$coefficients[3],4)
AC4[19,5]<-round(summary(lm(DL.favar.rol.h7~1))$coefficients[3],4)

# Bagging.

AC4[20,5]<-round(summary(lm(DL.arima.f.h7.b~1))$coefficients[3],4)
AC4[21,5]<-round(summary(lm(DL.arimax.f.h7.b~1))$coefficients[3],4)
AC4[22,5]<-round(summary(lm(DL.ets.f.h7.b~1))$coefficients[3],4)
AC4[23,5]<-round(summary(lm(DL.var.f.h7.b~1))$coefficients[3],4)
AC4[24,5]<-round(summary(lm(DL.favar.f.h7.b~1))$coefficients[3],4)

AC4[25,5]<-round(summary(lm(DL.arima.rec.h7.b~1))$coefficients[3],4)
AC4[26,5]<-round(summary(lm(DL.arimax.rec.h7.b~1))$coefficients[3],4)
AC4[27,5]<-round(summary(lm(DL.ets.rec.h7.b~1))$coefficients[3],4)
AC4[28,5]<-round(summary(lm(DL.var.rec.h7.b~1))$coefficients[3],4)
AC4[29,5]<-round(summary(lm(DL.favar.rec.h7.b~1))$coefficients[3],4)

AC4[30,5]<-round(summary(lm(DL.arima.rol.h7.b~1))$coefficients[3],4)
AC4[31,5]<-round(summary(lm(DL.arimax.rol.h7.b~1))$coefficients[3],4)
AC4[32,5]<-round(summary(lm(DL.ets.rol.h7.b~1))$coefficients[3],4)
AC4[33,5]<-round(summary(lm(DL.var.rol.h7.b~1))$coefficients[3],4)
AC4[34,5]<-round(summary(lm(DL.favar.rol.h7.b~1))$coefficients[3],4)

# P-value. 

# Esquema fijo. 

AC4[2,6]<-round(coeftest(lm(DL.arima.f.h7~1), vcov = NeweyWest(lm(DL.arima.f.h7~1)))[4],4)
AC4[3,6]<-round(coeftest(lm(DL.arimax.f.h7~1), vcov = NeweyWest(lm(DL.arimax.f.h7~1)))[4],4)
AC4[4,6]<-round(coeftest(lm(DL.ets.f.h7~1), vcov = NeweyWest(lm(DL.ets.f.h7~1)))[4],4)
AC4[5,6]<-round(coeftest(lm(DL.adl.f.h7~1), vcov = NeweyWest(lm(DL.arimax.f.h7~1)))[4],4)
AC4[6,6]<-round(coeftest(lm(DL.var.f.h7~1), vcov = NeweyWest(lm(DL.var.f.h7~1)))[4],4)
AC4[7,6]<-round(coeftest(lm(DL.favar.f.h7~1), vcov = NeweyWest(lm(DL.favar.f.h7~1)))[4],4)

# Esquema recursivo. 

AC4[8,6]<-round(coeftest(lm(DL.arima.rec.h7~1), vcov = NeweyWest(lm(DL.arima.rec.h7~1)))[4],4)
AC4[9,6]<-round(coeftest(lm(DL.arimax.rec.h7~1), vcov = NeweyWest(lm(DL.arimax.rec.h7~1)))[4],4)
AC4[10,6]<-round(coeftest(lm(DL.ets.rec.h7~1), vcov = NeweyWest(lm(DL.ets.rec.h7~1)))[4],4)
AC4[11,6]<-round(coeftest(lm(DL.adl.rec.h7~1), vcov = NeweyWest(lm(DL.arimax.rec.h7~1)))[4],4)
AC4[12,6]<-round(coeftest(lm(DL.var.rec.h7~1), vcov = NeweyWest(lm(DL.var.rec.h7~1)))[4],4)
AC4[13,6]<-round(coeftest(lm(DL.favar.rec.h7~1), vcov = NeweyWest(lm(DL.favar.rec.h7~1)))[4],4)

# Esquema rolling. 

AC4[14,6]<-round(coeftest(lm(DL.arima.rol.h7~1), vcov = NeweyWest(lm(DL.arima.rol.h7~1)))[4],4)
AC4[15,6]<-round(coeftest(lm(DL.arimax.rol.h7~1), vcov = NeweyWest(lm(DL.arimax.rol.h7~1)))[4],4)
AC4[16,6]<-round(coeftest(lm(DL.ets.rol.h7~1), vcov = NeweyWest(lm(DL.ets.rol.h7~1)))[4],4)
AC4[17,6]<-round(coeftest(lm(DL.adl.rol.h7~1), vcov = NeweyWest(lm(DL.arimax.rol.h7~1)))[4],4)
AC4[18,6]<-round(coeftest(lm(DL.var.rol.h7~1), vcov = NeweyWest(lm(DL.var.rol.h7~1)))[4],4)
AC4[19,6]<-round(coeftest(lm(DL.favar.rol.h7~1), vcov = NeweyWest(lm(DL.favar.rol.h7~1)))[4],4)

# Bagging.

AC4[20,6]<-round(coeftest(lm(DL.arima.f.h7.b~1), vcov = NeweyWest(lm(DL.arima.f.h7.b~1)))[4],4)
AC4[21,6]<-round(coeftest(lm(DL.arimax.f.h7.b~1), vcov = NeweyWest(lm(DL.arimax.f.h7.b~1)))[4],4)
AC4[22,6]<-round(coeftest(lm(DL.ets.f.h7.b~1), vcov = NeweyWest(lm(DL.ets.f.h7.b~1)))[4],4)
AC4[23,6]<-round(coeftest(lm(DL.var.f.h7.b~1), vcov = NeweyWest(lm(DL.var.f.h7.b~1)))[4],4)
AC4[24,6]<-round(coeftest(lm(DL.favar.f.h7.b~1), vcov = NeweyWest(lm(DL.favar.f.h7.b~1)))[4],4)

AC4[25,6]<-round(coeftest(lm(DL.arima.rec.h7.b~1), vcov = NeweyWest(lm(DL.arima.rec.h7.b~1)))[4],4)
AC4[26,6]<-round(coeftest(lm(DL.arimax.rec.h7.b~1), vcov = NeweyWest(lm(DL.arimax.rec.h7.b~1)))[4],4)
AC4[27,6]<-round(coeftest(lm(DL.ets.rec.h7.b~1), vcov = NeweyWest(lm(DL.ets.rec.h7.b~1)))[4],4)
AC4[28,6]<-round(coeftest(lm(DL.var.rec.h7.b~1), vcov = NeweyWest(lm(DL.var.rec.h7.b~1)))[4],4)
AC4[29,6]<-round(coeftest(lm(DL.favar.rec.h7.b~1), vcov = NeweyWest(lm(DL.favar.rec.h7~1)))[4],4)

AC4[30,6]<-round(coeftest(lm(DL.arima.rol.h7.b~1), vcov = NeweyWest(lm(DL.arima.rol.h7.b~1)))[4],4)
AC4[31,6]<-round(coeftest(lm(DL.arimax.rol.h7.b~1), vcov = NeweyWest(lm(DL.arimax.rol.h7.b~1)))[4],4)
AC4[32,6]<-round(coeftest(lm(DL.ets.rol.h7.b~1), vcov = NeweyWest(lm(DL.ets.rol.h7.b~1)))[4],4)
AC4[33,6]<-round(coeftest(lm(DL.var.rol.h7.b~1), vcov = NeweyWest(lm(DL.var.rol.h7.b~1)))[4],4)
AC4[34,6]<-round(coeftest(lm(DL.favar.rol.h7.b~1), vcov = NeweyWest(lm(DL.favar.rol.h7.b~1)))[4],4)

# Exportamos las tablas.

# Para h=1

colnames(AC) <- c("Modelo", "RMSE", "MAE", "MAPE", "DM", "p-val")
stargazer(AC[1:nrow(AC),], type = "latex")

# Para h=2

colnames(AC1) <- c("Modelo", "RMSE", "MAE", "MAPE", "DM", "p-val")
AC1[1,1:ncol(AC1)] <- AC[1,1:ncol(AC)]
stargazer(AC1[1:nrow(AC1),], type = "latex")

# Para h=7

colnames(AC4) <- c("Modelo", "RMSE", "MAE", "MAPE", "DM", "p-val")
AC4[1,1:ncol(AC4)] <- AC[1,1:ncol(AC)]
stargazer(AC4[1:nrow(AC4),], type = "latex")


### MEDIDAS DE ACCURACY COMPARABLES.

# En esta sección ajustaremos la cantidad de observaciones de los pronósticos
# para comparar todos los modelos.

AC<-matrix(NA,34,4)
colnames(AC) <- c("Modelo", "MAPE", "MAE", "RMSE")
AC2<-matrix(NA,34,1)

# Tabla 1

# Medidas de accuracy y test de DM de los pronósticos con esquema fijo y h=1.

AC[1,1]<-"AR(1)" 
AC2[1,1]<-"1"
AC[1,2:4]<-round(accuracy(pr.bench[7:58,], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[2,1]<-"ARIMA fijo" 
AC2[2,1]<-"1"
AC[2,2:4]<-round(accuracy(pr.f.h1[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[3,1]<-"ARIMAX fijo" 
AC2[3,1]<-"1"
AC[3,2:4]<-round(accuracy(pr.f.h1[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[4,1]<-"ETS fijo" 
AC2[4,1]<-"1"
AC[4,2:4]<-round(accuracy(pr.f.h1[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[5,1]<- "ADL fijo" 
AC2[5,1]<-"1"
AC[5,2:4]<-round(accuracy(pr.f.h1[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[6,1]<- "VAR fijo"
AC2[6,1]<-"1"
AC[6,2:4]<-round(accuracy(pr.f.h1[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[7,1]<- "FAVAR fijo" 
AC2[7,1]<-"1"
AC[7,2:4]<-round(accuracy(pr.f.h1[7:58,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=1.

AC[8,1]<-"ARIMA recursivo" 
AC2[8,1]<-"1"
AC[8,2:4]<-round(accuracy(pr.rec.h1[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[9,1]<-"ARIMAX recursivo" 
AC2[9,1]<-"1"
AC[9,2:4]<-round(accuracy(pr.rec.h1[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[10,1]<-"ETS recursivo" 
AC2[10,1]<-"1"
AC[10,2:4]<-round(accuracy(pr.rec.h1[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[11,1]<- "ADL recursivo" 
AC2[11,1]<-"1"
AC[11,2:4]<-round(accuracy(pr.rec.h1[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[12,1]<- "VAR recursivo"
AC2[12,1]<-"1"
AC[12,2:4]<-round(accuracy(pr.rec.h1[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[13,1]<- "FAVAR recursivo" 
AC2[13,1]<-"1"
AC[13,2:4]<-round(accuracy(pr.rec.h1[7:58,6], out.of.sample[7:58,2])[c(2:3,5)],4)

#  Medidas de accuracy de los pronósticos con esquema rolling y h=1.

AC[14,1]<-"ARIMA rolling" 
AC2[14,1]<-"1"
AC[14,2:4]<-round(accuracy(pr.rol.h1[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[15,1]<-"ARIMAX rolling" 
AC2[15,1]<-"1"
AC[15,2:4]<-round(accuracy(pr.rol.h1[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[16,1]<-"ETS rolling" 
AC2[16,1]<-"1"
AC[16,2:4]<-round(accuracy(pr.rol.h1[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[17,1]<- "ADL rolling" 
AC2[17,1]<-"1"
AC[17,2:4]<-round(accuracy(pr.rol.h1[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[18,1]<- "VAR rolling"
AC2[18,1]<-"1"
AC[18,2:4]<-round(accuracy(pr.rol.h1[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[19,1]<- "FAVAR rolling" 
AC2[19,1]<-"1"
AC[19,2:4]<-round(accuracy(pr.rol.h1[7:58,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 1 y series boots. 

AC[20,1]<-"ARIMA fijo bagged" 
AC2[20,1]<-"1"
AC[20,2:4]<-round(accuracy(pr.f.h1.b[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[21,1]<-"ARIMAX fijo bagged" 
AC2[21,1]<-"1"
AC[21,2:4]<-round(accuracy(pr.f.h1.b[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[22,1]<-"ETS fijo bagged" 
AC2[22,1]<-"1"
AC[22,2:4]<-round(accuracy(pr.f.h1.b[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[23,1]<- "VAR fijo bagged" 
AC2[23,1]<-"1"
AC[23,2:4]<-round(accuracy(pr.f.h1.b[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[24,1]<- "FAVAR fijo bagged"
AC2[24,1]<-"1"
AC[24,2:4]<-round(accuracy(pr.f.h1.b[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC[25,1]<-"ARIMA recursivo bagged" 
AC2[25,1]<-"1"
AC[25,2:4]<-round(accuracy(pr.rec.h1.b[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[26,1]<-"ARIMAX recursivo bagged" 
AC2[26,1]<-"1"
AC[26,2:4]<-round(accuracy(pr.rec.h1.b[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[27,1]<-"ETS recursivo bagged" 
AC2[27,1]<-"1"
AC[27,2:4]<-round(accuracy(pr.rec.h1.b[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[28,1]<- "VAR recursivo bagged" 
AC2[28,1]<-"1"
AC[28,2:4]<-round(accuracy(pr.rec.h1.b[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[29,1]<- "FAVAR recursivo bagged"
AC2[29,1]<-"1"
AC[29,2:4]<-round(accuracy(pr.rec.h1.b[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 1 y series boots.

AC[30,1]<-"ARIMA rolling bagged" 
AC2[23,1]<-"1"
AC[30,2:4]<-round(accuracy(pr.rol.h1.b[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[31,1]<-"ARIMAX rolling bagged" 
AC2[31,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[32,1]<-"ETS rollingbagged" 
AC2[32,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[33,1]<- "VAR rolling bagged" 
AC2[33,1]<-"1"
AC[33,2:4]<-round(accuracy(pr.rol.h1.b[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[34,1]<- "FAVAR rolling bagged"
AC2[34,1]<-"1"
AC[34,2:4]<-round(accuracy(pr.rol.h1.b[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)

# Tabla 2

AC1<-matrix(NA,34,4)
colnames(AC1) <- c("Modelo", "MAPE", "MAE", "RMSE")
AC3<-matrix(NA,34,1)

AC1[2,1]<-"ARIMA fijo" 
AC3[2,1]<-"2"
AC1[2,2:4]<-round(accuracy(pr.f.h2[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[3,1]<-"ARIMAX fijo" 
AC3[3,1]<-"2"
AC1[3,2:4]<-round(accuracy(pr.f.h2[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[4,1]<-"ETS fijo" 
AC3[4,1]<-"2"
AC1[4,2:4]<-round(accuracy(pr.f.h2[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[5,1]<- "ADL fijo" 
AC3[5,1]<-"2"
AC1[5,2:4]<-round(accuracy(pr.f.h2[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[6,1]<- "VAR fijo"
AC3[6,1]<-"2"
AC1[6,2:4]<-round(accuracy(pr.f.h2[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[7,1]<- "FAVAR fijo" 
AC3[7,1]<-"2"
AC1[7,2:4]<-round(accuracy(pr.f.h2[6:57,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=2.

AC1[8,1]<-"ARIMA recursivo" 
AC3[8,1]<-"2"
AC1[8,2:4]<-round(accuracy(pr.rec.h2[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[9,1]<-"ARIMAX recursivo" 
AC3[9,1]<-"2"
AC1[9,2:4]<-round(accuracy(pr.rec.h2[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[10,1]<-"ETS recursivo" 
AC3[10,1]<-"2"
AC1[10,2:4]<-round(accuracy(pr.rec.h2[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[11,1]<- "ADL recursivo" 
AC3[11,1]<-"2"
AC1[11,2:4]<-round(accuracy(pr.rec.h2[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[12,1]<- "VAR recursivo"
AC3[12,1]<-"2"
AC1[12,2:4]<-round(accuracy(pr.rec.h2[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[13,1]<- "FAVAR recursivo" 
AC3[13,1]<-"2"
AC1[13,2:4]<-round(accuracy(pr.rec.h2[6:57,6], out.of.sample[7:58,2])[c(2:3,5)],4)

#  Medidas de accuracy de los pronósticos con esquema rolling y h=2.

AC1[14,1]<-"ARIMA rolling" 
AC3[14,1]<-"2"
AC1[14,2:4]<-round(accuracy(pr.rol.h2[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[15,1]<-"ARIMAX rolling" 
AC3[15,1]<-"2"
AC1[15,2:4]<-round(accuracy(pr.rol.h2[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[16,1]<-"ETS rolling" 
AC3[16,1]<-"2"
AC1[16,2:4]<-round(accuracy(pr.rol.h2[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[17,1]<- "ADL rolling" 
AC3[17,1]<-"2"
AC1[17,2:4]<-round(accuracy(pr.rol.h2[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[18,1]<- "VAR rolling"
AC3[18,1]<-"2"
AC1[18,2:4]<-round(accuracy(pr.rol.h2[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[19,1]<- "FAVAR rolling" 
AC3[19,1]<-"2"
AC1[19,2:4]<-round(accuracy(pr.rol.h2[6:57,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC1[20,1]<-"ARIMA fijo bagged" 
AC3[20,1]<-"2"
AC1[20,2:4]<-round(accuracy(pr.f.h2.b[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[21,1]<-"ARIMAX fijo bagged" 
AC3[21,1]<-"2"
AC1[21,2:4]<-round(accuracy(pr.f.h2.b[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[22,1]<-"ETS fijo bagged" 
AC3[22,1]<-"2"
AC1[22,2:4]<-round(accuracy(pr.f.h2.b[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[23,1]<- "VAR fijo bagged" 
AC3[23,1]<-"2"
AC1[23,2:4]<-round(accuracy(pr.f.h2.b[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[24,1]<- "FAVAR fijo bagged"
AC3[24,1]<-"2"
AC1[24,2:4]<-round(accuracy(pr.f.h2.b[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC1[25,1]<-"ARIMA recursivo bagged" 
AC3[25,1]<-"2"
AC1[25,2:4]<-round(accuracy(pr.rec.h2.b[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[26,1]<-"ARIMAX recursivo bagged" 
AC3[26,1]<-"2"
AC1[26,2:4]<-round(accuracy(pr.rec.h2.b[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[27,1]<-"ETS recursivo bagged" 
AC3[27,1]<-"2"
AC1[27,2:4]<-round(accuracy(pr.rec.h2.b[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[28,1]<- "VAR recursivo bagged" 
AC3[28,1]<-"2"
AC1[28,2:4]<-round(accuracy(pr.rec.h2.b[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[29,1]<- "FAVAR recursivo bagged"
AC3[29,1]<-"2"
AC1[29,2:4]<-round(accuracy(pr.rec.h2.b[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC1[30,1]<-"ARIMA rolling bagged" 
AC3[30,1]<-"2"
AC1[30,2:4]<-round(accuracy(pr.rol.h2.b[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[31,1]<-"ARIMAX rolling bagged" 
AC3[31,1]<-"2"
AC1[31,2:4]<-round(accuracy(pr.rol.h2.b[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[32,1]<-"ETS rollingbagged" 
AC3[32,1]<-"2"
AC1[32,2:4]<-round(accuracy(pr.rol.h2.b[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[33,1]<- "VAR rolling bagged" 
AC3[33,1]<-"2"
AC1[33,2:4]<-round(accuracy(pr.rol.h2.b[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[34,1]<- "FAVAR rolling bagged"
AC3[34,1]<-"2"
AC1[34,2:4]<-round(accuracy(pr.rol.h2.b[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Tabla 3.

AC4<-matrix(NA,34,4)
colnames(AC4) <- c("Modelo", "MAPE", "MAE", "RMSE")
AC5<-matrix(NA,34,1)

AC4[2,1]<-"ARIMA fijo" 
AC5[2,1]<-"2"
AC4[2,2:4]<-round(accuracy(pr.f.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[3,1]<-"ARIMAX fijo" 
AC5[3,1]<-"2"
AC4[3,2:4]<-round(accuracy(pr.f.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[4,1]<-"ETS fijo" 
AC5[4,1]<-"2"
AC4[4,2:4]<-round(accuracy(pr.f.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[5,1]<- "ADL fijo" 
AC5[5,1]<-"2"
AC4[5,2:4]<-round(accuracy(pr.f.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[6,1]<- "VAR fijo"
AC5[6,1]<-"2"
AC4[6,2:4]<-round(accuracy(pr.f.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[7,1]<- "FAVAR fijo" 
AC5[7,1]<-"2"
AC4[7,2:4]<-round(accuracy(pr.f.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=7. 

AC4[8,1]<-"ARIMA recursivo" 
AC5[8,1]<-"2"
AC4[8,2:4]<-round(accuracy(pr.rec.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[9,1]<-"ARIMAX recursivo" 
AC5[9,1]<-"2"
AC4[9,2:4]<-round(accuracy(pr.rec.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[10,1]<-"ETS recursivo" 
AC5[10,1]<-"2"
AC4[10,2:4]<-round(accuracy(pr.rec.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[11,1]<- "ADL recursivo" 
AC5[11,1]<-"2"
AC4[11,2:4]<-round(accuracy(pr.rec.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[12,1]<- "VAR recursivo"
AC5[12,1]<-"2"
AC4[12,2:4]<-round(accuracy(pr.rec.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[13,1]<- "FAVAR recursivo" 
AC5[13,1]<-"2"
AC4[13,2:4]<-round(accuracy(pr.rec.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

#  Medidas de accuracy de los pronósticos con esquema rolling y h=7.

AC4[14,1]<-"ARIMA rolling" 
AC5[14,1]<-"2"
AC4[14,2:4]<-round(accuracy(pr.rol.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[15,1]<-"ARIMAX rolling" 
AC5[15,1]<-"2"
AC4[15,2:4]<-round(accuracy(pr.rol.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[16,1]<-"ETS rolling" 
AC5[16,1]<-"2"
AC4[16,2:4]<-round(accuracy(pr.rol.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[17,1]<- "ADL rolling" 
AC5[17,1]<-"2"
AC4[17,2:4]<-round(accuracy(pr.rol.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[18,1]<- "VAR rolling"
AC5[18,1]<-"2"
AC4[18,2:4]<-round(accuracy(pr.rol.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[19,1]<- "FAVAR rolling" 
AC5[19,1]<-"2"
AC4[19,2:4]<-round(accuracy(pr.rol.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC4[20,1]<-"ARIMA fijo bagged" 
AC5[20,1]<-"2"
AC4[20,2:4]<-round(accuracy(pr.f.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[21,1]<-"ARIMAX fijo bagged" 
AC5[21,1]<-"2"
AC4[21,2:4]<-round(accuracy(pr.f.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[22,1]<-"ETS fijo bagged" 
AC5[22,1]<-"2"
AC4[22,2:4]<-round(accuracy(pr.f.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[23,1]<- "VAR fijo bagged" 
AC5[23,1]<-"2"
AC4[23,2:4]<-round(accuracy(pr.f.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[24,1]<- "FAVAR fijo bagged"
AC5[24,1]<-"2"
AC4[24,2:4]<-round(accuracy(pr.f.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC4[25,1]<-"ARIMA recursivo bagged" 
AC5[25,1]<-"2"
AC4[25,2:4]<-round(accuracy(pr.rec.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[26,1]<-"ARIMAX recursivo bagged" 
AC5[26,1]<-"2"
AC4[26,2:4]<-round(accuracy(pr.rec.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[27,1]<-"ETS recursivo bagged" 
AC5[27,1]<-"2"
AC4[27,2:4]<-round(accuracy(pr.rec.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[28,1]<- "VAR recursivo bagged" 
AC5[28,1]<-"2"
AC4[28,2:4]<-round(accuracy(pr.rec.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[29,1]<- "FAVAR recursivo bagged"
AC5[29,1]<-"2"
AC4[29,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC4[30,1]<-"ARIMA rolling bagged" 
AC5[30,1]<-"2"
AC4[30,2:4]<-round(accuracy(pr.rol.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[31,1]<-"ARIMAX rolling bagged" 
AC5[31,1]<-"2"
AC4[31,2:4]<-round(accuracy(pr.rol.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[32,1]<-"ETS rollingbagged" 
AC5[32,1]<-"2"
AC4[32,2:4]<-round(accuracy(pr.rol.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[33,1]<- "VAR rolling bagged" 
AC5[33,1]<-"2"
AC4[33,2:4]<-round(accuracy(pr.rol.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[34,1]<- "FAVAR rolling bagged"
AC5[34,1]<-"2"
AC4[34,2:4]<-round(accuracy(pr.rol.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Exportamos.

# Para h=1

colnames(AC) <- c("Modelo", "RMSE", "MAE", "MAPE")
stargazer(AC[1:nrow(AC),], type = "latex")

# Para h=2

colnames(AC1) <- c("Modelo", "RMSE", "MAE", "MAPE")
AC1[1,1:ncol(AC1)] <- AC[1,1:ncol(AC)]
stargazer(AC1[1:nrow(AC1),], type = "latex")

# Para h=7

colnames(AC4) <- c("Modelo", "RMSE", "MAE", "MAPE")
AC4[1,1:ncol(AC4)] <- AC[1,1:ncol(AC)]
stargazer(AC4[1:nrow(AC4),], type = "latex")

#### Test de Giacomini Rossi ####

# Para realizar este test, vamos a plantear las funciones de 
# pérdida cuadráticas. 

### h=1 

# Esquema fijo.

error.bench.sq <- error.bench^2 

error.arima.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,1])^2

error.arimax.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,2])^2

error.ets.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,3])^2

error.adl.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,4])^2

error.var.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,5])^2

error.favar.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,6])^2

# Esquema recursivo.

error.arima.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,1])^2

error.arimax.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,2])^2

error.ets.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,3])^2

error.adl.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,4])^2

error.var.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,5])^2

error.favar.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,6])^2

# Esquema rolling.

error.arima.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,1])^2

error.arimax.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,2])^2

error.ets.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,3])^2

error.adl.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,4])^2

error.var.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,5])^2

error.favar.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,6])^2

# Bagging 

# Esquema fijo bagged.

error.arima.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,1])^2

error.arimax.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,2])^2

error.ets.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,3])^2

error.var.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,4])^2

error.favar.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,5])^2

# Esquema recursivo bagged.

error.arima.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,1])^2

error.arimax.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,2])^2

error.ets.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,3])^2

error.var.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,4])^2

error.favar.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,5])^2

# Esquema rolling bagged.

error.arima.rol.h1.b.sq <-  (out.of.sample[,2]-pr.rol.h1.b[,1])^2

error.arimax.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,2])^2

error.ets.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,3])^2

error.var.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,4])^2

error.favar.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,5])^2

### h=2 

error.arima.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,1])^{2}

error.arimax.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,2])^{2}

error.ets.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,3])^{2}

error.adl.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,4])^{2}

error.var.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,5])^{2}

error.favar.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,6])^{2}

# Esquema recursivo.

error.arima.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,1])^{2}

error.arimax.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,2])^{2}

error.ets.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,3])^{2}

error.adl.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,4])^{2}

error.var.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,5])^{2}

error.favar.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,6])^{2}

# Esquema rolling.

error.arima.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,1])^{2}

error.arimax.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,2])^{2}

error.ets.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,3])^{2}

error.adl.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,4])^{2}

error.var.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,5])^{2}

error.favar.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,6])^{2}

# Bagging. 

# Esquema fijo bagged.

error.arima.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,1])^{2}

error.arimax.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,2])^{2}

error.ets.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,3])^{2}

error.var.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,4])^{2}

error.favar.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,5])^{2}

# Esquema recursivo bagged.

error.arima.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,1])^{2}

error.arimax.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,2])^{2}

error.ets.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,3])^{2}

error.var.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,4])^{2}

error.favar.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,5])^{2}

# Esquema rolling.

error.arima.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,1])^{2}

error.arimax.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,2])^{2}

error.ets.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,3])^{2}

error.var.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,4])^{2}

error.favar.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,5])^{2}

### h=7

error.arima.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,1])^2

error.arimax.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,2])^2

error.ets.f.h7.sq<- (out.of.sample[7:58,2]-pr.f.h7[,3])^2

error.adl.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,4])^2

error.var.f.h7.sq <-  (out.of.sample[7:58,2]-pr.f.h7[,5])^2

error.favar.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,6])^2

# Esquema recursivo.

error.arima.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,1])^2

error.arimax.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,2])^2

error.ets.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,3])^2

error.adl.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,4])^2

error.var.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,5])^2

error.favar.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,6])^2

# Esquema rolling. 

error.arima.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,1])^2

error.arimax.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,2])^2

error.ets.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,3])^2

error.adl.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,4])^2

error.var.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,5])^2

error.favar.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,6])^2

# Bagging 

# Esquema fijo bagged.

error.arima.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,1])^2

error.arimax.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,2])^2

error.ets.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,3])^2

error.var.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,5])^2

error.favar.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,4])^2

# Esquema recursivo bagged.

error.arima.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,1])^2

error.arimax.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,2])^2

error.ets.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,3])^2

error.var.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,5])^2

error.favar.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,4])^2

# Esquema rolling bagged.

error.arima.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,1])^2

error.arimax.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,2])^2

error.ets.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,3])^2

error.var.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,5])^2

error.favar.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,4])^2

# Ahora graficamos.

library(murphydiagram)

# Pronósticos h=1 fijo.

gr1 <- fluctuation_test(error.arima.f.h1.sq, error.bench.sq, mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h1.sq, error.bench.sq, mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h1.sq, error.bench.sq, mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.f.h1.sq, error.bench.sq, mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.f.h1.sq, error.bench.sq, mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h1.sq, error.bench.sq, mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo, un paso hacia adelante",
       caption = "Fuente: elaboración propia")

# Pronósticos h=2 fijo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.f.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.f.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo, dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=7 fijo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.f.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.f.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo, siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=1 recursivo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h1.sq, error.bench.sq, mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h1.sq, error.bench.sq, mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h1.sq, error.bench.sq, mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rec.h1.sq, error.bench.sq, mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rec.h1.sq, error.bench.sq, mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h1.sq, error.bench.sq, mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))


ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo, un paso hacia adelante",
       caption = "Fuente: elaboración propia")



# Pronósticos h=2 recursivo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rec.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rec.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo, dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=7 recursivo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rec.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rec.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo, siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=1 rolling.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h1.sq, error.bench.sq, mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h1.sq, error.bench.sq, mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h1.sq, error.bench.sq, mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rol.h1.sq, error.bench.sq, mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rol.h1.sq, error.bench.sq, mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h1.sq, error.bench.sq, mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'rolling', un paso hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=2 rolling.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rol.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rol.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h2.sq, error.bench.sq[2:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'rolling', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=7 rolling.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rol.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rol.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h7.sq, error.bench.sq[7:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,53) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'rolling;, siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")



# Pronosticos h=1 fijo bagged.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.f.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[5],
                                colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo 'bagged', un paso hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=2 fijo bagged.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.f.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo 'bagged', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=7 fijo bagged.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.f.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6])) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +                     
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo 'bagged', siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=1 recursivo bagged.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rec.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo 'bagged', un paso hacia adelante",
       caption = "Fuente: elaboración propia")



# Pronósticos h=2 recursivo.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo 'bagged', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=7 recursivo.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", 
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(4,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo 'bagged', siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")



# Pronósticos h=1 rolling.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rol.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h1.b.sq, error.bench.sq, mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'bagged rolling', un paso hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=2 rolling.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'bagged rolling', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")


# Pronósticos h=7 rolling.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.1)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'bagged rolling', siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")



#### GR Comparando ####

# A continuación, haremos el test de Giacomini-Rossi comparando a cada modelo con 
# su versión bagged. Graficamos por esquema-ventana.

# Fijos h = 1.

gr1c <- fluctuation_test(error.arima.f.h1.sq, error.arima.f.h1.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h1.sq, error.arimax.f.h1.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h1.sq, error.ets.f.h1.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h1.sq, error.var.f.h1.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h1.sq, error.favar.f.h1.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-6,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema fijo un paso adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")


# Fijos h = 2.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c, 
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.f.h2.sq, error.arima.f.h2.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h2.sq, error.arimax.f.h2.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h2.sq, error.ets.f.h2.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h2.sq, error.var.f.h2.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h2.sq, error.favar.f.h2.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema fijo dos pasos adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")


# Fijo con h=7.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.f.h7.sq, error.arima.f.h7.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h7.sq, error.arimax.f.h7.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h7.sq, error.ets.f.h7.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h7.sq, error.var.f.h7.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h7.sq, error.favar.f.h7.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,52) +
  ylim(-5,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema fijo siete pasos adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")


# Rolling h = 1.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c, 
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rol.h1.sq, error.arima.rol.h1.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rol.h1.sq, error.arimax.rol.h1.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rol.h1.sq, error.ets.rol.h1.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rol.h1.sq, error.var.rol.h1.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rol.h1.sq, error.favar.rol.h1.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-6,4) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema 'rolling' un paso adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")



# Rolling h = 2.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.f.h2.sq, error.arima.f.h2.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h2.sq, error.arimax.f.h2.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h2.sq, error.ets.f.h2.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h2.sq, error.var.f.h2.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h2.sq, error.favar.f.h2.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema 'rolling' dos pasos adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")


# Rolling con h=7.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rol.h7.sq, error.arima.rol.h7.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rol.h7.sq, error.arimax.rol.h7.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rol.h7.sq, error.ets.rol.h7.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rol.h7.sq, error.var.rol.h7.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rol.h7.sq, error.favar.rol.h7.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,53) +
  ylim(-5,5.5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema 'rolling' siete pasos adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")



# Recursivo h = 1.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c, 
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rec.h1.sq, error.arima.rec.h1.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rec.h1.sq, error.arimax.rec.h1.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rec.h1.sq, error.ets.rec.h1.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rec.h1.sq, error.var.rec.h1.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rol.h1.sq, error.favar.rec.h1.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-6,4) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema recursivo un paso adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")


# Recursivo h = 2.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rec.h2.sq, error.arima.rec.h2.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rec.h2.sq, error.arimax.rec.h2.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rec.h2.sq, error.ets.rec.h2.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rec.h2.sq, error.var.rec.h2.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rec.h2.sq, error.favar.rec.h2.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema recursivo dos pasos adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")


# Recursivo con h=7

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rec.h7.sq, error.arima.rec.h7.b.sq , mu = 0.1)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rec.h7.sq, error.arimax.rec.h7.b.sq, mu = 0.1)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rec.h7.sq, error.ets.rec.h7.b.sq, mu = 0.1)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rec.h7.sq, error.var.rec.h7.b.sq, mu = 0.1)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rec.h7.sq, error.favar.rec.h7.b.sq, mu = 0.1)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(5,53) +
  ylim(-5,5.5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema recursivo siete pasos adelante",
       caption = "Nota: valores negativos reflejan un peor desempeño de la versión bagging.\
       Fuente: elaboración propia")


rm(list = ls())

