# ¿Como se siente el presidente?

En este repositorio se encuentran los archivos necesarios para replicar el trabajo "¿Cómo se siente el presidente?" de Tomás Pacheco y Abigail Riquelme.

## Archivos

El repostorio cuenta con diversos archivos y carpetas. El scrip "1ManejoDatos.R" sirve para descargar la base de datos, darle el formato deseado y realizar los tests de raíz unitaria. Con el archivo "2EstimacionesInSample.R" se pueden replicar las estimaciones in-sample con los datos. Luego, con "3Pronósticos.R" se pueden replicar los pronósticos. Dado que los programas tardan un tiempo considerable en correr, los pronósticos ya se encuentran disponibles en la carpeta "Forecasts" en formato .csv. Por último, con el archivo "4Accuracy.R" se pueden computar las métricas de desempeño, así como los tests de Diebold-Mariano y Giacomini-Rossi.

Por último está la carpeta "Data_Prep" en la que se encuentran todos los archivos para replicar la construcción de la base de datos. En esta se encuentran los datos por separado y el do file "Data_Merging" exporta la base de datos completa. Es posible replicar la base de datos de COVID-19 en Argentina con el do file "CleaningCovid". Los datos ya limpios se encuentran en "CovidAndVaccines.dta" ya que el proceso de descarga de datos lleva mucho tiempo.

Ante cualquier duda o error escribir a tpacheco@udesa.edu.ar
