# Librerías necesarias
install.packages(c("tidyverse", "corrplot", "gridExtra", "GGally"))

library(tidyverse)   # Manipulación y visualización de datos
library(corrplot)    # Visualización de correlaciones
library(gridExtra)   # Visualización múltiple
library(GGally)

library(dplyr)
library(readr)

# Gráficos combinados para análisis multivariado

# 1. Carga de datos
#EL ARCHIVO DEBE ESTAR DENTRO DE TU PROPIA RUTA

datos <- read.csv("antidepressant_dataset.csv", sep = ";")

# Verificar estructura de los datos
head(datos)
str(datos)
getwd()

summary(datos)

#2. Limpieza inicial
# Eliminar columnas irrelevantes o redundantes

# Preservamos datos origiales en la variable datosrw
datos_raw <- datos
str(datos_raw)

# Seleccionamos la última visita por paciente
datos_last <- datos_raw %>%
  group_by(PATIENT) %>%
  slice_max(order_by = VISIT, n = 1) %>%
  ungroup() 
# Crear un dataset limpio:datos_last es el pso intermedio entre datosraw y datosclean en el que nos quedamos solo con la ultima visita por paciente.
datos_clean <- datos_last %>%
  select(
    PATIENT,
    GENDER,
    THERAPY,
    basval,
    change
  ) %>%
  rename(
    patient_id = PATIENT,
    gender     = GENDER,
    therapy    = THERAPY,
    baseline   = basval,
    improvement = change
  ) %>%
  mutate(
    therapy = factor(therapy),
    gender  = factor(gender)
  )
# Esto nos permite convertir las variables categóricas en factores
# Revisar el dataset final:
str(datos_clean)
summary(datos_clean)
head(datos_clean)
# Para comprobar que todo está bien cargado:
table(datos_raw$VISIT)


# Verificar valores NA
cat("Número de valores faltantes por columna:\n")
colSums(is.na(datos_clean))
sum(!complete.cases(datos_clean)) # ver s hay filas enteras con valores faltantes
lapply(datos_clean, unique) # ver si hay algún dato faltante que no está codifiado como NA


# Eliminar filas con valores NA si es necesario
datos <- na.omit(datos) # En nuestro caso no hace falta

# Verificar que los datos sean consistentes y ordenados, esto ya lo hemos hcho previamente
datos <- datos %>%
  mutate(across(where(is.character), as.factor))  # Convertir variables categóricas a factores si corresponde

# 3. Exploración básica
cat("Dimensiones del dataset:\n")
dim(datos_clean)  # Número de filas y columnas

cat("Resumen de las variables:\n")
summary(datos_clean)

# cat("Número de valores únicos por columna:\n")
# sapply(datos, function(x) length(unique(x)))

# 4.Comenzamos con la parte estadística del EDA:
# Histogramas:
# Histograma de la distribución de gravedad basal
hist(datos_clean$baseline,
     main = "Distribución de la gravedad basal (HAM-D)",
     xlab = "Puntuación basal",
     ylab = "Número de pacientes",
     col = "lightblue",
     border = "white")

# Histograma de la distribución de la mejora 
hist(datos_clean$improvement,
     main = "Distribución de la mejora final (HAM-D)",
     xlab = "Cambio total",
     ylab = "Número de pacientes",
     col = "lightgreen",
     border = "white")
# Boxplot según la gravedad inicial:
# Antes de hacer el boxplot generamos la variable severity grupo a partir de la 
# variable baseline, par dividir a nuesros pacientes en 3 grupos en función a la
# severidad de su depresión al inicio del estudio

datos_clean$severity_group <- cut(datos_clean$baseline,
                                  breaks = c(-Inf, 17, 24, Inf),
                                  labels = c("Leve", "Moderada", "Severa"))
severity_group


boxplot(improvement ~ severity_group,
        data = datos_clean,
        main = "Mejora según gravedad inicial",
        xlab = "Gravedad inicial",
        ylab = "Mejora (HAM-D)",
        col = c("#FFC1C1", "#FFDD99", "#A8D5BA"))
# Boxplot de mejora según tratamiento
boxplot(improvement ~ therapy,
        data = datos_clean,
        main = "Mejora por grupo de tratamiento",
        xlab = "Grupo",
        ylab = "Mejora (HAM-D)",
        col = c("lightpink", "lightblue"))

# Scatterplot baseline vs improvement
plot(datos_clean$baseline, datos_clean$improvement,
     main = "Relación entre gravedad basal y mejora",
     xlab = "Gravedad inicial (baseline)",
     ylab = "Mejora (HAM-D)",
     pch = 19, col = "darkgray")

abline(lm(improvement ~ baseline, data = datos_clean),
       col = "red", lwd = 2)

# Correlación entre baseline e improvement
cor(datos_clean$baseline, datos_clean$improvement)

# Scatter plot para el grupo DRUG
drug_data <- subset(datos_clean, therapy == "DRUG")

plot(drug_data$baseline, drug_data$improvement,
     main = "Relación gravedad-mejora en el grupo DRUG",
     xlab = "Gravedad inicial (baseline)",
     ylab = "Mejora (HAM-D)",
     pch = 19, col = "red")
placebo_data <- subset(datos_clean, therapy == "PLACEBO")
abline(lm(improvement ~ baseline, data = drug_data),
       col = "darkred", lwd = 2)

# Scatter plot para el grupo PLACEBO
plot(placebo_data$baseline, placebo_data$improvement,
     main = "Relación gravedad-mejora en el grupo PLACEBO",
     xlab = "Gravedad inicial (baseline)",
     ylab = "Mejora (HAM-D)",
     pch = 19, col = "blue")

abline(lm(improvement ~ baseline, data = placebo_data),
       col = "darkblue", lwd = 2)


abline(lm(improvement ~ baseline, data = drug_data),
       col = "darkred", lwd = 2)

# Boxplot combinado severidad inicial × tratamiento
boxplot(improvement ~ severity_group + therapy,
        data = datos_clean,
        las = 2,
        main = "Mejora por gravedad inicial y tratamiento",
        ylab = "Mejora (HAM-D)")


boxplot(datos_clean$baseline,
        main = "Detección de outliers en gravedad basal",
        ylab = "Puntuación HAM-D basal")

# Boxplot general de improvement (para ver outliers)
boxplot(datos_clean$improvement,
        main = "Detección de outliers en mejora",
        ylab = "Cambio total (HAM-D)")
# Tablas de frecuencias por categoría (opcional pero útil)
# Severidad:
table(datos_clean$severity_group)
# Tratamiento:
table(datos_clean$therapy)

# Matriz de correlación:con las variables originales de nuestro dataset
# incluimos reldays y visit
# Recuperar variables numéricas originales
numericas <- datos_last[, c("basval", "change", "RELDAYS", "VISIT")]

# Renombrar para mayor claridad
colnames(numericas) <- c("baseline", "improvement", "reldays", "visit")
correlaciones <- cor(numericas)
correlaciones
install.packages("corrplot")
library(corrplot)

corrplot(correlaciones,
         method = "circle",
         type = "upper",
         tl.cex = 0.9,
         tl.col = "black",
         addCoef.col = "black",     # añade los números en negro
         number.cex = 0.8,
         col = colorRampPalette(c("#e6f2ff", "#99ccff", "#3399ff", "#0066cc"))(200),
         main = "Matriz de correlación entre variables numéricas")

# 5.COMENZAMOS CON EL MODELO:

# 5.1.T-test: DRUG vs PLACEBO, Sirve para ver si el fármaco funciona mejor el placebo
t.test(improvement ~ therapy, data = datos_clean)

# 5.2.Modelo de regresión lineal básico:
# Predice la mejora en cada paciente solo basándose en el estadío inicial de los pacientes
modelo1 <- lm(improvement ~ baseline, data = datos_clean)
summary(modelo1)

# 5.3.Modelo lineal con interacción (modelo final del trabajo)
#Modelo que responde a mi pregunta, si la interacción es significaiva, el fármaco funciona mejor en pacientes más graves.
modelo2 <- lm(improvement ~ baseline * therapy, data = datos_clean)
summary(modelo2)

# 5.4.Comparación de modelos (opcional pero recomendable)
anova(modelo1, modelo2)
# Si sale significativo, el modelo2 explica más variabilidad.


# 6.VERSIÓN DE GRÁFICOS MEJORADA PARA EL PÓSTER:

library(ggplot2)
library(dplyr)

#Histograma
ggplot(datos_clean, aes(baseline)) +
  geom_histogram(binwidth = 3, fill = "#E4D4FF", color = "white", alpha = 0.95) +
  labs(
    title = "Distribución de la severidad inicial (HAM-D)",
    x = "Puntuación basal",
    y = "Número de pacientes"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#4A3B70"),
    axis.title = element_text(size = 16, color = "#4A3B70"),
    axis.text = element_text(size = 14)
  )
ggsave("histograma_baseline.png", width = 8, height = 6, dpi = 300)



# Boxplot drug y placebo
ggplot(datos_clean, aes(x = therapy, y = improvement, fill = therapy)) +
  geom_boxplot(alpha = 0.9, color = "black", size = 1.2) +
  scale_fill_manual(values = c(
    "DRUG" = "#6A1B9A",     # morado oscuro
    "PLACEBO" = "#E1BEE7"   # lila pastel claro
  )) +
  labs(
    title = "Mejora clínica por tratamiento",
    x = "Tratamiento",
    y = "Mejora (HAM-D)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#4A3B70"),
    axis.title = element_text(color = "#4A3B70"),
    legend.position = "none"
  )


ggsave("boxplot_drug_placebo.png", width = 8, height = 6, dpi = 300)


# Scatter plot
ggplot(
  datos_clean %>% filter(therapy == "DRUG"),
  aes(x = baseline, y = improvement)
) +
  geom_point(color = "#A679D2", alpha = 0.5, size = 3) +
  geom_smooth(method = "lm", color = "#6A3FBF", size = 1.7) +
  labs(
    title = "Relación gravedad–mejora (grupo DRUG)",
    x = "Gravedad inicial (baseline)",
    y = "Mejora (HAM-D)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#4A3B70")
  )
ggsave("scatter_drug.png", width = 8, height = 6, dpi = 300)


#Boxplot severidad y tratamiento
ggplot(datos_clean, aes(
  x = interaction(severity_group, therapy),
  y = improvement,
  fill = therapy
)) +
  geom_boxplot(alpha = 0.9, color = "black", size = 1.2) +
  scale_fill_manual(values = c(
    "DRUG" = "#6A1B9A",     # morado oscuro
    "PLACEBO" = "#E1BEE7"   # lila pastel claro
  )) +
  labs(
    title = "Mejora por severidad inicial y tratamiento",
    x = "Severidad × Tratamiento",
    y = "Mejora (HAM-D)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#4A3B70"),
    axis.text.x = element_text(size = 12, angle = 20, hjust = 1)
  )

ggsave("boxplot_severidad_tratamiento.png", width = 10, height = 6, dpi = 300)

getwd()

# 7. Diagnóstico del modelo, ver si cumple las condiciones de Gauss Markov:

par(mfrow = c(2, 2)) 
plot(modelo2)
par(mfrow = c(1, 1))
# Con esta función podemos verificar si nuestro modelo cumple con las dos primeras condiciones
# Independencia de erorres (test de Durvin-Watson), permite verificar la tercera condición
library(lmtest)
dwtest(modelo2)
#Interpretación de DW, nos interesa el valor de DW = 1.5516, p-value = 0.001617 ????
# DW ≈ 2 → los errores son independientes (OK para Gauss-Markov), en nuestro caso el DW=
# DW < 1.5 → autocorrelación de los errores positiva (problema), no se cumple la condición
# DW > 2.5 → autocorrelación negativa (raro, pero también problema)


# 4. Estadística descriptiva para variables numéricas
numericas <- select(datos, where(is.numeric))
cat("Estadísticas descriptivas para variables numéricas:\n")
summary(numericas)

# 5. Visualización de distribuciones
# Histogramas para todas las variables numéricas
par(mfrow = c(3, 3))  # Configurar múltiples gráficos
for (var in names(numericas)) {
  hist(numericas[[var]], main = paste("Histograma de", var),
       xlab = var, col = "skyblue", border = "black")
}

# Boxplots para detectar outliers
par(mfrow = c(1, 1))
boxplot(numericas, main = "Boxplots de Variables Numéricas", las = 2, col = "lightblue")

# 6. Correlaciones
# Visualización de la matriz de correlación, la realcion entre las variables nos las indica eltamaño del circulo.
# la diagonal siemore esta porque significa que nuestras variables se correlaconan perferctamente entre ellas.
# No vemos casi relación entre el resto de las variables poqrue la asociacion entre ellas será muy pequeña.
correlaciones <- cor(numericas)
corrplot(correlaciones, method = "circle", type = "upper", tl.cex = 0.7, tl.col = "black", main = "Matriz de Correlación")

# 7. Análisis univariante #trabajamos sobre variables cualitativas 
# Análisis de la variable objetivo, 
table(datos$Diagnostico_Alzheimer) #Nos permite contar el numero de pacientes con alzehimer(1) y sin alzehimer(0)
barplot(table(datos$Diagnostico_Alzheimer), main = "Distribución de la Variable Target",
        col = c("orange", "skyblue"), xlab = "Clase", ylab = "Frecuencia")

# 8. Análisis bivariante
#Para ver si existe relación entre la edad dl pacinte y su desempeño cognitivo(MOCA)
# Relación entre Edad y Puntuación MOCA estudiar relaciones entre todas las variables de interés
plot(datos$Edad, datos$Puntuacion_MOCA, main = "Edad vs Puntuación MOCA",
     xlab = "Edad", ylab = "Puntuación MOCA", col = "blue", pch = 16)
summary(datos$Edad)
colnames(datos)
# Comparación de Puntuación MMSE entre clases
boxplot( Puntuacion_TestEstandarizado_MMSE ~ Diagnostico_Alzheimer, data = datos, main = "Puntuación MMSE por Clase",
         xlab = "Clase", ylab = "Puntuación MMSE", col = c("orange", "skyblue"))

# 9. Análisis multivariante
# Pair plot para las variables principales
ggpairs(select(datos, Edad_Paciente, Puntuacion_TestEstandarizado_MMSE, 
               Puntuacion_BiomarcadorEstandarizado_Hipocampo, 
               Puntuacion_SubtestCognitivo_MOCA, Diagnostico_Alzheimer),
        aes(color = as.factor(Diagnostico_Alzheimer), alpha = 0.5)) 
#En el caso de las montañitas se está refiriendo a l distibucín que tiene cda variable d manera individual
#Los puntitos nos indican qu ecurre con una variable cuando una sube y otra baja o cuando ambas suben..
#Los numerios expresan si las variables están realconadas, si es 0, normalmente no están relcionadas, si es>0 normalment eestán realcionadas.

# 10. Observaciones
cat("Observaciones importantes:\n")
cat("- Verificar posibles outliers en variables como Edad y Puntuación MOCA.\n")
cat("- La correlación más fuerte encontrada es entre Volumen_Hipocampo y Nivel_Amyloide_Beta.\n")
cat("- La clase 'Target' está balanceada.\n")

