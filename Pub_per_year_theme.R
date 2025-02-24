
library(ggplot2)
#cargo tabla de publicaciones por año por tematica
library(readxl)
fig1 <- read_excel("publication_year.xlsx")
fig1$Año <- as.factor(fig1$Año)
View(fig1)

#lo convierto a formato largo con estructura de tematica en una sola columna + año + publiaciones
library(tidyr)
library(dplyr)

#convertir dataframe para modelar
datos_pub_year <- fig1 %>%
  pivot_longer(cols = c("Ecological dimension", "Social dimension", "Socioecological dimension"), # Nombres de columnas a convertir
               names_to = "Theme",  # Nueva columna que indicará la temática
               values_to = "Publications") %>% # Nueva columna con el número de publicaciones 
    rename(Year=Año)
  

# Cuantas publicaciones por tema por año hubo (no es acumulado del total)
datos_resumen <- datos_pub_year %>%
  group_by(Theme, Year) %>%  # Agrupamos por tema y año
  summarise(Total_Publications = sum(Publications)) %>%  # Sumamos las publicaciones por tema y año
  ungroup()  # Desagrupamos

#PUblicaciones por tematica
Theme_sum <- datos_resumen %>%
  group_by(Theme) %>%
  summarise(total_publications = sum(Total_Publications))



library(dplyr)
# Calcular publicaciones acumuladas por theme
datos_acumulados <- datos_pub_year %>%
  arrange(Theme, Year) %>%  # Ordenamos primero por temática y luego por año
  group_by(Theme) %>%
  mutate(Publications_cum = cumsum(Publications))  # Generamos la suma acumulada

# Aseguramos que 'Publications_cum' sea numérico
datos_acumulados$Publications_cum <- as.numeric(datos_acumulados$Publications_cum)


# Verificar las clases de las columnas
str(datos_acumulados)

# Asegurarnos de que 'Year' y 'Publications_cum' son numéricas
datos_acumulados$Year <- as.numeric(datos_acumulados$Year)
datos_acumulados$Publications_cum <- as.numeric(datos_acumulados$Publications_cum)


#Graficar la tendencia de publicaciones acumuladas
#ajuste exponencial
ggplot(datos_acumulados, aes(x = Year, y = Publications_cum, color = Theme)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_smooth(method = "nls", formula = y ~ a * exp(b * x), #ajuste de mínimos cuadrados no lineales (NLS) para la regresión exponencial.
              method.args = list(start = c(a = 1, b = 0.1)), se = FALSE, 
              linetype = "dashed") +  # Ajuste exponencial
  labs(title = "Tendencia acumulada de publicaciones por temática",
       x = "Año", y = "Publicaciones acumuladas") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(datos_acumulados$Year), max(datos_acumulados$Year), by = 1)) +  # Mejor control sobre el eje X
  scale_y_continuous(labels = scales::comma)  # Formato para las etiquetas del eje Y

# Ajustar el modelo exponencial y obtener los parámetros para cada temática
models <- datos_acumulados %>%
  group_by(Theme) %>%
  do({
    model <- tryCatch({
      nls(Publications_cum ~ a * exp(b * Year), data = ., start = list(a = 1, b = 0.1))
    }, error = function(e) NULL)
    
    if (!is.null(model)) {
      rsq <- 1 - sum(resid(model)^2) / sum((.$Publications_cum - mean(.$Publications_cum))^2)
      coef <- coef(model)
      data.frame(a = coef[1], b = coef[2], R_squared = rsq)
    } else {
      data.frame(a = NA, b = NA, R_squared = NA)
    }
  })

# Ver los resultados (coeficientes y R²)
models #Ajustamos el modelo exponencial con la fórmula y=a.exp(b⋅x).




#A PARTIR DE ACA ES LO QUE IMPORTA



# Cuantas publicaciones por tema por año hubo (no es acumulado del total)
datos_resumen <- datos_pub_year %>%
  group_by(Theme, Year) %>%  # Agrupamos por tema y año
  summarise(Total_Publications = sum(Publications)) %>%  # Sumamos las publicaciones por tema y año
  ungroup()  # Desagrupamos

# Asegurarnos de que 'Year' y 'Publications_cum' son numéricas
datos_resumen$Year <- as.numeric(datos_resumen$Year)



##EXPONENCIAL
# Graficar la cantidad de publicaciones por tema por año
ggplot(datos_resumen, aes(x = Year, y = Total_Publications, color = Theme)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_smooth(method = "nls", formula = y ~ a * exp(b * x),  # Ajuste exponencial
              method.args = list(start = c(a = 1, b = 0.1)), se = FALSE, 
              linetype = "dashed") +  
  labs(title = "Tendencia de publicaciones por temática",
       x = "Año", y = "Cantidad de publicaciones") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(datos_resumen$Year), max(datos_resumen$Year), by = 1)) +  
  scale_y_continuous(labels = scales::comma)  # Formato para las etiquetas del eje Y

#LINEAL
ggplot(datos_resumen, aes(x = Year, y = Total_Publications, color = Theme)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed") +  # Ajuste lineal
  labs(title = "Tendencia de publicaciones por temática",
       x = "Año", y = "Cantidad de publicaciones") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(datos_resumen$Year), max(datos_resumen$Year), by = 1)) +  
  scale_y_continuous(labels = scales::comma)  # Formato para las etiquetas del eje Y




#Ajustar los modelos con sus correspondientes lineas de tendencia

eco_mod <- nls(Total_Publications ~ a * exp(b * Year), 
               data = datos_resumen %>% filter(Theme == "Ecological dimension"),
               start = list(a = 1, b = 0.1))

soc_mod1 <- lm(Total_Publications ~ Year, 
              data = datos_resumen %>% filter(Theme == "Social dimension"))


socioeco_mod1 <- lm(Total_Publications ~ Year, 
                   data = datos_resumen %>% filter(Theme == "Socioecological dimension"))


#Graficar con tendencias distnintas
# Graficar las publicaciones con las diferentes líneas de tendencia
ggplot(datos_resumen, aes(x = Year, y = Total_Publications, color = Theme)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_smooth(method = "nls", formula = y ~ a * exp(b * x), 
              method.args = list(start = c(a = 1, b = 0.1)), se = FALSE, 
              linetype = "dashed", data = datos_resumen %>% filter(Theme == "Ecological dimension")) +  # Ajuste exponencial para Ecological
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "solid", 
              data = datos_resumen %>% filter(Theme == "Social dimension")) +  # Ajuste lineal para Social
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "solid", 
              data = datos_resumen %>% filter(Theme == "Socioecological dimension")) +  # Ajuste lineal para Socio-ecological
  labs(title = "Tendencia de publicaciones por temática",
       x = "Año", y = "Cantidad de publicaciones") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(datos_resumen$Year), max(datos_resumen$Year), by = 1)) +  
  scale_y_continuous(labels = scales::comma)  # Formato para las etiquetas del eje Y




######### cambiarmos a ajuste polinomico

# Ajustar modelos para cada temática
eco_mod <- nls(Total_Publications ~ a * exp(b * Year), 
               data = datos_resumen %>% filter(Theme == "Ecological dimension"),
               start = list(a = 1, b = 0.1))

# Ajustes polinómicos para los otros temas
soc_mod <- lm(Total_Publications ~ poly(Year, 3),  # Polinómica de grado 3 para Social
              data = datos_resumen %>% filter(Theme == "Social dimension"))

socioeco_mod <- lm(Total_Publications ~ poly(Year, 2),  # Polinómica de grado 2 para Socio-ecological
                   data = datos_resumen %>% filter(Theme == "Socioecological dimension"))

# Graficar las publicaciones con las diferentes líneas de tendencia
ggplot(datos_resumen, aes(x = Year, y = Total_Publications, color = Theme)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_smooth(method = "nls", formula = y ~ a * exp(b * x), 
              method.args = list(start = c(a = 1, b = 0.1)), se = FALSE, 
              linetype = "dashed", data = datos_resumen %>% filter(Theme == "Ecological dimension")) +  # Ajuste exponencial para Ecological
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, linetype = "dashed", 
              data = datos_resumen %>% filter(Theme == "Social dimension")) +  # Polinómica grado 3 para Social
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linetype = "dashed", 
              data = datos_resumen %>% filter(Theme == "Socioecological dimension")) +  # Polinómica grado 2 para Socioecologico
  #geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", 
              #data = datos_resumen %>% filter(Theme == "Socioecological dimension")) +  # Ajuste lineal para Socio-ecological
  labs(title = "Tendencia de publicaciones por temática",
       x = "Año", y = "Cantidad de publicaciones") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(datos_resumen$Year), max(datos_resumen$Year), by = 1)) +  
  scale_y_continuous(labels = scales::comma)  # Formato para las etiquetas del eje Y

#Veo como ajustan las lineas de tendencia

# Extract the formula and R² for the exponential model (eco_mod)
eco_formula <- as.character(formula(eco_mod))
eco_r2 <- 1 - sum(residuals(eco_mod)^2) / sum((datos_resumen$Total_Publications - mean(datos_resumen$Total_Publications))^2)

# Extract the formula and R² for the polynomial models
soc_formula <- as.character(formula(soc_mod))
soc_r2 <- summary(soc_mod)$r.squared

# Extraer la fórmula y R² para el modelo de Social dimension (soc_mod1)
soc_mod1_formula <- as.character(formula(soc_mod1))
soc_mod1_r2 <- summary(soc_mod1)$r.squared

# Extraer la fórmula y R² para el modelo de Socioecological dimension (socioeco_mod1)
socioeco_mod1_formula <- as.character(formula(socioeco_mod1))
socioeco_mod1_r2 <- summary(socioeco_mod1)$r.squared

# Extract the formula and R² for the polynomial models
socioeco_formula <- as.character(formula(socioeco_mod))
socioeco_r2 <- summary(socioeco_mod)$r.squared

# Print the formulas and R²
cat("Ecological model (Exponential):\n")
cat("Formula: ", eco_formula, "\n")
cat("R²: ", eco_r2, "\n\n")

cat("Social model (Polynomial degree 3):\n")
cat("Formula: ", soc_formula, "\n")
cat("R²: ", soc_r2, "\n\n")

cat("Social dimension model (Linear):\n")
cat("Formula: ", soc_mod1_formula, "\n")
cat("R²: ", soc_mod1_r2, "\n\n")


cat("Socio-ecological model (Polynomial degree 2):\n")
cat("Formula: ", socioeco_formula, "\n")
cat("R²: ", socioeco_r2, "\n")

cat("Socioecological dimension model (Linear):\n")
cat("Formula: ", socioeco_mod1_formula, "\n")
cat("R²: ", socioeco_mod1_r2, "\n")



#ver diferencias significativas

#si consideras que las publicaciones siguen una distribución sesgada o que el número de publicaciones crece de manera exponencial (o en una escala multiplicativa), aplicar la transformación logarítmica es una buena opción.
#hay años con pocos registros y otros con muchos), el uso de un logaritmo puede ayudar a normalizar los datos y hacer que sigan una distribución más normal.

# Aplicar transformación logarítmica
datos_resumen$Log_Publications <- log(datos_resumen$Total_Publications + 1)  # +1 para evitar log(0)

# Realizar ANCOVA con Log_Publications como variable dependiente
#El ANCOVA es útil cuando deseas comparar las tendencias de las publicaciones a lo largo del tiempo entre diferentes grupos (temáticas) mientras controlas el efecto del año como covariable.
#En un ANCOVA, el año sería una covariable continua, y las temáticas serían un factor categórico. Esto te permitirá evaluar si las tendencias de las publicaciones son significativamente diferentes entre los grupos temáticos, controlando el efecto del tiempo.

# Asegurarse de que 'Theme' sea un factor
datos_resumen$Theme <- factor(datos_resumen$Theme)

# #Año como covariable y Tema como factor
ancova_model <- aov(Log_Publications ~ Year * Theme, data = datos_resumen)
summary(ancova_model)


library(emmeans)


# Realizar comparaciones post-hoc con emmeans
emmeans_results <- emmeans(ancova_model, pairwise ~ Theme)

# Ver los resultados
summary(emmeans_results)


#GRAFICO FINAAAAAAAAAAAAL --------------------------





