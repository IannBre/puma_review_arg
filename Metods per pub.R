library(readxl)
methods <- read_excel("Metodologias Review.xlsx")

head(methods)

#transformar los datos a formato largo
library(tidyr)
library(dplyr)

methods_largos <- methods %>%
  pivot_longer(cols = 8:20,  # Ajusta según las columnas de metodologías
               names_to = "Metodología", 
               values_to = "Usó_Metodología") %>%
  replace_na(list(Usó_Metodología = 0))  # Reemplazar NA por 0

head(methods_largos)
View(methods_largos)


# Resumir los datos: contar la cantidad de trabajos que usaron cada metodología -----------------------
resumen_metodologias <- methods_largos %>%
  group_by(Metodología) %>%
  summarise(Total_Usaron = sum(Usó_Metodología)) 

# Ver el resumen
print(resumen_metodologias)
View(resumen_metodologias)


# Crear gráfico de barras ---------------------
library(ggplot2)

ggplot(resumen_metodologias, aes(x = Metodología, y = Total_Usaron)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Proporción de Trabajos por Metodología", 
       x = "Metodología", y = "Número de Trabajos") +
  theme_minimal()



################################### Estadistica ------------------------------------
resumen_metodologias <- resumen_metodologias[resumen_metodologias$Total_Usaron > 0, ]

### 1) Chicuadrado usa proporciones

# Crear vector con los totales de publicaciones por metodología
totales <- resumen_metodologias$Total_Usaron

# Etiquetas de metodologías
categorias <- resumen_metodologias$Metodología

# Aplicar Chi-cuadrado
chisq.test(totales)


# Total de publicaciones
n_total_publicaciones <- sum(totales)  

# Comparaciones post-hoc con Bonferroni
pairwise.prop.test(totales, rep(n_total_publicaciones, length(totales)), p.adjust.method = "bonferroni")




### GRAFICO PARA PRESENTAR
library(ggplot2)

# Crear un data frame con las etiquetas de significancia
significancia <- data.frame(
  Metodología = c("Observacional"),  # Agregar más metodologías si es necesario
  Total_Usaron = c(max(resumen_metodologias$Total_Usaron) + 5),  # Ubicación de la etiqueta
  label = c("***")  # Asteriscos según nivel de significancia
)

View(significancia)

# Gráfico con asteriscos
methods_plot <- ggplot(resumen_metodologias, aes(x = Metodología, y = Total_Usaron, fill = Metodología)) +
  geom_bar(stat = "identity") +
  geom_text(data = significancia, aes(x = Metodología, y = Total_Usaron, label = label), size = 6, color = "black") +
  theme_minimal() +
  labs(title = "",
       x = "Methodology",
       y = "Number of publications") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Exportar
# Guardar como PNG con 300 dpi
ggsave("grafico_metodologias.png", plot = methods_plot, width = 8, height = 6, dpi = 300)

# Guardar como TIFF con 300 dpi
ggsave("grafico_metodologias.tif", plot = methods_plot, width = 8, height = 6, dpi = 300)


