install.packages("igraph")
install.packages("ggraph")
install.packages("fastmap")
install.packages("ggplot2")

library(igraph)
library(ggraph)


# Crear la base de datos
data <- data.frame(
  Categoria = c("Puma", "Puma", "Puma", "Puma", "Puma", "Puma"),
  Categoria_relacionada = c("Puma", "Puma + Prey", "Puma + Carnivore/s", "Puma in Carnivore Assemblage", "Puma and Multispecies", "Other Species"),
  Porcentaje_relacionados = c(100, 50, 30, 10, 5, 3)
)

# Ver el dataframe
print(data)


# Crear el grafo
graph <- graph_from_data_frame(data, directed = TRUE)

# Visualización
puma_web <- ggraph(graph, layout = "kk") +  # Usamos el layout "kk" para una disposición más compacta El layout Kamada-Kawai se enfoca en minimizar la distancia de los nodos 
  geom_edge_link(aes(edge_width = Porcentaje_relacionados), alpha = 0.7) +
  geom_node_point(aes(color = Porcentaje_relacionados), size = ifelse(V(graph)$name == "Puma", 15, 5),  # Tamaño mayor para Puma
                  shape = 21, fill = "red", color = "black") +
  scale_color_viridis_c() +
  geom_node_text(aes(label = name), repel = TRUE, size = 4, nudge_x = -0.1, nudge_y = 0.2, max.overlaps = 10) +  # Ajustar el desplazamiento
  # Agregar un círculo alrededor de todos los nodos
  geom_polygon(data = data.frame(x = c(-2, 2, 2, 1), y = c(-1, -2, -.2, 2.6)), aes(x, y), fill = "blue", alpha = 0.1) +
    theme_void() +
  labs(title = "")
puma_web

ggsave("grafico_puma.png", plot = puma_web, width = 8, height = 6, dpi = 300)







