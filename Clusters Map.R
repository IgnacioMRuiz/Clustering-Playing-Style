#Librerías necesarias
library(readxl)
library(dplyr)
library(umap)
library(mclust)
library(ggplot2)
library(ggrepel)
library(plotly)


# Creamos un data frame con los resultados de UMAP y los labels de clusters
umap_data <- data.frame(UMAP1 = umap_model$layout[,1], UMAP2 = umap_model$layout[,2], Cluster = df$Cluster, Team = df$Team)


# Definimos la paleta de colores personalizada para los clusters
cluster_colors <- c("#37758C", "#C69E54", "#BC5B61", "#9FABB5", "#5B9B8A", "#A99F54" )

# Ajustamos el gráfico para añadir las áreas de densidad difuminadas y los puntos
ggplot(umap_data, aes(x=UMAP1, y=UMAP2)) +
  # Añadir polígonos de densidad
  stat_density2d(
    aes(fill=factor(Cluster), alpha=..level..), 
    geom="polygon", 
    color="transparent", 
    bins=20, 
    adjust=1
  ) +
  scale_fill_manual(values=cluster_colors) +  # Usar la paleta de colores personalizada para las áreas de densidad
  scale_alpha(range = c(0.2, 0.5), guide = FALSE) +  # Ajustar la transparencia y ocultar la guía de alpha
  guides(fill = FALSE, alpha = FALSE) +  # Eliminar la leyenda de fill y alpha
  geom_point(aes(color=factor(Cluster)), alpha=1) +  # Puntos sólidos sobre las áreas de densidad
  geom_point(color="#36526A", size=2, shape=1, stroke=0.5) +  # Borde negro alrededor de los puntos
  scale_color_manual(values=cluster_colors) +  # Usar la paleta de colores personalizada para los puntos
  theme_minimal(base_family = "Helvetica") +
  labs(color='Cluster') +
  theme(
    plot.title = element_text(color="#36526A", size=20, face="bold", hjust = 0.5),
    plot.background = element_rect(fill="white", color=NA),
    panel.background = element_rect(fill="white", color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.9),
    legend.justification = c(1, 0.5),
    legend.direction = "vertical",
    legend.box.just = "right",
    legend.box.margin = margin(t = 0, r = 10, b = 0, l = 10, unit = "pt"),
    legend.margin = margin(t = 0, unit = "pt"),
    legend.title = element_text(color="#36526A"),
    legend.text = element_text(color="#36526A"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(color="#36526A")
  ) +
  geom_text_repel(
    aes(label=Team),
    box.padding = 0.35,
    point.padding = 0.5,
    size = 2,
    color="#36526A"
  ) +
  #ggtitle('Cluster estilos de juego') +
  # Ajustar los límites del gráfico si es necesario
  xlim(min(umap_data$UMAP1) - 0.5, max(umap_data$UMAP1) + 0.5) +
  ylim(min(umap_data$UMAP2) - 0.5, max(umap_data$UMAP2) + 0.5)


# Agrupar los equipos por cluster y listar los nombres
teams_by_cluster <- df %>%
  select(Team, Cluster) %>%
  group_by(Cluster) %>%
  summarise(Teams = list(Team), .groups = 'drop')

# Iterar sobre cada cluster y imprimir los nombres de los equipos
for (i in 1:nrow(teams_by_cluster)) {
  cat("Cluster", teams_by_cluster$Cluster[i], ":\n")
  cat(paste(teams_by_cluster$Teams[[i]], collapse = ", "), "\n\n")
}