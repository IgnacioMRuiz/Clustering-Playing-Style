library(readxl)
library(dplyr)
library(umap)
library(mclust)
library(ggplot2)
library(ggrepel)
library(plotly)


# P1 Cargar los Datos

file_path <- "/Users/nacho/Desktop/Proyectos personales/Clusterización Proyecto/Ligas Españolas/Spain Teams Stats.xlsx"
df<- read_excel(file_path)


# P2 Crear Nuevas Métricas para el Modelo

df <- df %>%
  mutate(
    AttackingAbility = (`Penalty area entries` / Passes) * 100,
    OffsideRate = (Offsides / Passes) * 100,
    BuildupRate = (`Passes to final third` / Passes) * 100,
    CrossingEfficiency = (Crosses / `Penalty area entries`) * 100,
    DeepPassesEfficiency = (`Deep completed passes` / `Penalty area entries`) * 100,
    PassingAccuracy = (`Passes accurate` / Passes) * 100,
    LengthPassesRate = (`Long passes` / Passes) * 100,
    Directness = (`Average pass length` / Passes) * 100,
    Switches = (`Lateral passes` / Passes) * 100,
    DuelsRate = (`Aerial Duels won` / `Aerial duels`) * 100,
    Progressiveness = (`Progressive passes` / `Passes to final third`) * 100,
    Dribbling = (`Offensive duels` / Passes) * 100,
    LostBalls = (Losses / Passes) * 100,
    ClosingDown = ((`Sliding tackles` + Interceptions + Clearances) / `Defensive duels`) * 100,
    Fouling = ((Fouls + `Yellow cards` + `Red cards`) / `Defensive duels`) * 100,
    RecoveryHigh = (`% Recoveries High` / Passes) * 100, # Asegúrate de que `Recoveries High` corresponde al nombre de la columna real
    MatchTempo = `Match tempo`,
    APPP = `Average passes per possession`
  )



# Reemplazar NaN con 0 en las columnas específicas
df <- df %>%
  mutate(
    AttackingAbility = replace(AttackingAbility, is.nan(AttackingAbility), 0),
    OffsideRate = replace(OffsideRate, is.nan(OffsideRate), 0),
    BuildupRate = replace(BuildupRate, is.nan(BuildupRate), 0),
    CrossingEfficiency = replace(CrossingEfficiency, is.nan(CrossingEfficiency), 0),
    DeepPassesEfficiency = replace(DeepPassesEfficiency, is.nan(DeepPassesEfficiency), 0),
    PassingAccuracy = replace(PassingAccuracy, is.nan(PassingAccuracy), 0),
    LengthPassesRate = replace(LengthPassesRate, is.nan(LengthPassesRate), 0),
    Directness = replace(Directness, is.nan(Directness), 0),
    Switches = replace(Switches, is.nan(Switches), 0),
    DuelsRate = replace(DuelsRate, is.nan(DuelsRate), 0),
    Progressiveness = replace(Progressiveness, is.nan(Progressiveness), 0),
    Dribbling = replace(Dribbling, is.nan(Dribbling), 0),
    LostBalls = replace(LostBalls, is.nan(LostBalls), 0),
    ClosingDown = replace(ClosingDown, is.nan(ClosingDown), 0),
    Fouling = replace(Fouling, is.nan(Fouling), 0),
    RecoveryHigh = replace(RecoveryHigh, is.nan(RecoveryHigh), 0),
    MatchTempo = replace(MatchTempo, is.nan(MatchTempo), 0),
    APPP = replace(APPP, is.nan(APPP), 0)
  )


# P3 UMAP

set.seed(123) # Para reproducibilidad
umap_model <- umap(df %>% select(AttackingAbility, OffsideRate, BuildupRate, CrossingEfficiency, 
                                 DeepPassesEfficiency, PassingAccuracy, LengthPassesRate, Directness, 
                                 Switches,DuelsRate, Progressiveness, Dribbling, LostBalls,
                                 ClosingDown, Fouling, PPDA, RecoveryHigh, MatchTempo, APPP))


# P4 GMM Clustering

set.seed(123) # Para reproducibilidad
gmm_model <- Mclust(umap_model$layout, G = 5) # Ajustar el número de clusters si es necesario
df$Cluster <- gmm_model$classification


# P5 Visualización

# Creamos un data frame con los resultados de UMAP y los labels de clusters
umap_data <- data.frame(UMAP1 = umap_model$layout[,1], UMAP2 = umap_model$layout[,2], Cluster = df$Cluster, Team = df$Team)
# Definimos la paleta de colores personalizada para los clusters
cluster_colors <- c("#36758D", "#CCA356", "#BD5B61", "#9EABB5", "#5B9A8B")

# Ajustamos el gráfico para añadir las áreas de densidad difuminadas
ggplot(umap_data, aes(x=UMAP1, y=UMAP2)) +
  # Añadir polígonos de densidad
  stat_density2d(aes(fill=factor(Cluster), alpha=..level..), geom="polygon", color="transparent") +
  scale_fill_manual(values=cluster_colors) +  # Usar la paleta de colores personalizada
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
    legend.position = c(1, 0.9),
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
