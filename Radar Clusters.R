library(readxl)
library(dplyr)
library(umap)
library(mclust)
library(ggplot2)
library(ggrepel)
library(plotly)
library(radarchart)
library(fmsb)

df_cluster_means <- df %>%
  group_by(Cluster) %>%
  summarise(
    AttackingAbility = mean(AttackingAbility, na.rm = TRUE),
    OffsideRate = mean(OffsideRate, na.rm = TRUE),
    BuildupRate = mean(BuildupRate, na.rm = TRUE),
    CrossingEfficiency = mean(CrossingEfficiency, na.rm = TRUE),
    DeepPassesEfficiency = mean(DeepPassesEfficiency, na.rm = TRUE),
    PassingAccuracy = mean(PassingAccuracy, na.rm = TRUE),
    LengthPassesRate = mean(LengthPassesRate, na.rm = TRUE),
    Directness = mean(Directness, na.rm = TRUE),
    Switches = mean(Switches, na.rm = TRUE),
    DuelsRate = mean(DuelsRate, na.rm = TRUE),
    Progressiveness = mean(Progressiveness, na.rm = TRUE),
    Dribbling = mean(Dribbling, na.rm = TRUE),
    LostBalls = mean(LostBalls, na.rm = TRUE),
    MatchTempo = mean(MatchTempo, na.rm = TRUE),
    APPP = mean(LostBalls, na.rm = TRUE),
    ClosingDown = mean(APPP, na.rm = TRUE),
    Fouling = mean(Fouling, na.rm = TRUE),
    PPDA = mean(PPDA, na.rm = TRUE),
    RecoveryHigh = mean(RecoveryHigh, na.rm = TRUE)
  )

# Normalizar los datos entre 0 y 10
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)) * 10)
}

# Aplicar la normalización, excluyendo la columna 'Cluster'
df_cluster_means_normalized <- as.data.frame(lapply(df_cluster_means[, -1], normalize))

# Añadir la columna 'Cluster' a los datos normalizados
df_cluster_means_normalized$Cluster <- df_cluster_means$Cluster

# Colores personalizados para el relleno y trazo interno de cada cluster
cluster_colors <- c("#36758D", "#E2B45F", "#BD5B61", "#9EABB5", "#5B9A8B", "#A99F54")

# Crear un gráfico de radar para cada cluster
for(i in 1:max(df_cluster_means$Cluster)){
  # Seleccionar los datos del cluster actual
  cluster_data <- df_cluster_means_normalized[df_cluster_means_normalized$Cluster == i, -ncol(df_cluster_means_normalized)]
  
  # Crear el dataframe para el gráfico de radar
  # Añadir una fila con los valores mínimos y máximos
  radar_data <- rbind(rep(0, ncol(cluster_data)), rep(10, ncol(cluster_data)), cluster_data)
  
  # Ajustar los nombres de las filas
  row.names(radar_data) <- c("min", "max", paste("Cluster", i))
  
  # Crear el gráfico de radar para el cluster actual
  radarchart(radar_data, 
             axisticks=seq(0, 10, by=1), 
             pcol=cluster_colors[i], 
             pfcol=adjustcolor(cluster_colors[i], alpha.f=0.8), # Opacidad al 90%
             plty=1, 
             plwd=2, 
             pty=32, # No plotting of points
             pdg=0, 
            #title=paste("Cluster", i), 
             cglcol="#36526A", 
             cglty=1, 
             axislabcol="#36526A", 
             vlcex=0.4)
  
  # Agregar un breve retraso para que las ventanas no se sobreescriban
  Sys.sleep(0.5)
}

# Normalizar las métricas individuales dentro de cada grupo global entre 0 y 10
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)) * 10)
}

# Aplicar normalización a cada métrica dentro de su grupo global
df_normalized <- df %>%
  mutate_at(vars(AttackingAbility, OffsideRate, BuildupRate, CrossingEfficiency, DeepPassesEfficiency), funs(normalize)) %>%
  mutate_at(vars(PassingAccuracy, LengthPassesRate, Directness, Switches, DuelsRate), funs(normalize)) %>%
  mutate_at(vars(Progressiveness, Dribbling, LostBalls, MatchTempo, APPP), funs(normalize)) %>%
  mutate_at(vars(ClosingDown, Fouling, PPDA, RecoveryHigh), funs(normalize))

# Calcular la puntuación promedio para cada grupo global por cluster
df_group_scores <- df_normalized %>%
  group_by(Cluster) %>%
  summarise(
    PossessionScore = mean(normalize(c(AttackingAbility, OffsideRate, BuildupRate, CrossingEfficiency, DeepPassesEfficiency)), na.rm = TRUE),
    PassingScore = mean(normalize(c(PassingAccuracy, LengthPassesRate, Directness, Switches, DuelsRate)), na.rm = TRUE),
    OffensiveScore = mean(normalize(c(Progressiveness, Dribbling, LostBalls, MatchTempo, APPP)), na.rm = TRUE),
    DefensiveScore = mean(normalize(c(ClosingDown, Fouling, PPDA, RecoveryHigh)), na.rm = TRUE)
  )

# Normalizamos las puntuaciones por columna
df_group_scores_normalized <- as.data.frame(lapply(df_group_scores, function(x) {
  ceiling((x - min(x)) / (max(x) - min(x)) * 5)
}))