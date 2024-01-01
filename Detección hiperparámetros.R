#Librerías necesarias
library(readxl)
library(dplyr)
library(umap)
library(mclust)
library(ggplot2)
library(ggrepel)
library(plotly)
library(cluster)
library(fpc)
library(factoextra)

# Marca el tiempo de inicio
start_time <- Sys.time()

#Cargar los datos
file_path <- "/Users/nacho/Desktop/Proyectos personales/Terminados/Clusterización Proyecto/Ligas Inglesas/EnglandTeamsStatsTodos.xlsx"
df<- read_excel(file_path)

# Función para crear nuevas métricas
crear_métricas <- function(df) {
  df %>%
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
      RecoveryHigh = (`% Recoveries High` / Passes) * 100, 
      MatchTempo = `Match tempo`,
      APPP = `Average passes per possession`
    ) %>%
    # Reemplazar NaN con 0 en todas las columnas de métricas
    mutate(across(everything(), ~replace(., is.nan(.), 0)))
}

# Crear nuevas métricas para el modelo
df <- crear_métricas(df)

# Preparar los datos para UMAP (seleccionar las métricas necesarias)
umap_data <- df %>% select(AttackingAbility, OffsideRate, BuildupRate, CrossingEfficiency, 
                           DeepPassesEfficiency, PassingAccuracy, LengthPassesRate, Directness, 
                           Switches, DuelsRate, Progressiveness, Dribbling, LostBalls,
                           ClosingDown, Fouling, RecoveryHigh, MatchTempo, APPP)

# Establecer semilla para reproducibilidad
set.seed(123)

# Inicializa la variable para guardar el mejor valor de Silhouette
best_silhouette <- -Inf
best_n_neighbors <- NULL
best_min_dist <- NULL

# Define los rangos de los hiperparámetros que quieres probar
n_neighbors_range <- seq(10, 50, by = 1)
min_dist_range <- seq(0.01, 0.50, by = 0.01)
num_clusters_range <- 4:10  # Puedes ajustar esto según lo que consideres apropiado

# Inicializa la lista para guardar los resultados y sus parámetros
results <- list()

# Bucle sobre los rangos de n_neighbors, min_dist y num_clusters
for (n_neighbors in n_neighbors_range) {
  for (min_dist in min_dist_range) {
    for (num_clusters in num_clusters_range) {
      # Ejecuta UMAP con los hiperparámetros actuales
      umap_result <- umap(umap_data, n_neighbors = n_neighbors, min_dist = min_dist)
      
      # Ejecuta GMM Clustering en el resultado de UMAP
      gmm_model <- Mclust(umap_result$layout, G = num_clusters)
      clusters <- gmm_model$classification
      
      # Calcula el índice Silhouette
      silhouette_score <- mean(silhouette(clusters, dist(umap_result$layout))[, "sil_width"])
      
      # Guarda los resultados y los parámetros
      results[[length(results) + 1]] <- list(
        silhouette_score = silhouette_score,
        n_neighbors = n_neighbors,
        min_dist = min_dist,
        num_clusters = num_clusters
      )
    }
  }
}

# Ordena los resultados por el índice Silhouette y conserva los 10 mejores
results <- results[order(sapply(results, function(x) x$silhouette_score), decreasing = TRUE)]
top_10_results <- head(results, 10)

# Imprime los 10 mejores resultados
for (i in 1:10) {
  cat("Resultado", i, ":\n",
      "Índice Silhouette:", top_10_results[[i]]$silhouette_score, "\n",
      "n_neighbors óptimo:", top_10_results[[i]]$n_neighbors, "\n",
      "min_dist óptimo:", top_10_results[[i]]$min_dist, "\n",
      "Número óptimo de clusters:", top_10_results[[i]]$num_clusters, "\n\n")
}

# Marca el tiempo de finalización
end_time <- Sys.time()

# Calcula la duración y muestra el tiempo total de ejecución
duration <- end_time - start_time
cat("Tiempo total de ejecución:", duration, "\n")