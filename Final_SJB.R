library(dplyr)
library(sf)
library(usethis)
use_git_config(user.name = "sjbltran", user.email = "sydneybeltran10@gmail.com")

#Use FIA full dataset (eastern US forest)

# Conduct EDA
# Calculate species richness (sr) at grid level
# Calculate average sr aggregated at the latitudinal band
# Calculate the moving average at 0.5 degree interval
# Estimate the SR using bootstrapping methods (1000 times simulation) and report mean and std. sr
# Present your work using ggplot


## Calculate species richness (sr) at grid level

fia <- read.csv("E:/Grad/R_Seminar/R_Seminar/Data/tree_raw_data_with_env_cleaned.csv")

fia_df <- fia %>%
  select(X, TREEcn, plt_cn, invyr, statecd, spcd, dia, ht, lat, long, common)

fia_sf <- st_as_sf(fia_df, coords = c("long", "lat"), crs = 4326)
fia_sf_albers <- st_transform(fia_sf, crs = "ESRI:102008")
extent <- st_bbox(fia_sf_albers)
grid <- st_make_grid(st_as_sfc(extent), crs = "ESRI:102008", cellsize = c(20000, 20000), square = TRUE)
grid_sf <- st_sf(id = 1:length(grid), geometry = grid)
overlaps <- st_join(fia_sf_albers, grid_sf)
centroids_albers <- st_centroid(grid_sf$geometry)
id1 <- unique(overlaps$id)
centroids_albers <- st_centroid(grid_sf[grid_sf$id %in% id1,]$geometry)
centroids_wgs84 <- st_transform(centroids_albers, crs = 4326)
centroids_wgs84_coords <- st_coordinates(centroids_wgs84)
gridid <- grid_sf[grid_sf$id %in% id1,]$id
centroid_df <- data.frame(
  GRIDID = id1,
  centroid_long = centroids_wgs84_coords[,"X"],
  centroid_lat = centroids_wgs84_coords[,"Y"]
)
fia_grid <- merge(overlaps, centroid_df, by.x = "id", by.y = "GRIDID", all.x = TRUE)
fia_grid_df <- as.data.frame(fia_grid)

tree_count_per_id <- fia_grid_df %>%
  group_by(id, centroid_long, centroid_lat) %>%
  summarise(treecount_id = n_distinct(X))
head(tree_count_per_id)  

tree_count_per_spcd <- fia_grid_df %>%
  group_by(spcd, common, id) %>%
  summarise(treecount_spcd = n_distinct(X))
head(tree_count_per_spcd)

species_rich <- merge(tree_count_per_id, tree_count_per_spcd, by = "id", all.x = TRUE)

species_richness1 <- species_rich %>%
  group_by(common, id) %>%
  summarise(richness = n_distinct(spcd)) %>%
  arrange(-richness)
head(species_richness1)

# data frame of the final species richness 
species_richness_final <- as.data.frame(species_richness1)

species_richness <- function(species) {
  df <- species_rich %>%
    filter(common == species) %>%
    group_by(id) %>%
    summarise(richness = n_distinct(spcd)) %>%
    arrange(-richness)
  return(df)
}

species_richness("balsam fir")
