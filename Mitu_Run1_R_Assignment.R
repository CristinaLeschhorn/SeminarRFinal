#1. Conduct EDA (Exploratory Data Analysis)
df <- read.csv("C:/Users/Ad/Documents/Rtest_git/Data/fia_df_final.csv")
unique(df$COMMON_NAME)
library(dplyr)
nrow(df)
ncol(df)
names(df)
head(df)
summary(df)


#####2. Calculate species richness (sr) at grid level

sr <- df%>%
  group_by(id)%>%
  summarise(srbygrid=n_distinct(spcd))
sr
plot(sr$id, sr$srbygrid)

# 3. Calculate average sr aggregated at the latitudinal band
###########
min(df$centroid_lat)
max(df$centroid_lat)
?seq
seq(from = 25, to = 50)
seq(from = 25, to = 50, by = 1)
myseq <- seq(from = 25, to = 50, by = 1)
for (i in myseq){
  print(i+1)
}
sr_bands <- data.frame(matrix(ncol = 2, nrow = length(myseq)))
names(sr_bands) <- c("LatBands","SR")
sr_bands

for (i in 1:length(myseq)){
  lat_min <- myseq[i] 
  lat_max <- myseq[i] + 1
  df_subset <- df %>%
    filter(centroid_lat > lat_min & centroid_lat <= lat_max)
  sr <- length(unique(df_subset$COMMON_NAME))
  sr_bands[i,1] <- myseq[i]
  sr_bands[i,2] <- sr
}
sr_bands

######

#exercise for loop to do moving average

min(df$centroid_lat)
max(df$centroid_lat)
?seq
seq(from = 25, to = 50)
seq(from = 25, to = 50, by = 0.5)
myseq <- seq(from = 25, to = 50, by = 0.5)
for (i in myseq){
  print(i+1)
}

myseq
lat_min <- myseq[2] - 0.5
lat_min
lat_max <- myseq[2] + 0.5
lat_max
length(myseq)
length(myseq)

sr_bands <- data.frame(matrix(ncol = 2, nrow = length(myseq)))
names(sr_bands) <- c("LatBands","SR")
sr_bands

for (i in 1:length(myseq)){
  lat_min <- myseq[i] - 0.5
  lat_max <- myseq[i] + 0.5
  df_subset <- df %>%
    filter(centroid_lat > lat_min & centroid_lat <= lat_max)
  sr <- length(unique(df_subset$COMMON_NAME))
  sr_bands[i,1] <- myseq[i]
  sr_bands[i,2] <- sr
}
library(ggplot2)
sr_bands
ggplot(data = sr_bands, aes(x = LatBands, y = SR)) +
  geom_point()


# Bootstrapping function########
#myiteration <- seq(from = 1, to = 5, by = 1)
#for (i in myiteration){
# print(i)
#}
bootstrap <- data.frame(matrix(ncol = 1001, nrow = length(myseq)))
#names(bootstrap) <- c("LatBands","iteration1","iteration2","iteration3","iteration4","iteration5")
bootstrap

myseq <- seq(from = 25, to = 50, by = 0.5)



library(dplyr)

# Initialize the bootstrap data frame with NA values
bootstrap <- data.frame(matrix(NA, nrow = length(myseq), ncol = 1001))
#names(bootstrap) <- c("LatBands", "iteration1", "iteration2", "iteration3", "iteration4", "iteration5")

# Bootstrapping loop
for (i in 1:length(myseq)) {
  lat_min <- myseq[i] - 0.5
  lat_max <- myseq[i] + 0.5
  df_subset <- df %>%
    filter(centroid_lat > lat_min & centroid_lat <= lat_max)
  
  # Store the latitude band
  bootstrap[i, 1] <- myseq[i]
  
  # Perform 1000 bootstrapping iterations for the current latitude band
  for (iteration in 2:1001) {
    sample_200 <- sample(1:nrow(df_subset), size = 200, replace = TRUE)
    bootstrap_sample <- df_subset[sample_200, ]
    bootstrap_sr <- length(unique(bootstrap_sample$COMMON_NAME))
    bootstrap[i, iteration] <- bootstrap_sr
  }
}

# Display the bootstrapped dataset
print(bootstrap)
head(bootstrap)

#write.csv(bootstrap, "bootsrapSR.csv")
?sample


###for initialize new dataset with mean and std
myseq <- seq(from = 25, to = 50, by = 0.5)
Result <- data.frame(matrix(ncol = 3, nrow = length(myseq)))
names(Result) <- c("LatBands","Mean", "std")
Result


for (i in 1:length(bootstrap$X1)){
  means <- rowMeans(bootstrap[i, ], na.rm = TRUE)
  stds <- sd(bootstrap[i, ],  na.rm = TRUE)
  Result[i,1] <- myseq[i]
  Result[i,2] <- means
  Result[i,3] <- stds
}
Result  
library(ggplot2)


# Plot using ggplot2
ggplot(Result, aes(x = LatBands, y = Mean)) +
  geom_point() +  # This adds the mean points
  geom_errorbar(aes(ymin = Mean - std, ymax = Mean + std), width = 0.2) +
  labs(title = "Mean and Standard Deviation by Latitude Bands",
       x = "Latitude Bands",
       y = "Mean+std") + 
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

