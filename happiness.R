library(tidyverse)
library(purr)
library(factoextra)
library(ggrepel)
library(FactoMineR)
library(leaflet)
library(tmap)


data_17 <- read.csv("2017.csv")

str(data_17)

data_17_pca_data <- as.matrix(data_17[,6:12])

rownames(data_17_pca_data) <- data_17$Country

str(data_17_pca)

data_17_pca <- prcomp(data_17_pca_data, scale. = TRUE, center = TRUE)
str(data_17_pca)
print(data_17_pca)
summary(data_17_pca)




# k means groups
set.seed(1234)

data_17_coord <- tibble(pca_1 = data_17_pca$x[,1],
                        pca_2 = data_17_pca$x[,2],
                        pca_3 = data_17_pca$x[,3])

data_17_coms  <- kmeans(data_17_coord, centers = 4, nstart = 100, iter.max = 50)

clusters <- factor(data_17_coms$cluster)

cluster.df <- as.data.frame(clusters)

data_17 <- cbind(data_17, kmeans = cluster.df)

fviz_pca_biplot(data_17_pca, label = "var", repel =T, col.ind = clusters)

View(data_17_coms)


plot(data_17_pca$sdev)





# mapping using T maps 

data("World")

colnames(World)[2] <- "Country"

World <- left_join(World, data_17, by = "Country")

tmap_mode("view")

tm_shape(World) + tm_polygons(c("Happiness.Rank","clusters"), popup.vars = c("Country", "clusters", "Happiness.Rank")) +
  tm_facets(sync = TRUE, nrow = 2) + tm_layout(inner.margins = c(0, .02, .02, .02), earth.boundary = c(-180, -90, 180, 90))

