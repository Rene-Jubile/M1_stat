
#             Travail pratique de Statistique Spatiale
#             Par MSc. Alain Kangela
#             Promotion: Master 1 EEB et Sol
# ==========================================
# width: length dist
# cutoff: maximum distance

# I. Statistique Spatiale Classique
# **********************************
#              a) Variogramme empirique
#              b) Modeles du variogramme
#              c) Krigging
#              d) Simulation
# Load data in R
# **************

data <- read.table(file = "data/Data_Example_1.txt", header = T, sep = "\t")
names(data)
str(data)

data$sampleID <- as.numeric(data$sampleID)
data$subplot <- as.numeric(data$subplot)
data$OTU_all_taxa <- as.numeric(data$OTU_all_taxa)
str(data)

# create dataset subsets for each month
april <- data[data$month == "April", ]
may <- data[data$month == "May", ]
june <- data[data$month == "June", ]
august <- data[data$month == "August", ]
october <- data[data$month == "October", ] #Me
november <- data[data$month == "November", ]

# Sampling date April
# ====================
#  1. Variogramme

library(gstat)
# compute undirected (isotropic) empirical semivariogram
var1 <- variogram((log(OTU_all_taxa) * 10) ~ 1, ~x + y, cutoff = 8, width = 1, 
                  alpha = 90, data = november) #Replace april by october

# var1 <- variogram((log(OTU_all_taxa) * 10) ~ 1, ~x + y, cutoff = 8,
#                   width = 1, data = october) #Replace april by october

# create figure of empirical semivariogram
#pdf("Figure_Semivariogram_april_undirected.pdf", height = 5, width = 7)
plot(var1, plot.numbers = TRUE, xlab = "Distance", ylab = "Semivariance", cex = 1, cex.axis = 2)
dev.off()

# compute semivariogram model based on the empiral semivariogram 'var1'
mod1 <- fit.variogram(var1, vgm(psill = NA, c("Exp", "Sph"), range = NA, 1), fit.sills = TRUE, fit.ranges = TRUE, fit.method = 1)

# compute semivariogram model based on the empiral semivariogram 'var1'
# mod1 <- fit.variogram(var1, vgm(psill = NA,"Mat", range = NA, 1), fit.sills = TRUE, fit.ranges = TRUE, fit.method = 1)
# show nugget, psill and range of the semivariogram model
mod1
# show the (weighted) sum of squared errors of the fitted model
attr(mod1, "SSErr")


#pdf("Figure_semivariogram_model_april.pdf", height = 5, width = 7)
# plot figure of semivariogram model
plot(var1, plot.numbers = TRUE, model = mod1, xlab = "Distance", ylab = "Semivariance")
dev.off()


#   2. Krigging

# y-values of the corners
y <- c(10, 10, 0, 0)
# x-values of the corners
x <- c(0, 10, 10, 0)
# pdf(file = "xy_scatter_polygon_coordinates.pdf", width = 5, height = 5.5)
# Scatterplot of the coordinates
plot(x, y)
# adding a square with the coordinates defined by x and y
polygon(x, y, border = "red")
dev.off()

# create matrix from x and y coordinates of the corners
xym <- cbind(x, y)
xym
# create spatial polygon
library(sp)

p <- Polygon(xym)

# create list of Polygon object
ps <- Polygons(list(p), 1)
# create objects of class SpatialPolygons or SpatialPolygonsDataFrame from lists of Polygons objects and
# data.frames
sps <- SpatialPolygons(list(ps))
#pdf(file = "polygon_with_prediction_points.pdf", width = 5, height = 5)
# plot polygon frame
plot(sps)
# show points for prediction locations within the polygon, using regular sampling methods, n = number of
# sampling points, the higher the number of points for which predictions should be calculated, the
# smoother is the projection
points(spsample(sps, n = 1000, "regular"), pch = 20)
dev.off()


# create a dataframe from prediction locations
grid_x_y <- as.data.frame(spsample(sps, n = 15000, "regular"))
# set column names to be identical on both datasets, the one containing the original measurement data and
# the one conatinaing the prediction locations
colnames(grid_x_y) <- c("x", "y")
# specify the columns x and y as coordinates
coordinates(grid_x_y) <- ~x + y

library(ggplot2)
#first, we have a look at the scatterplot plot of the sampling points

#pdf(file="points_april.pdf", width = 7, height = 5)
#plot the actual sampling points in April with point size occording to number of OTUs occurring
ggplot(april, aes(x, y)) +
  geom_point(aes(size=OTU_all_taxa), color="blue", alpha=3/4) +
  labs(x="m", y="m", title = NULL, subtitle = NULL, caption = NULL, tag = NULL,
       size="April \n \ntotal \nOTU richness") +
  coord_equal() +
  theme_bw()
dev.off()

#specify the columns x and y as coordinates
coordinates(april) <- ~ x + y
#kriging on the untransformed data of total OTU richness from sampling locations
#in april and the 15000 locations used for interpolation should be done
mod1.kriged <- krige((OTU_all_taxa) ~ 1, april, grid_x_y, model=mod1)
## [using ordinary kriging]
#write kriging file as dataframe for graphical display
frame_mod1.kriged<-as.data.frame(mod1.kriged)
#create figure from kriging results
# pdf(file="krig_april.pdf", width = 7, height = 5)
ggplot(frame_mod1.kriged, aes(x=x, y=y)) +
  geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_distiller(palette = "Purples", limits=c(10,45), direction = 1)+
  labs(x="m", y="m", title = NULL, subtitle = NULL, caption = NULL, tag = NULL,
       fill="total \nOTU richness") +
  annotate("text", label = "April", x = 9, y = 10.5, color = "black")+
  theme_classic()
dev.off()
