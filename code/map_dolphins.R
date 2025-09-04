# Install packages if needed
# install.packages("marmap")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages("rnaturalearth")

rm(list = ls())

# Load packages
library(dplyr)
library(marmap)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggrepel)

# Define the geographic area (adjust as needed to fit dataset locations)
min_lon <- -90
max_lon <- 0
min_lat <- -40
max_lat <- 50

# Download bathymetry data from NOAA
bathy_data <- getNOAA.bathy(lon1 = min_lon, lon2 = max_lon,
                            lat1 = min_lat, lat2 = max_lat,
                            resolution = 10)

# Convert bathymetry data to a data frame for ggplot
bathy_df <- as.xyz(bathy_data)

# Create a data frame for dataset locations
dataset_locations <- readxl::read_xlsx("dataraw/DSI_Recording Metadata and Coordinates.xlsx", n_max = 10) %>%
    mutate(Use = factor(Use, levels = c("Training", "Verification")))

# Set fill colors for Use
use_fill_colors <- c("Training" = "black", "Verification" = "limegreen")

# Get coastline data for plotting land
land_data <- ne_countries(scale = "large", returnclass = "sf")

# Create the map
map_plot <- ggplot() +
    # 1. Add the bathymetry layer as a raster
    geom_raster(data = bathy_df, aes(x = V1, y = V2, fill = V3)) +

    # 2. Add a color scale for bathymetry (blue gradient for water)
    scale_fill_gradientn(
        colors = c("darkblue", "blue", "lightblue"),
        values = scales::rescale(c(min(bathy_df$V3, na.rm = TRUE), -100, 0)),
        name = "Depth (m)",
        limits = c(min(bathy_df$V3, na.rm = TRUE), 0) # Focus on negative depths (water)
    ) +

    # 3. Add contour lines for bathymetry
    geom_contour(data = bathy_df, aes(x = V1, y = V2, z = V3),
                 breaks = seq(-8000, 0, by = 1000),
                 colour = "white", alpha = 0.5, linewidth = 0.5) +

    # 4. Add land/coastline features
    geom_sf(data = land_data, fill = "antiquewhite", color = "black", inherit.aes = FALSE) +

    # 5. Add dataset locations as points
    # geom_point(data = dataset_locations, aes(x = Long, y = Lat, color = Dataset, shape = Species),
    #            size = 4) +

    # 6. Add labels for locations
    # geom_label(data = dataset_locations,
    #            aes(x = Long, y = Lat, label = Dataset, color = Use),
    #            size = 3, fill = "white", alpha = 0.8, label.padding = unit(0.15, "lines"),
    #            # color = "black",
    #            fontface = "bold") +

    # OR
    geom_point(data = dataset_locations, aes(x = Long, y = Lat), size = 2) +
    geom_label_repel(
        data = dataset_locations,
        aes(x = Long, y = Lat, label = Dataset, color = Use),
        fontface = "bold",
        size = 3,
        box.padding = 0.5,         # more padding between labels
        point.padding = 0.5,       # more padding between label and point
        segment.color = "black",  # color of leader lines
        segment.size = 0.7        # thickness of leader lines
        # show.legend = FALSE        # hide label legend if needed
    ) +

    scale_colour_manual(
        name = "Dataset",
        values = use_fill_colors
    ) +

    # 7. Set map limits and labels
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat), expand = FALSE) +
    scale_x_continuous(name = "Longitude", breaks = seq(min_lon, max_lon, by = 10)) +
    scale_y_continuous(name = "Latitude", breaks = seq(min_lat, max_lat, by = 5)) +
    # labs(#title = "Dataset Recording Locations",
    #      #subtitle = "Labeled by Dataset and Location",
    #      x = "Longitude",
    #      y = "Latitude",
    #      # fill = "Use",
    #      # fill = "Dataset"
    #      # ,shape = "Species"
    #      ) +
    theme_light() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right",
        text = element_text(family = "Arial")
    )

# Display the map
print(map_plot)
ggsave("map_dolphins.png", width = 10, height = 9, dpi = 600)
