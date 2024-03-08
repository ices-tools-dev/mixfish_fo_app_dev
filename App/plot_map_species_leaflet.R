# plot_map_species <- function(filtered_data, input_sc, ecoregion){
# print(filtered_data)
# op <- par(cex = 1.5, mar = c(2,2,1,1))

#     data2 <- filtered_data
    
#     op <- par(cex = 1.5)
#     mapLimits <- st_bbox(shape_eco[shape_eco$Ecoregion == ecoregion,])
#     plot(1, xlim = c(mapLimits$xmin, mapLimits$xmax), ylim = c(mapLimits$ymin, mapLimits$ymax), 
#       t = "n", asp = 2, xlab = "", ylab = "")
#     # map("world", xlim = range(agg$lon), ylim = range(agg$lat))
#     urect <- unique(data2$icesname)
#     for(i in seq(urect)){
#       aggsub <- subset(data2, icesname == urect[i])
#       sc <- ifelse(input_sc, 1, aggsub$sc[1])

#       excl <- which(aggsub$percLandings==0)
#       if(length(excl)>0) aggsub <- aggsub[-which(aggsub$percLandings==0)]
#       barplot2D(z = aggsub$percLandings, x = aggsub$lon[1], y = aggsub$lat[1],
#         width = 1*sc, height = 0.5*sc,
#         colour = aggsub$col, border = NA,
#         lwd.frame = 0.25, col.frame = "black")
#     }
#     map("world", add = T, fill = T, col = 8, boundary = 1)
#     box()
#     uSppCol <- unique(data2[,c("species", "col")])
#     uSppCol <- uSppCol[order(uSppCol$species),]
#     legend("topright", legend = uSppCol$species, col = uSppCol$col, fill = uSppCol$col)
#     par(op)
# }


library(leaflet)
library(tidyr)
library(dplyr)
library(leaflet.minicharts)

plot_map_species_leaflet <- function(filtered_data, ecoregion){
  geo <- filtered_data
  geo_subset <- geo %>% filter(species %in% c("COD", "HAD","POK" ,"WHG"))
  head(geo_subset)
  test <- geo_subset %>%
  select(icesname,lon , lat, percLandings, species, sc) %>% 
  pivot_wider(names_from = species, values_from = percLandings) 

mapLimits <- st_bbox(shape_eco[shape_eco$Ecoregion == ecoregion,])
print(mapLimits)
basemap <- leaflet(test) %>%
  addTiles() %>%
  setView(lng = -4.5, lat = 48.25, zoom = 6)

colors <- c('#D7191C','#FDAE61', "#ABDDA4", "#2B83BA")

my_popups <- test %>%
  group_by(icesname) %>%
  mutate(popup = paste0(
    "<h3>",
    "ICES area: ", icesname,
    "</h3><br>",
    paste("COD:", COD, "<br>",
      "HAD:", HAD, "<br>",
      "POK:", POK, "<br>",
      "WHG:", WHG,
      collapse = "<br>"
    )
  )) %>%
  pull(popup)


basemap %>%
    addMinicharts(
        test$lon, test$lat,
        type = "pie",
        chartdata = test[, c("COD", "HAD","POK" ,"WHG")], 
        colorPalette = colors,
        popup=popupArgs(
            labels=c("COD", "HAD","POK" ,"WHG"),
            html=my_popups
        ),
        # width = 100*test$sc , 
        width = 50,
        transitionTime = 0
    )

}
















# geo <- read.table("geo_data.csv", sep = ",", row.names = NULL)
# geo_subset <- geo %>% filter(species %in% c("COD", "HAD","POK" ,"WHG"))
# head(geo_subset)

# test <- geo_subset %>%
#   select(icesname,lon , lat, percLandings, species, sc) %>% 
#   pivot_wider(names_from = species, values_from = percLandings) 


# basemap <- leaflet(test) %>%
#   addTiles() %>%
#   setView(lng = -4.5, lat = 48.25, zoom = 6)

# colors <- c('#7fc97f','#beaed4', "#351DB9", "#FAE842")

# my_popups <- test %>%
#   group_by(icesname) %>%
#   mutate(popup = paste0(
#     "<h3>",
#     "ICES area: ", icesname,
#     "</h3><br>",
#     paste("COD:", COD, "<br>",
#       "HAD:", HAD, "<br>",
#       "POK:", POK, "<br>",
#       "WHG:", WHG,
#       collapse = "<br>"
#     )
#   )) %>%
#   pull(popup)


# basemap %>%
#     addMinicharts(
#         test$lon, test$lat,
#         type = "pie",
#         chartdata = test[, c("COD", "HAD","POK" ,"WHG")], 
#         colorPalette = colors,
#         popup=popupArgs(
#             labels=c("COD", "HAD","POK" ,"WHG"),
#             html=my_popups
#         ),
#         # width = 100*test$sc , 
#         width = 50,
#         transitionTime = 0
#     )
