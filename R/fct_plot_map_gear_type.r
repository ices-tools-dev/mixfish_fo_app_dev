#' Title
#'
#' @param filtered_data_gear_type 
#' @param input_sc 
#' @param ecoregion 
#'
#' @return
#' @export
#' @importFrom sf st_bbox #' 
#' @importFrom mapplots barplot2D 
#' @importFrom maps map
#' @importFrom graphics box legend
#'
#' @examples
plot_map_gear_type <- function(filtered_data_gear_type, input_sc, ecoregion) {
    
    data2 <- filtered_data_gear_type

    op <- par(cex = 1.5, mar = c(2, 2, 1, 1))
    mapLimits <- sf::st_bbox(shape_eco[shape_eco$Ecoregion == ecoregion,])

    plot(1,
        xlim = c(mapLimits$xmin, mapLimits$xmax), ylim = c(mapLimits$ymin, mapLimits$ymax),
        t = "n", asp = 2, xlab = "", ylab = ""
    )

    urect <- unique(data2$icesname)
    for (i in seq(urect)) {
        aggsub <- subset(data2, icesname == urect[i])
        sc <- ifelse(input_sc, 1, aggsub$sc[1])

        excl <- which(aggsub$percLandings == 0)
        if (length(excl) > 0) aggsub <- aggsub[-which(aggsub$percLandings == 0)]
        barplot2D(
            z = aggsub$percLandings, x = aggsub$lon[1], y = aggsub$lat[1],
            width = 1 * sc, height = 0.5 * sc,
            colour = aggsub$col, border = NA,
            lwd.frame = 0.25, col.frame = "black"
        )
    }
    map("world", add = T, fill = T, col = 8, boundary = 1)
    box()
    uGearCol <- unique(data2[, c("gear_type", "col")])
    uGearCol <- uGearCol[order(uGearCol$gear_type), ]
    legend("topright", legend = uGearCol$gear_type, col = uSppCol$col, fill = uGearCol$col)
    par(op)
}