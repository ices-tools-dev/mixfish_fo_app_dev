plot_map_species <- function(filtered_data, input_sc){

op <- par(cex = 1.5, mar = c(2,2,1,1))

    data2 <- filtered_data
    
    op <- par(cex = 1.5)
    plot(1, xlim = c(-6,15), ylim = c(51,62), 
      t = "n", asp = 2, xlab = "", ylab = "")
    # map("world", xlim = range(agg$lon), ylim = range(agg$lat))
    urect <- unique(data2$icesname)
    for(i in seq(urect)){
      aggsub <- subset(data2, icesname == urect[i])
      sc <- ifelse(input_sc, 1, aggsub$sc[1])

      excl <- which(aggsub$percLandings==0)
      if(length(excl)>0) aggsub <- aggsub[-which(aggsub$percLandings==0)]
      barplot2D(z = aggsub$percLandings, x = aggsub$lon[1], y = aggsub$lat[1],
        width = 1*sc, height = 0.5*sc,
        colour = aggsub$col, border = NA,
        lwd.frame = 0.25, col.frame = "black")
    }
    map("world", add = T, fill = T, col = 8, boundary = 1)
    box()
    uSppCol <- unique(data2[,c("species", "col")])
    uSppCol <- uSppCol[order(uSppCol$species),]
    legend("topright", legend = uSppCol$species, col = uSppCol$col, fill = uSppCol$col)
    par(op)
}