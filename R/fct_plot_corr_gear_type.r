#' Title
#'
#' @param filtered_data_gear_type 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom data.table dcast
#' 
plot_corr_gear_type <- function(filtered_data_gear_type) {
    data2 <- filtered_data_gear_type

    data3 <- dcast(
        data = data2, formula = icesname ~ gear_type,
        value.var = "landings", fun.aggregate = sum, na.rm = TRUE
    )
    rownames(data3) <- data3$icesname
    data3 <- data3[, -1]
    # corrTab <- cor(as.matrix(data3[,-1]))

    op <- par(cex = 1.5, mar = c(1, 1, 1, 1))
    # imageDimnames(round(corrTab,2), col = colorRampPalette(c(2,"white", 4))(21), zlim = c(-1,1))
    # plotCor(data3, log = FALSE)
    mat <- round(cor(data3, use = "pairwise.complete.obs", method = "pearson"), 2)
    imageCor(mat)
    par(op)
}