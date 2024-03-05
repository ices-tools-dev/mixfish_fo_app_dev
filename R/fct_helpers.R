#' helpers 
#' 
#' 
#' imageCor
#'
#' @description A plotting function for producing correlation plots
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
imageCor <- function(mat, 
xlab = NULL, ylab = NULL, xaxisSide = 1, yaxisSide = 2,
axisLas = 1, log = FALSE,
drawBorders = TRUE, borderCol = 1, borderLty = 1, borderLwd = 1,
addLabels = TRUE, labels = ac(c(mat)), labelCol = "black", labelFont = 1,
col = colorRampPalette(c(2,"white", 4))(21), zlim = c(-1,1),
...){
  
  
  if(is.null(dimnames(mat))){
    dimnames(mat) <- list(seq(nrow(mat)), seq(ncol(mat)))
  }
  if(is.null(dimnames(mat)[[1]])){
    dimnames(mat)[[1]] <- seq(nrow(mat))
  }
  if(is.null(dimnames(mat)[[2]])){
    dimnames(mat)[[2]] <- seq(ncol(mat))
  }
  
  # mat[!lower.tri(mat)] <- NaN
  x <- seq(dimnames(mat)[[1]])
  y <- seq(dimnames(mat)[[2]])
  z <- mat
  if(is.null(xlab)) xlab = names(dimnames(mat))[1]
  if(is.null(ylab)) ylab = names(dimnames(mat))[2]
  if(is.null(xlab)) xlab = ""
  if(is.null(ylab)) ylab = ""
  
  idx <- which(lower.tri(mat))
  mat[!lower.tri(mat)] <- NaN
  z <- mat
  image(x = x, y = y, z = z, axes = FALSE, xlab = xlab, ylab = ylab, col = col, zlim = zlim)#, ...)
  
  polys <- matrixPoly(x, y, z=z, n=idx)
  COL <- val2col(z = z[idx], col = col, zlim = zlim)
  for(i in seq(polys)){
    polygon(polys[[i]], col=COL[i], border=1)
  }
  
  # matrix value labels
  txt <- as.data.frame(matrixIndex(idx = idx, dim.mat = dim(mat)))
  txt$val <- c(z[idx])
  txt$x <- x[txt$row]
  txt$y <- y[txt$col]
  txt$labels <- labels[idx]
  text(x = txt$x, y = txt$y, labels = txt$labels, 
       col = labelCol, font = labelFont)
  
  # dimension labels
  txt <- data.frame(x = x, y = y-0.5, labels = dimnames(mat)[[1]])
  text(x = txt$x, y = txt$y, labels = txt$labels, 
       col = 1, font = labelFont, pos = 3)
  
}

