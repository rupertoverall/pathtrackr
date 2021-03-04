#' Plot animal movement data
#'
#' This function plots the path of an animal tracked using the \code{trackPath} function.
#' @param path.list a list object created by the \code{trackPath} function
#' @details This function relies on the ggplot package.
#' @return A density plot of time spent in a position overlaid with the tracked animal's path.
#' @importFrom ggplot2 aes geom_point geom_path element_blank stat_density2d coord_fixed scale_x_continuous scale_y_continuous theme_bw theme scale_fill_gradientn ggplot xlab ylab element_text
#' @export
plotPath = function(path.list) {
  ..density.. = NULL
  
  dat = as.data.frame(path.list$position)
  dat$xpos = dat$xpos * (path.list$dim.arena[1]/path.list$dim.pix[1])
  dat$ypos = dat$ypos * (path.list$dim.arena[2]/path.list$dim.pix[2])
  x_max = path.list$dim.pix[1] * (path.list$dim.arena[1]/path.list$dim.pix[1])
  y_max = path.list$dim.pix[2] * (path.list$dim.arena[2]/path.list$dim.pix[2])
  
  ggplot2::ggplot(dat, ggplot2::aes(x = path.list$xpos, y = path.list$ypos)) +
    ggplot2::stat_density2d(ggplot2::aes(fill = ..density.., alpha = ..density..), geom = "tile", contour = FALSE) +
    ggplot2::scale_fill_gradientn(colours = viridis::viridis(256)) +
    ggplot2::geom_path(na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = dat[1,1], y = dat[1,2]), size = 3, color = "blue") +
    ggplot2::geom_point(ggplot2::aes(x = dat[nrow(dat),1], y = dat[nrow(dat),2]), size = 3, color = "red") +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
    ggplot2::xlab("Distance (mm)") +
    ggplot2::ylab("Distance (mm)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none", axis.title = ggplot2::element_text(size = 14, face = "bold"), panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

}
