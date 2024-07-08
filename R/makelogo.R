if (!require("hexSticker")) {
  install.packages("hexSticker")
  library(hexSticker)
}
if (!require("purrr")) {
  install.packages("purrr")
  library(purrr)
}

# Define the base coordinates and sizes
xbase <- 0
ybase <- 0.5
xsize <- 2
ysize <- 0.18

# Function to create polygons
create_polygons <- function(xbase, xsize, ybase, ysize, n) {
  x_coords <- c(xbase, xsize, xsize, xbase)
  y_coords <- c(ybase, ybase, ybase + ysize, ybase + ysize)

  xlist <- rep(list(x_coords), n)
  ylist <- lapply(0:(n - 1), function(i) y_coords + ysize * i * 2)

  list(xlist = xlist, ylist = ylist)
}

# Function to plot polygons
plot_polygons <- function(xlist, ylist, color) {
  mapply(function(x, y) polygon(x, y, col = color, border = NA), x = xlist, y = ylist)
}

# Function to create a circle
create_circle <- function(center_x, center_y, radius, color) {
  theta <- seq(0, 2 * pi, length = 30)
  polygon(
    x = (radius * cos(theta)) + center_x,
    y = (radius * sin(theta)) + center_y,
    col = color, border = color
  )
}

generate_coordinates <- function(x, y, rad, ptsl) {
  positions <- list(
    list(min(x) + rad, max(y) - rad),
    list(max(x) - rad, max(y) - rad),
    list(max(x) - rad, min(y) + rad),
    list(min(x) + rad, min(y) + rad)
  )

  coord_list <- pmap(list(positions, ptsl), function(pos, pt) {
    cbind(pos[[1]] + rad * sin(pt), pos[[2]] + rad * cos(pt))
  })
  names(coord_list) <- c("xy_1", "xy_2", "xy_3", "xy_4")
  coord_list
}

# Function to create a rounded rectangle
create_rounded_rectangle <- function(xleft, xright, ydown, yup, rad, ver, color) {
  x <- c(xright, xright, xleft, xleft)
  y <- c(yup, ydown, ydown, yup)
  yMod <- y
  yMod <- replace(y, y == max(y), max(y) - rad)
  yMod <- replace(yMod, yMod == min(y), min(y) + rad)
  topline_y <- rep(max(y), 2)
  topBotline_x <- c(min(x) + rad, max(x) - rad)
  bottomline_y <- rep(min(y), 2)
  pts <- seq(-pi / 2, pi * 1.5, length.out = ver * 4)
  ptsl <- split(pts, sort(rep(1:4, each = length(pts) / 4, len = length(pts))))
  coordinates <- generate_coordinates(x, y, rad, ptsl)
  newLongx <- c(
    x[4], coordinates$xy_1[, 1], topBotline_x, coordinates$xy_2[, 1]
  )
  newLongy <- c(
    yMod[4], coordinates$xy_1[, 2], topline_y, coordinates$xy_2[, 2]
  )
  polygon(newLongx, newLongy, col = color, border = color)
}

polygons1 <- create_polygons(xbase, xsize, ybase, ysize, 3)
polygons2 <- create_polygons(xbase, xsize / 2, ybase + ysize * 6, ysize, 2)
plot("", xlim = c(0, 4), ylim = c(0, 2.5), axes = FALSE, main = NA, xlab = "", ylab = "")
plot_polygons(polygons1$xlist, polygons1$ylist, "blue")
plot_polygons(polygons2$xlist, polygons2$ylist, "blue")
create_circle(1.5, 1.95, 0.18, "blue")
create_rounded_rectangle(1.25, 1.75, 1.2, 1.77, 0.2, 25, "blue")

color2 <- "cadetblue4"
color_azure <- "azure4"
library(hexSticker)
sticker(
  expression({
    polygons1 <- create_polygons(xbase, xsize, ybase, ysize, 3)
    polygons2 <- create_polygons(xbase, xsize / 2, ybase + ysize * 6, ysize, 2)
    plot("", xlim = c(0, 4), ylim = c(0, 2.5), axes = FALSE, main = NA, xlab = "", ylab = "")
    plot_polygons(polygons1$xlist, polygons1$ylist, color2)
    plot_polygons(polygons2$xlist, polygons2$ylist, color2)
    create_circle(1.5, 1.95, 0.18, color_azure)
    create_rounded_rectangle(1.25, 1.75, 1.2, 1.77, 0.2, 25, color_azure)
  }),
  package = "CV_yml", p_size = 16, s_x = 1.5, s_y = .8, s_width = 3, s_height = 2, p_color = color2, p_y = 1.5,
  filename = "assets/logo.png", h_color = color2, h_fill = "white"
)
