library(hexSticker)

xbase <- 0
ybase <- 0
xsize <- .8
ysize <- .2

fpolygonx <- c(xbase, xsize, xsize, xbase)
spolygonx <- fpolygonx + xsize * (1 / .8)
tpolygonx <- spolygonx + xsize * (1 / .8)

fpolygony <- c(ybase, ybase, ybase + ysize, ybase + ysize)
spolygony <- fpolygony + ysize * 2
tpolygony <- spolygony + ysize * 2

xlist <- c(
  rep(list(fpolygonx), 3),
  rep(list(spolygonx), 3),
  rep(list(tpolygonx), 3)
)

ylist <- c(rep(list(
  c(fpolygony),
  c(spolygony),
  c(tpolygony)
), 3))

{
  plot("", xlim = c(0, 4), ylim = c(0, 2.5), axes = F, main = NA, xlab = "", ylab = "")
  mapply(function(x, y) polygon(x, y, col = "blue", border = NA), x = xlist, y = ylist)
  axis(1)
  axis(2)
  radius <- .2
  theta <- seq(0, 2 * pi, length = 30)
  polygon(x = (radius * cos(theta)) + 1.7, y = (radius * sin(theta)) + 1.95, col = "blue", border = "blue")
}

{
  xleft <- 1.4
  xright <- 2
  ydown <- 1.45
  yup <- 1.75
  x <- c(xright, xright, xleft, xleft)
  y <- c(yup, ydown, ydown, yup)
  rad <- .2
  ver <- 25
  yMod <- y
  yMod[which(yMod == max(yMod))] <- yMod[which(yMod == max(yMod))] - rad
  yMod[which(yMod == min(yMod))] <- yMod[which(yMod == min(yMod))] + rad
  topline_y <- rep(max(y), 2)
  topBotline_x <- c(min(x) + rad, max(x) - rad)
  bottomline_y <- rep(min(y), 2)
  pts <- seq(-pi / 2, pi * 1.5, length.out = ver * 4)
  ptsl <- split(pts, sort(rep(1:4, each = length(pts) / 4, len = length(pts))))
  xy_1 <- cbind((min(x) + rad) + rad * sin(ptsl[[1]]), (max(y) - rad) + rad * cos(ptsl[[1]]))
  xy_2 <- cbind((max(x) - rad) + rad * sin(ptsl[[2]]), (max(y) - rad) + rad * cos(ptsl[[2]]))
  xy_3 <- cbind((max(x) - rad) + rad * sin(ptsl[[3]]), (min(y) + rad) + rad * cos(ptsl[[3]]))
  xy_4 <- cbind((min(x) + rad) + rad * sin(ptsl[[4]]), (min(y) + rad) + rad * cos(ptsl[[4]]))
  newLongx <- c(
    x[4], xy_1[, 1], topBotline_x, xy_2[, 1]
  )
  newLongy <- c(
    yMod[4], xy_1[, 2], topline_y, xy_2[, 2]
  )
  # par(pty="s")
  # plot("", xlim=c(0,4), ylim=c(0,4))
  polygon(newLongx, newLongy, col = "blue", border = "blue")
}

color2 <- "cadetblue4"
color <- "azure4"
library(hexSticker)
sticker(
  expression({
    plot("", xlim = c(0, 4), ylim = c(0, 2.5), axes = F, main = NA, xlab = "", ylab = "")
    mapply(function(x, y) polygon(x, y, col = color, border = NA), x = xlist, y = ylist)
    polygon(x = (radius * cos(theta)) + 1.7, y = (radius * sin(theta)) + 1.95, col = color2, border = color2)
    polygon(newLongx, newLongy, col = color2, border = color2)
  }),
  package = "curriculumPu", p_size = 16, s_x = 1, s_y = .8, s_width = 3, s_height = 2, p_color = color2, p_y = 1.5,
  filename = "logo2.png", h_color = color2, h_fill = "white"
)
