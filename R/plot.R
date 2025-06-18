#' Plot a TreeMapMatching object
#'
#' @param x TreeMapMatching object
#' @param y not used
#' @param scale upscale tree diameters for a better readability of the plot
#' @param rgl bool plot in 3D with rgl
#' @param ... not used
#' @exportS3Method
plot.TreeMapMatching = function(x, y, scale = 1, rgl = FALSE, ...)
{
  if (is.null(x$match_table))
  {
    if (!rgl)
      compare_plot(x, scale, ...)
    else
      compare_plot3d(x)
  }
  else
  {
    if (!rgl)
      plot_spatial_matching(x, scale, ...)
    else
      plot_spatial_matching3d(x)
  }
}

plot_plot = function(x, scale = 1)
{
  center = get_center(x)
  plot(sf::st_buffer(center, x$radius), border = "red", axes = T)
  plot(sf::st_buffer(center, 4), add = T, border = "blue")
  plot(center, add = T, col = "red", cex = 2, pch = 3)

  shapes = sf::st_buffer(x, x$DBH/2*scale)
  plot(sf::st_geometry(shapes), add = T, col = "gray")
}

compare_plot = function(treemap, scale = 1)
{
  inventory = treemap$inventory
  measure = treemap$measured
  radius = treemap$radius
  center = treemap$center

  is_standardized(inventory)
  is_standardized(measure)

  plot(sf::st_buffer(center, radius), border = "red", axes = T)
  plot(sf::st_buffer(center, 4), add = T, border = "blue")
  plot(center, add = T, col = "red", cex = 2, pch = 3)

  shapes = sf::st_buffer(inventory, inventory$DBH/2*scale)
  plot(sf::st_geometry(shapes), add = T, col = "lightgreen", border = "darkgreen")

  shapes = sf::st_buffer(measure, measure$DBH/2*scale)
  plot(sf::st_geometry(shapes), add = T, col = "lightblue", border = "blue")

  graphics::legend(x = "topleft", legend=c("Ground truth", "Measurements"), fill = c("lightgreen","lightblue"), border = c("darkgreen", "blue"))
}

compare_plot3d = function(treemap)
{
  factor = scale_z_factor(treemap$radius, treemap$inventory$DBH)

  inventory = treemap$inventory
  measure = treemap$measured

  offsetx = mean(xyz[,1])
  offsety = mean(xyz[,2])
  xyz = sf::st_coordinates(inventory) |> as.data.frame()
  xyz[,1] = xyz[,1] - offsetx
  xyz[,2] = xyz[,2] - offsety
  xyz$Z = inventory$DBH*factor
  XYZ = sf::st_coordinates(measure) |> as.data.frame()
  XYZ[,1] = XYZ[,1] - offsetx
  XYZ[,2] = XYZ[,2] - offsety
  XYZ$Z = measure$DBH*factor

  rgl::plot3d(xyz, asp =1, col = "darkgreen", size = 5)
  rgl::aspect3d("iso")
  rgl::points3d(XYZ, asp =1, col = "blue", size = 5)
  pan3d(2)
}

plot_spatial_matching = function(treemap, scale = 1)
{
  inventory = treemap$inventory
  measure = treemap$measured
  radius = treemap$radius
  center = treemap$center
  match_table = treemap$match_table

  d = as.numeric(sf::st_distance(treemap$measured, treemap$center))
  inside = d < treemap$radius

  matched = match_table[!is.na(match_table$index_inventory),]

  plot(sf::st_buffer(center, 11.28), border = "red", axes = T)
  plot(sf::st_buffer(center, 4), add = T, border = "blue")
  plot(center, add = T, col = "red", cex = 2, pch = 3)

  color = rep("orange", nrow(inventory))
  color[matched$index_inventory] = "black"
  shapes = sf::st_buffer(inventory, inventory$DBH/2*scale)
  plot(sf::st_geometry(shapes), add = T, col = color, border = color)
  graphics::text(sf::st_coordinates(inventory)+0.15, labels = 1:nrow(inventory), cex = 0.5)

  shapes = sf::st_buffer(measure, measure$DBH/2*scale)
  color = rep("red", nrow(measure))
  color[!inside] = "gray"
  color[matched$index_measure] = "darkgreen"
  plot(sf::st_geometry(shapes), add = T, col =  color, border = color)
  graphics::text(sf::st_coordinates(measure)-0.15, labels = 1:nrow(measure), cex = 0.5)

  coord_inventory = sf::st_coordinates(inventory)
  coord_measure = sf::st_coordinates(measure)
  coord_inventory = coord_inventory[matched$index_inventory,]
  coord_measure = coord_measure[matched$index_measure,]

  make_line <- function(p1, p2) { sf::st_linestring(rbind(p1, p2))}
  lines <- mapply(make_line, split(coord_inventory, row(coord_inventory)[,1]), split(coord_measure, row(coord_measure)[,1]), SIMPLIFY = FALSE)
  sf_lines <- sf::st_sf(geometry = sf::st_sfc(lines, crs = 32633))  # Adjust CRS as needed

  plot(sf_lines, add = T, col = "green")

  graphics::legend(x = "topleft",
         legend=c("Omission", "Commision", "Matched", "Ground truth", "Outside plot"),
         fill = c("orange","red", "darkgreen" , "black", "gray"))
}

plot_spatial_matching3d = function(treemap)
{
  factor = scale_z_factor(treemap$radius, treemap$inventory$DBH)

  inventory = treemap$inventory
  measure = treemap$measured
  radius = treemap$radius
  center = treemap$center
  match_table = treemap$match_table

  d = as.numeric(sf::st_distance(treemap$measured, treemap$center))
  inside = d < treemap$radius

  matched = match_table[!is.na(match_table$index_inventory),]

  xyz = sf::st_coordinates(inventory) |> as.data.frame()
  xyz$Z = inventory$DBH*factor
  offsetx = mean(xyz[,1])
  offsety = mean(xyz[,2])
  xyz[,1] = xyz[,1] - offsetx
  xyz[,2] = xyz[,2] - offsety

  XYZ = sf::st_coordinates(measure) |> as.data.frame()
  XYZ[,1] = XYZ[,1] - offsetx
  XYZ[,2] = XYZ[,2] - offsety
  XYZ$Z = measure$DBH*factor

  center_x = sf::st_coordinates(center)[1,1] - offsetx
  center_y = sf::st_coordinates(center)[1,2] - offsety
  theta <- seq(0, 2 * pi, length.out = 50)
  cos_theta <- cos(theta)
  sin_theta <- sin(theta)
  radius = 11.28

  for (i in seq_along(center_x))
  {
    xc <- center_x[i]
    yc <- center_y[i]
    r <- radius[i]
    h <- 0

    xx <- xc + r * cos_theta
    yy <- yc + r * sin_theta
    zz <- rep(h, length(theta))

    rgl::lines3d(xx, yy, zz, col = "red", lwd = 3)
  }

  color = rep("orange", nrow(inventory))
  color[matched$index_inventory] = "black"
  rgl::points3d(xyz, color = color, size = 5)

  color = rep("red", nrow(measure))
  color[!inside] = "gray"
  color[matched$index_measure] = "green"
  rgl::points3d(XYZ, color = color, size = 5)

  # Draw lines between matched points
  for (i in seq_len(nrow(matched))) {
    p1 <- as.numeric(XYZ[matched$index_measure[i], ])
    p2 <- as.numeric(xyz[matched$index_inventory[i], ])
    rgl::segments3d(rbind(p1, p2), color = "black", lwd = 1)
  }

  rgl::aspect3d("iso")
  rgl::axes3d()
  pan3d(2)
}

pan3d = function(button, dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(dev))
{
  start <- list()

  begin <- function(x, y)
  {
    activeSubscene <- rgl::par3d("activeSubscene", dev = dev)
    start$listeners <<- rgl::par3d("listeners", dev = dev, subscene = activeSubscene)

    for (sub in start$listeners)
    {
      init <- rgl::par3d(c("userProjection","viewport"), dev = dev, subscene = sub)
      init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
      start[[as.character(sub)]] <<- init
    }
  }

  update <- function(x, y)
  {
    for (sub in start$listeners)
    {
      init <- start[[as.character(sub)]]
      xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
      mouseMatrix <- rgl::translationMatrix(xlat[1], xlat[2], xlat[3])
      rgl::par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
    }
  }
  rgl::rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
}

