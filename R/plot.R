#' Plot a TreeMapMatching object
#'
#' @param x TreeMapMatching object
#' @param y not used
#' @param scale upscale tree diameters for a better readability of the plot
#' @param rgl bool. plot in 3D with rgl
#' @param gg bool. Plot with ggplot instead of base R graphic system
#' @param show_matching bool. Disable matching display
#' @param ... not used
#' @exportS3Method
plot.TreeMapMatching = function(x, y, scale = 1, rgl = FALSE, gg = FALSE, show_matching = TRUE, ...)
{
  if (is.null(x$match_table) || show_matching == FALSE)
  {
    if (rgl)
      compare_plot3d(x)

    if (gg)
    {
      return(compare_plot_gg(x, scale, ...))
    }
    else
      return(compare_plot(x, scale, ...))
  }
  else
  {
    if (rgl)
      plot_spatial_matching3d(x)

    if (gg)
      return(plot_spatial_matching_gg(x, scale, ...))
    else
      return(plot_spatial_matching(x, scale, ...))
  }
}

compare_plot = function(treemap, scale = 1)
{
  inventory = treemap$inventory
  measure = treemap$measured
  radius = treemap$radius
  buffer = treemap$buffer
  center = treemap$center
  inventory_labels = 1:nrow(inventory)
  measure_labels = 1:nrow(measure)

  if (!is.null(inventory$TREEUID))
    inventory_labels = inventory$TREEUID

  if (!is.null(measure$TREEUID))
    measure_labels = measure$TREEUID

  plot(sf::st_buffer(center, radius+buffer), border = "red", lty = 3, axes = TRUE)
  plot(sf::st_buffer(center, radius), border = "red", add = T)
  plot(sf::st_buffer(center, 4), add = T, border = "blue")
  plot(center, add = T, col = "red", cex = 2, pch = 3)

  shapes = sf::st_buffer(inventory, inventory$ZDIM/2*scale)
  plot(sf::st_geometry(shapes), add = T, col = "lightgreen", border = "darkgreen")
  graphics::text(sf::st_coordinates(inventory)-inventory$ZDIM*scale, labels = inventory_labels, cex = 0.5, col = darken("lightgreen"))


  shapes = sf::st_buffer(measure, measure$ZDIM/2*scale)
  plot(sf::st_geometry(shapes), add = T, col = "lightblue", border = "blue")
  graphics::text(sf::st_coordinates(measure)+measure$ZDIM*scale, labels = measure_labels, cex = 0.5, col = darken("blue"))

  graphics::legend(x = "topleft", legend=c("Ground truth", "Measurements"), fill = c("lightgreen","lightblue"), border = c("darkgreen", "blue"))
}

compare_plot_gg = function(treemap, scale = 1)
{
  X <- Y <- NULL

  inventory = treemap$inventory
  measure = treemap$measured
  radius = treemap$radius
  buffer = treemap$buffer
  center = treemap$center
  inventory_labels = 1:nrow(inventory)
  measure_labels = 1:nrow(measure)
  crs = sf::st_crs(inventory)

  if (!is.null(inventory$TREEUID))
    inventory_labels = inventory$TREEUID

  if (!is.null(measure$TREEUID))
    measure_labels = measure$TREEUID

  # Buffers
  buffer_outer <- sf::st_buffer(center, radius + buffer)
  buffer_inner <- sf::st_buffer(center, radius)
  buffer_core  <- sf::st_buffer(center, 4)

  # Circle for each point in inventory and measure
  inventory_shapes <- sf::st_buffer(inventory, inventory$ZDIM / 2 * scale)
  measure_shapes   <- sf::st_buffer(measure,   measure$ZDIM / 2 * scale)

  # Base plot with buffer outlines
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = buffer_outer, fill = NA, color = "red", linetype = 3) +
    ggplot2::geom_sf(data = buffer_inner, fill = NA, color = "red") +
    ggplot2::geom_sf(data = buffer_core,  fill = NA, color = "blue") +
    ggplot2::geom_sf(data = center, color = "red", size = 2, shape = 3)

  # Inventory shapes and labels
  p <- p +
    ggplot2::geom_sf(data = inventory_shapes, fill = "lightgreen", color = "darkgreen") +
    ggplot2::geom_text(data = cbind(inventory, sf::st_coordinates(inventory) - inventory$ZDIM /2 * scale),
                       ggplot2::aes(X, Y, label = inventory_labels),
                       size = 2, color = darken("lightgreen"))

  # Measurement shapes and labels
  p <- p +
    ggplot2::geom_sf(data = measure_shapes, fill = "lightblue", color = "blue") +
    ggplot2::geom_text(data = cbind(measure, sf::st_coordinates(measure) + measure$ZDIM / 2 * scale),
                       ggplot2::aes(X, Y, label = measure_labels),
                       size = 2, color = darken("blue"))

  # Add legend manually
  p + ggplot2::scale_fill_identity(name = "Legend", guide = "legend",
                          labels = c("Ground truth", "Measurements"),
                          breaks = c("lightgreen", "lightblue")) +
    ggplot2::coord_sf(datum = crs) +
    ggplot2::theme_minimal()
}

compare_plot3d = function(treemap)
{
  factor = scale_z_factor(treemap$radius, treemap$inventory$ZDIM)

  inventory = treemap$inventory
  measure = treemap$measured

  xyz = sf::st_coordinates(inventory) |> as.data.frame()
  offsetx = mean(xyz[,1])
  offsety = mean(xyz[,2])
  xyz[,1] = xyz[,1] - offsetx
  xyz[,2] = xyz[,2] - offsety
  xyz$Z = inventory$ZDIM*factor
  XYZ = sf::st_coordinates(measure) |> as.data.frame()
  XYZ[,1] = XYZ[,1] - offsetx
  XYZ[,2] = XYZ[,2] - offsety
  XYZ$Z = measure$ZDIM*factor

  rgl::plot3d(xyz, asp =1, col = "lightgreen", size = 5)
  rgl::aspect3d("iso")
  rgl::points3d(XYZ, asp =1, col = "lightblue", size = 5)
  pan3d(2)
}

plot_spatial_matching = function(treemap, scale = 1)
{
  inventory = treemap$inventory
  measure = treemap$measured
  buffer = treemap$buffer
  radius = treemap$radius
  center = treemap$center
  match_table = treemap$match_table
  match_table = stats::na.omit(match_table)

  inventory_labels = 1:nrow(inventory)
  measure_labels = 1:nrow(measure)

  if (!is.null(inventory$TREEUID))
    inventory_labels = inventory$TREEUID

  if (!is.null(measure$TREEUID))
    measure_labels = measure$TREEUID


  matched = match_table[!is.na(match_table$index_inventory),]

  # Plotting circles and center
  plot(sf::st_buffer(center, radius+buffer), border = "red", lty = 3, axes = TRUE)
  plot(sf::st_buffer(center, radius), border = "red", add = T)
  plot(sf::st_buffer(center, 4), add = T, border = "blue")
  plot(center, add = T, col = "red", cex = 2, pch = 3)

  # Plotting inventory in black
  d = as.numeric(sf::st_distance(treemap$inventory, treemap$center))
  inside = d < treemap$radius
  color = rep("orange", nrow(inventory))
  color[matched$index_inventory] = "black"
  color[!inside] = "gray"
  shapes = sf::st_buffer(inventory, inventory$ZDIM/2*scale)
  plot(sf::st_geometry(shapes), add = T, col = color, border = color)
  graphics::text(sf::st_coordinates(inventory)-inventory$ZDIM/2*scale*1.25, labels = inventory_labels, cex = 0.5, col = darken(color))

  # Plotting lidar measure in green
  d = as.numeric(sf::st_distance(treemap$measured, treemap$center))
  inside = d < treemap$radius

  shapes = sf::st_buffer(measure, measure$ZDIM/2*scale)
  color = rep("red", nrow(measure))
  color[!inside] = "gray"
  color[matched$index_measure] = "chartreuse3"
  plot(sf::st_geometry(shapes), add = T, col =  color, border = color)
  graphics::text(sf::st_coordinates(measure)+measure$ZDIM/2*scale*1.25, labels = measure_labels, cex = 0.5, col = darken(color))

  coord_inventory = sf::st_coordinates(inventory)
  coord_measure = sf::st_coordinates(measure)
  coord_inventory = coord_inventory[matched$index_inventory,]
  coord_measure = coord_measure[matched$index_measure,]
  cost = matched$cost

  make_line <- function(p1, p2) { sf::st_linestring(rbind(p1, p2))}
  lines <- mapply(make_line, split(coord_inventory, row(coord_inventory)[,1]), split(coord_measure, row(coord_measure)[,1]), SIMPLIFY = FALSE)
  sf_lines <- sf::st_sf(geometry = sf::st_sfc(lines, crs = 32633))  # Adjust CRS as needed

  plot(sf_lines, add = T, col = "blue2")

  if (!is.null(cost))
  {
    line_center = sf::st_centroid(sf_lines)
    graphics::text(sf::st_coordinates(line_center)-0.15, labels = round(cost,1), cex = 0.5, col = "blue2")
  }

  graphics::legend(x = "topleft",
                   legend=c("Omission", "Commision", "Matched", "Ground truth", "Outside plot"),
                   fill = c("orange","red", "darkgreen" , "black", "gray"))
}

plot_spatial_matching3d = function(treemap)
{
  zrel = attr(treemap$match_table, "zrel")
  if (is.null(zrel)) zrel = 1

  factor = scale_z_factor(treemap$radius, treemap$inventory$ZDIM, zrel/100)

  inventory = treemap$inventory
  measure = treemap$measured
  radius = treemap$radius
  center = treemap$center
  match_table = treemap$match_table
  match_table = stats::na.omit(match_table)

  inventory_labels = 1:nrow(inventory)
  measure_labels = 1:nrow(measure)

  if (!is.null(inventory$TREEUID))
    inventory_labels = inventory$TREEUID

  if (!is.null(measure$TREEUID))
    measure_labels = measure$TREEUID

  d = as.numeric(sf::st_distance(treemap$measured, treemap$center))
  inside = d < treemap$radius

  matched = match_table[!is.na(match_table$index_inventory),]

  xyz = sf::st_coordinates(inventory) |> as.data.frame()
  xyz$Z = inventory$ZDIM*factor
  offsetx = mean(xyz[,1])
  offsety = mean(xyz[,2])
  xyz[,1] = xyz[,1] - offsetx
  xyz[,2] = xyz[,2] - offsety

  XYZ = sf::st_coordinates(measure) |> as.data.frame()
  XYZ[,1] = XYZ[,1] - offsetx
  XYZ[,2] = XYZ[,2] - offsety
  XYZ$Z = measure$ZDIM*factor

  center_x = sf::st_coordinates(center)[1,1] - offsetx
  center_y = sf::st_coordinates(center)[1,2] - offsety
  theta <- seq(0, 2 * pi, length.out = 50)
  cos_theta <- cos(theta)
  sin_theta <- sin(theta)
  radius = treemap$radius

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
  color[matched$index_measure] = "chartreuse4"
  rgl::points3d(XYZ, color = color, size = 5)

  # Draw lines between matched points
  for (i in seq_len(nrow(matched))) {
    p1 <- as.numeric(XYZ[matched$index_measure[i], ])
    p2 <- as.numeric(xyz[matched$index_inventory[i], ])
    rgl::segments3d(rbind(p1, p2), color = "blue", lwd = 1)
  }

  rgl::aspect3d("iso")
  rgl::axes3d()
  pan3d(2)
}

plot_spatial_matching_gg = function(treemap, scale = 1)
{
  X <-Y <- Z <- label <-x <-y <- NULL
  inventory = treemap$inventory
  measure = treemap$measured
  radius = treemap$radius
  center = treemap$center
  match_table = treemap$match_table
  match_table = stats::na.omit(match_table)

  inventory_labels = 1:nrow(inventory)
  measure_labels = 1:nrow(measure)

  if (!is.null(inventory$TREEUID))
    inventory_labels = inventory$TREEUID

  if (!is.null(measure$TREEUID))
    measure_labels = measure$TREEUID


  # Compute distances and determine which measured points are inside the plot radius
  d <- as.numeric(sf::st_distance(treemap$measured, treemap$center))
  inside <- d < treemap$radius

  # Filter matched indices
  matched <- match_table[!is.na(match_table$index_inventory), ]
  cost = matched$cost

  # Assign colors and labels to inventory points
  inventory$plot_color <- "orange"
  inventory$plot_color[matched$index_inventory] <- "black"
  inventory$label <- inventory_labels

  # Assign colors and labels to measured points
  measure$plot_color <- "red"
  measure$plot_color[!inside] <- "gray"
  measure$plot_color[matched$index_measure] <- "chartreuse4"
  measure$label <- measure_labels

  # Create circle geometries
  shapes_inventory <- sf::st_buffer(inventory, inventory$ZDIM / 2 * scale)
  shapes_measure <- sf::st_buffer(measure, measure$ZDIM / 2 * scale)

  # Convert to sf objects with color and label columns
  shapes_inventory <- sf::st_sf(geometry = sf::st_geometry(shapes_inventory), color = inventory$plot_color, label = inventory$label)
  shapes_measure <- sf::st_sf(geometry = sf::st_geometry(shapes_measure), color = measure$plot_color, label = measure$label)

  # Create line geometries for matched pairs
  coord_inventory <- sf::st_coordinates(inventory)[matched$index_inventory, ]
  coord_measure <- sf::st_coordinates(measure)[matched$index_measure, ]

  make_line <- function(p1, p2) sf::st_linestring(rbind(p1, p2))
  lines <- mapply(make_line,
                  split(coord_inventory, row(coord_inventory)[, 1]),
                  split(coord_measure, row(coord_measure)[, 1]),
                  SIMPLIFY = FALSE)

  sf_lines <- sf::st_sf(geometry = sf::st_sfc(lines, crs = sf::st_crs(inventory)))

  line_center = NULL
  if (!is.null(cost))
  {
    line_center = sf::st_centroid(sf_lines)
    line_center = as.data.frame(sf::st_coordinates(line_center)-0.15)
    line_center$cost = round(cost,1)
  }

  # Prepare coordinate data frames for text labels
  coords_inv <- sf::st_coordinates(inventory) - inventory$ZDIM/2*scale *1.25
  coords_inv_df <- data.frame(X = coords_inv[, 1], Y = coords_inv[, 2], label = shapes_inventory$label)
  coords_inv_df$plot_color = inventory$plot_color

  coords_mea <- sf::st_coordinates(measure) + measure$ZDIM/2*scale *1.25
  coords_mea_df <- data.frame(X = coords_mea[, 1], Y = coords_mea[, 2], label = shapes_measure$label)
  coords_mea_df$plot_color = measure$plot_color

  legend_df <- data.frame(
    x = as.numeric(sf::st_coordinates(center)[1,1]), y = as.numeric(sf::st_coordinates(center)[1,2]),
    class = factor(c("Omission", "Commission", "Matched", "Ground truth", "Outside plot"),
                   levels = c("Omission", "Commission", "Matched", "Ground truth", "Outside plot"))
  )

  crs = sf::st_crs(inventory)

  # Generate ggplot
  p = ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf::st_buffer(treemap$center, treemap$radius+treemap$buffer), fill = NA, color = "red", linetype = 3) +
    ggplot2::geom_sf(data = sf::st_buffer(treemap$center, treemap$radius), fill = NA, color = "red") +
    ggplot2::geom_sf(data = sf::st_buffer(treemap$center, 4), fill = NA, color = "blue") +
    ggplot2::geom_sf(data = sf_lines, color = "blue2", linewidth = 0.25) +
    ggplot2::geom_sf(data = treemap$center, shape = 3, size = 3, color = "red") +
    ggplot2::geom_sf(data = shapes_inventory, fill = shapes_inventory$color) +
    ggplot2::geom_text(data = coords_inv_df, ggplot2::aes(x = X, y = Y, label = label), color = coords_inv_df$plot_color, size = 2) +
    ggplot2::geom_sf(data = shapes_measure, fill = shapes_measure$color, alpha = 0.8) +
    ggplot2::geom_text(data = coords_mea_df, ggplot2::aes(x = X, y = Y, label = label), color = coords_mea_df$plot_color, size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "gray95")) +
    ggplot2::geom_point(data = legend_df, ggplot2::aes(x = x, y = y, fill = class), shape = 21, size = 2) +
    ggplot2::coord_sf(datum = crs) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c(
        "Omission" = "orange",
        "Commission" = "red",
        "Matched" = "chartreuse4",
        "Ground truth" = "black",
        "Outside plot" = "gray"
      )
    ) +
    ggplot2::theme(
      legend.position = c(0.02, 0.98),              # Position: top-left in normalized coordinates
      legend.justification = c("left", "top"),      # Anchor corner of the legend box
      legend.background = ggplot2::element_rect(    # White background box with black border
        fill = "white",
        color = "black",
        linewidth = 0.2
      ),
      legend.key = ggplot2::element_rect(           # Optional: clean keys
        fill = "white",
        color = NA
      ),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      legend.key.size = ggplot2::unit(1, "lines")
    )

  if (!is.null(line_center))
  {
    p = p +
      ggplot2::geom_text(data = line_center, ggplot2::aes(x = X, y = Y, label = cost), color = "blue2", size = 2)

  }

  p
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

darken <- function(colors, factor = 1.5)
{
  factor <- max(factor, 1)

  rgb <- grDevices::col2rgb(colors) / 255
  rgb_dark <- rgb / factor

  rgb(rgb_dark[1, ], rgb_dark[2, ], rgb_dark[3, ])
}

