plot_id = "PRF002"
plot_id = "PRF025"
plot_id = "PRF036"
plot_id = "PRF133"
plot_id = "PRF184"
plot_id = "PRF185"
plot_id = "PRF193"
plot_id = "PRF200"

finventory = "/home/jr/Téléchargements/ALS_TLS_GJ/Bastien/Matching_MLS_FI/FI/FieldData_2025_Aligned_UTM.csv"
all_inventory <- read.csv(finventory)
inventory = all_inventory[all_inventory$Plot == plot_id,]

fmeasure = file.path("/home/jr/Téléchargements/ALS_TLS_GJ/Bastien/Matching_MLS_FI/MLS/", paste0(plot_id, "_DBH_HTs.csv"))
measure <- read.csv(fmeasure)

inventory = standardize(inventory, "Field_Xpj", "Field_Ypj", "DBH", dbhunits = "m", crs = 2959)
measure   = standardize(measure, "X", "Y", "DBH", dbhunits = "cm", crs = 2959)
center    = c(inventory$Easting[1], inventory$Northing[1])

treemap = make_mapmatching(inventory, measure, center = center, radius = 11.5)

plot(treemap, scale = 2)

treemap = match_trees(treemap, bidirectionnal_double_matching, dmax = 2, dz = 0.1)
plot(treemap, scale = 2)
plot(treemap, scale = 2, rgl = TRUE)

treemap = match_trees(treemap, lsap_matching, dmax = 2, dz = 0.1)
plot(treemap, scale = 2)
plot(treemap, scale = 2, rgl = TRUE)

