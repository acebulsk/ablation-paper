# create a site map for the windswept subalpine forest site at fortress

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(tmap)

# MAIN MAP ----

# bring in uav rgb raster
# bg_full <- rast('data/gis/22_292FT_RGB.tif')

# Resample to 25 cm resolution using mean (or another function)
# bg_25cm <- aggregate(bg_full, fact=10, fun=mean)  # you can also use 'max', 'min', etc.

# Save or plot
# plot(bg_25cm)
# writeRaster(bg_25cm, 'data/gis/22_292FT_RGB_resamp_25cm.tif', overwrite=TRUE)

bg_resamp <- rast('data/gis/22_292FT_RGB_resamp_25cm.tif')
bad_names <- c('EC low',
               'SR50')

scl_name_dict <- data.frame(name = c('SCL 1', 'SCL 2', 'SCL 3', 'TB1', 'TB2', 'TB3', 'TB4'),
                            new_name = c('SCL Mixed', 'SCL Sparse', 'SCL Dense', 'TB 1', 'TB 2', 'TB 3', 'TB 4'))

inst_coords <- sf::read_sf('../../analysis/interception/data/lai/instrument_coords.gpkg') |>
  filter(!name %in% bad_names) |>
  st_transform(st_crs(bg_resamp)) |>
  mutate(name = gsub('Trough', 'SCL', name)) |>
  left_join(scl_name_dict, by = 'name') |>
  select(name = new_name, type, geometry = geom)

stns <- data.frame(
  name = c('PWL Station', 'FT Station'),
  type = 'Flux Tower',
  x = c(626890.966, 627006.643),
  y = c(5632024.896, 5631995.019)) |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(bg_resamp)) |> rbind(inst_coords)

# st_write(stns, 'data/gis/stn_coords.gpkg')


bbox <- st_bbox(stns)
buffer_dist <- 10
bbox_buffered <- bbox
bbox_buffered["xmin"] <- bbox["xmin"] - buffer_dist
bbox_buffered["xmax"] <- bbox["xmax"] + buffer_dist
bbox_buffered["ymin"] <- bbox["ymin"] - buffer_dist
bbox_buffered["ymax"] <- bbox["ymax"] + buffer_dist+50
# bbox['xmax'] <- bbox['xmax'] + 50
# bbox['ymax'] <- bbox['ymax'] + 50
main_map <- tm_shape(bg_resamp, bbox = bbox_buffered) +
  tm_rgb()  +
  tm_graticules(
    ticks = TRUE,
    lines = FALSE,
    n.x = 2,
    n.y = 1
  ) +
  # tm_shape(fsr_plots) +
  # tm_polygons('plot_name', palette = c('salmon', 'dodgerblue'), title = '', fill_alpha = .5) +
  # tm_lines(col = 'blue', lty = 'dashed', lwd = 2, legend.col.show = T) +
  # tm_shape(ss_transect_path_rough) +
  # tm_lines(col = 'orange', lty = 'solid', lwd = 2) +
  tm_shape(stns) +
  tm_symbols(size = 1,
             # size.scale = tm_scale_continuous(values.scale = 1.25),
             # size.legend.show = FALSE,  # hide size from legend
             fill = 'name',
             shape = 'type',
             fill.scale = tm_scale_categorical(values = cols4all::c4a('carto.safe'))
             ) +
  tm_scalebar(position = c(-0.025, 0)) +
  tm_compass(position = c(0, 0.15)) +
  tm_layout(
    # legend.frame = 'black',
    legend.bg.color = 'antiquewhite',
    legend.position = c('right', 'top')
    # legend.outside = T
    # outer.margins = 0.05
  ) 

main_map

tmap::tmap_save(main_map, 'figs/study-site/site_map.png', height = 6, unit = 'in')

# INSET MAP ----

library(sf)
library(tidyverse)
library(tmap)
library(terra)
library(grid)

data(land)

land_ele <- terra::rast(land)[[4]]

sites <- data.frame(
  site = c('Wolf Creek', 'Russell Creek', 'Fortress Mountain', 'Marmot Creek'),
  lon = c(-135.1497, -126.3090,-115.1983, -115.155154),
  lat = c(60.567, 50.3710, 50.8269, 50.9255877)
) |>
  filter(site == 'Fortress Mountain')

sites_sf <- sf::st_as_sf(sites, coords = c('lon', 'lat'), crs = st_crs(land))

# sf::write_sf(sites_sf, 'data/gis/site_coords.gpkg')

site_sf_buff <- st_buffer(sites_sf, dist = 2500000)
bb <- st_bbox(site_sf_buff)

canusa <-
  spData::world %>%
  dplyr::filter(name_long %in% c('Canada', 'United States', 'Mexico')) |>
  st_transform(st_crs(land))

land_crop <- terra::crop(land_ele, canusa)

bb_canusa <- st_bbox(st_buffer(canusa , dist = 100))

inset_map <-
  # tm_shape(land_crop, bbox = bb) +
  # tm_raster(
  #   "elevation_elevation",
  #   palette = terrain.colors(10),
  #   breaks = c(0, 100, 300, 700, 1500, 2500, 4500),title = 'Elevation (m)'
  # ) +
  # tm_shape(canusa, bbox = bb) +
  # tm_borders() +
  tm_shape(canusa, bbox = bb) +
  tm_polygons() +
  tm_shape(sites_sf) +
  tm_dots(col = 'red', size = 0.25) +
  tm_text("site", size = 0.75, options = opt_tm_text(point.label = F, just = 'bottom'), ymod = 0.1) +
  # tm_graticules(n.x = 3, n.y = 4, ) +
  tm_layout(legend.position = c('left', 'bottom'))
inset_map
tmap::tmap_save(inset_map, 'figs/study-site/site_map_na_scale.png', width = 3)

# put inset on main plot ----

norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(c(w, h))
}

ins_dim = norm_dim(site_sf_buff)

ins_vp <- viewport(width = ins_dim[1] * 0.4, height = ins_dim[2] * 0.4,
                  x = unit(1.7, "in"), y = unit(5, "in"))

tmap::tmap_save(
  main_map,
  filename = 'figs/final/figure3.png',
  # filename = 'figs/study-site/site_map_inset.png',
  # width = 6,
  height = 6,
  unit = 'in',
  insets_tm = inset_map,
  insets_vp = ins_vp
)

