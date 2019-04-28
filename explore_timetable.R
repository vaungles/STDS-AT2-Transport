library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(rworldmap)
library(collections)
library(tuple)
library(lubridate)
library(ggmap)
library(svMisc)
library(doParallel)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

setwd("C:/Users/Mactuary/OneDrive - UTS/36103/AT2")
data_dir <- paste(getwd(), "Data", sep = "/")
data_dir

timetable_dir <- paste(data_dir, "Timetable", sep = "/")

filepaths <- grep("*.txt", list.files(timetable_dir), value = TRUE)
filepaths

data_names <- str_sub(filepaths, 1, -5)
data_names

filepaths <- map(filepaths, function(x) paste(timetable_dir, x, sep = "/"))
names(filepaths) <- data_names
filepaths

# Defining columng types
cols_list <- list(agency = cols(agency_id = col_character(),
                                agency_name = col_character(),
                                agency_url = col_character(),
                                agency_timezone = col_character(),
                                agency_lang = col_character(),
                                agency_phone = col_double()),
                  calendar = cols(service_id = col_character(),
                                  monday = col_factor(),
                                  tuesday = col_factor(),
                                  wednesday = col_factor(),
                                  thursday = col_factor(),
                                  friday = col_factor(),
                                  saturday = col_factor(),
                                  sunday = col_factor(),
                                  start_date = col_date(format = "%Y%m%d"),
                                  end_date = col_date(format = "%Y%m%d")),
                  calendar_dates = cols(service_id = col_character(),
                                        date = col_date(format = "%Y%m%d"),
                                        exception_type = col_factor()),
                  routes = cols(route_id = col_character(),
                                agency_id = col_character(),
                                route_short_name = col_character(),
                                route_long_name = col_character(),
                                route_desc = col_factor(),
                                route_type = col_factor(),
                                route_color = col_factor(),
                                route_text_color = col_factor()),
                  shapes = cols(shape_id = col_character(),
                                shape_pt_lat = col_double(),
                                shape_pt_lon = col_double(),
                                shape_pt_sequence = col_integer(),
                                shape_dist_traveled = col_double()),
                  stop_times = cols(trip_id = col_character(),
                                    arrival_time = col_character(),
                                    departure_time = col_character(),
                                    stop_id = col_character(),
                                    stop_sequence = col_integer(),
                                    stop_headsign = col_factor(),
                                    pickup_type = col_factor(),
                                    drop_off_type = col_factor(),
                                    shape_dist_traveled = col_double()),
                  stops = cols(stop_id = col_character(),
                               stop_code = col_double(),
                               stop_name = col_character(),
                               stop_lat = col_double(),
                               stop_lon = col_double(),
                               location_type = col_factor(),
                               parent_station = col_character(),
                               wheelchair_boarding = col_factor(),
                               platform_code = col_factor()),
                  trips = cols(route_id = col_character(),
                               service_id = col_character(),
                               trip_id = col_character(),
                               shape_id = col_character(),  
                               trip_headsign = col_character(),
                               direction_id = col_factor(),
                               block_id = col_character(),
                               wheelchair_accessible = col_factor())
)

files_df <- tibble(data = data_names,
                   filepath = filepaths,
                   column_def = cols_list)
glimpse(files_df)

data <- map2(files_df$filepath, files_df$column_def, ~readr::read_csv(.x, col_types = .y))

names(data) <- data_names
map(data, glimpse)

data$stop_times %<>%
  mutate(arrival_time = hms(arrival_time),
         departure_time = hms(departure_time))

glimpse(data$stop_times)

# Calculating the level of Public Transport In a Given Area

joined <- data$stops %>%
  inner_join(data$stop_times, by = 'stop_id') %>%
  inner_join(data$trips, by = 'trip_id') %>%
  inner_join(data$routes, by = 'route_id')
head(joined)
glimpse(joined)

count_by_type <- joined %>%
  group_by(stop_id, route_type) %>%
  summarize(num_service = n()) %>%
  select(stop_id, 
         route_type,
         num_service)
glimpse(count_by_type)
head(count_by_type)

# SHAPES; assigning each stop_id to a particular LGA or locality
nsw_locality_polygon_shp_path = paste(data_dir, "Map/nsw_locality_polygon_shp/NSW_LOCALITY_POLYGON_shp.shp", sep = "/")
nsw_lga_polygon_shp_path = paste(data_dir, "Map/nsw_lga_polygon_shp/NSW_LGA_POLYGON_shp.shp", sep = "/")

nsw_locality_polygon_shp <- rgdal::readOGR(nsw_locality_polygon_shp_path)
nsw_lga_polygon_shp <- rgdal::readOGR(nsw_lga_polygon_shp_path)

nsw_lga_polygon_tbl <- as.tbl(nsw_lga_polygon_shp@data)
class(nsw_lga_polygon_tbl)
glimpse(nsw_lga_polygon_tbl)

num_stops <- nrow(data$stops)
num_stops

# Looping over the LGAs
num_lgas <- nrow(nsw_lga_polygon_shp@data)
num_lgas

coords_list = list()
for(lga in 1:num_lgas){
  coords = nsw_lga_polygon_shp@polygons[[lga]]@Polygons[[1]]@coords
  coords_list[[lga]] = coords
}

nsw_lga_polygon_tbl$polygon = coords_list
glimpse(nsw_lga_polygon_tbl)

# Get the stop_id and coordinates in a data.frame
stop_coords_df <- data$stops %>%
  select(stop_id, stop_lon, stop_lat)
head(stop_coords_df)

rownames(stop_coords_df) <- 1:num_stops

# Calculate to which LGA a particular stop_id belongs
in_polygon = list()
Sys.time()
for(lga in 1:num_lgas){
  progress(lga)
  polygon_coords = nsw_lga_polygon_tbl$polygon[[lga]]
  in_polygon[[lga]] = sp::point.in.polygon(stop_coords_df$stop_lon, 
                                           stop_coords_df$stop_lat,
                                           polygon_coords[, 1],
                                           polygon_coords[, 2])
}
Sys.time()


in_polygon_df <- data.frame(in_polygon)
# Add the lga number (in the order given in the original data)
names(in_polygon_df) <- 1:num_lgas
# Add the stop_id column
in_polygon_df$stop_id <- data$stops$stop_id

write.csv(in_polygon_df, file = paste(getwd(), "Output/in_polygon.csv", sep = "/"))

# Check that for each stop (row), the sum is 1, i.e. that it only belongs to one LGA
stop_id_total_lga <- in_polygon_df %>% 
  select(-stop_id) %>% 
  apply(1, function(x) sum(x != 0))
table(stop_id_total_lga)

# 0: 189 stops don't belong to an LGA
# 1: 43927 stops that are strictly within an LGA
# 2: 77 stops lies that on the relative interios of an edge of a polygon
# 3: 0 stops are the vertex of a polygon

# For each stop_id, get the LGA number, i.e. 1:197.
stop_id_lga_map <- list()
for(stop in 1:num_stops){
  index <- which(in_polygon_df[stop, ] == 1)

  # Consider only the first lga
  if(length(index) == 1)
    stop_id_lga_map[[stop]] <- index
  else{
    stop_id_lga_map[[stop]] <- index[1]
  }
}

# Append the stop_id_total_lga to the stop_coord_df
stop_coords_df$total_lga <- stop_id_total_lga

# Append the stop_id_lga_map to the stop_coord_df
stop_coords_df$lga_number <- unlist(stop_id_lga_map)

# Join the three tables:
# 1. stop_coords_df
# 2. nsw_lga_polygon_tbl (the table containing the LGA information), excluding the polygon

stop_coords_df %<>% 
  mutate(lga_number = as.character(lga_number)) %>% 
  left_join(rownames_to_column(nsw_lga_polygon_tbl), by = c("lga_number" = "rowname")) %>% 
  select(-polygon)

# Join stop_coords_df with count_by_type to get the route_type and the
# num_service for each stop_id.

final_stop_count_coords_df <- stop_coords_df %>% 
  left_join(count_by_type, by = 'stop_id')

write.csv(final_stop_count_coords_df, 
          file = paste(getwd(), "Output/final_stop_coords_df.csv", sep = "/"))
