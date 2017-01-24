# main
rm(list=ls())

# Load global_config which sets working directory and creates the following objects:
#   data_source
#   global_filters
#   style_guide
#   devstrand_colour_palette
#   devstrand_categories
source("global_config.r")

# Load report_config which creates the following objects:
#   report
source("report_config.r")

# Load db_credentials which creates the following objects:
#   db_credentials
# No longer needed now we have ~/.my
# source("../db_credentials.r")

# Load functions to get data
source("get_data.r")

# Load in data transform functions
source("data_transforms.r")

# Read in data from source defined in data_source in global_config.r
d <- read_data(data_source)

# Define groups within which percentage point change in assessment scores is to be calculated
score_change_groups <- list("Child_ID", "Dev_Stage")

# Create a data.table of percentage point change data
score_change_dt <- score_change_data(d, score_change_groups)

# head(score_change_dt)
source("plot_functions.r")

for(i in reports) {
    for(j in i){
        if(j[["type"]]=="bar_stacked") {
            do.call(get(j[["type"]]), c(j, "data_set_name"="score_change_dt"))    
        }
    }
}
