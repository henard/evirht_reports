# main
rm(list=ls())
setwd(file.path("C:", "Users", "Henard", "dev", "r", "evirht_reports"))

required_packages = c("RODBC", "data.table", "plyr", "ggplot2", "scales")
for(package in required_packages) {
    if(!(package %in% installed.packages()[,"Package"])) install.packages(package)  
}

# Load functions to manage local Rdata copies of data
source("rdata_utils.r")

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

# Load functions to get data
source("get_data.r")

# Load in data transform functions
source("data_transforms.r")

# Read in data from source defined in data_source in global_config.r
# apply formating to variables and calculate analysis variables
d <- read_data(data_source)

# Define groups within which percentage point change in assessment scores is to be calculated
score_change_groups <- list("Child_ID", "Dev_Stage")

# Create a data.table of scores
score_dt <- score_data(d, score_change_groups)

# Create a data.table of percentage point change data
score_change_dt <- score_change_data(score_dt, score_change_groups)

# head(score_change_dt)
source("plot_functions.r")

# Create png files of all charts of type "bar_stacked"
for(i in reports) {
    for(j in i){
        if(grepl("bar", j[["type"]])) {
            do.call(get("bar_chart"), c(j, "data_set_name"="score_change_dt"))
        }
    }
}

# j <- reports[["report1"]][["chart1"]]
# do.call(get("bar_chart"), c(j, "data_set_name"="score_change_dt"))
