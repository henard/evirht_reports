# main
rm(list=ls())

# Load global_config which sets working directory and creates the following objects:
#   data_source
#   global_filters
#   style_guide
#   devstrand_colour_palette
#   devstrand_categories
source("global_config.r")
print(style_guide)

# Load report_config which creates the following objects:
#   report
source("report_config.r")
print(data_source)
print(report_filters)
print(reports)

# Load db_credentials which creates the following objects:
#   db_credentials
# No longer needed now we have ~/.my
# source("../db_credentials.r")

# Get data: a dataframe called d. (data will be grabbed from source defined in data_source in global_config.r)
source("get_data.r")

# Load in data transform functions
source("data_transforms.r")

d <- format_dataframe(d)

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
