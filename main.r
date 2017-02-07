# main
rm(list=ls())

# Load functions to manage local Rdata copies of data if available
# Also installs required packages if necesary
source("rdata_utils.r")

# Load global_config which creates the following objects:
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
score_change_groups2 <- list("Child_ID")

# Create a data.table of percentage point score change for each child within dev stage 
score_dt <- score_data(d, score_change_groups)

# Create a data.table of percentage point score change for each child ignoring  dev stage 
score_dt2 <- score_data(d, score_change_groups2)

# Create a dataframe holding results formatted for displaying in bulleted summary
# first_last_pupil_shares <- pupil_share_Dev_Stage(score_dt2, unlist(get_devstrand_categories(style_guide)))
# save(first_last_pupil_shares, file=file.path("rdata", "first_last_pupil_shares.Rdata"))

# Create a data.table of percentage point score change data for each child within dev stage
score_change_dt <- score_change_data(score_dt, score_change_groups)

# Load in bar chart and pie chart plotting functions
source("plot_functions.r")

# Remove existing plots
delete_existing_plots()

# Create png files of all charts of type "bar_stacked"
for(i in reports) {
    for(j in i){
        if(grepl("bar", j[["type"]])) {
            do.call(get("bar_chart"), c(j, "data_set_name"="score_change_dt"))
        } else if(grepl("pie", j[["type"]])) {
            do.call(get("pie_chart"), c(j, "data_set_name"="score_dt2"))
        } else if(grepl("data", j[["type"]])) {
            do.call(get("pupil_shares_data"), c(j, "data_set_name"="score_dt2", get_devstrand_categories(style_guide)))
        }
    }
}
