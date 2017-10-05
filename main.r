# main.r - click on [source] to run this script to create the charts and data needed for the pdf reports.

# Clear all results in memory from last time.
rm(list=ls())

# Install required packages if necessary.
source("install_rpackages.r")

# Load global_config which creates the following objects:
#   data_source
#   global_filters
#   style_guide
source("global_config.r")

# Load report_config which creates the following objects:
#   report
source("report_config.r")

# Load functions to manage local RData copies of data if available
source("rdata_utils.r")

# Load functions to get data
source("get_data.r")

# Load in data transform functions
source("data_transforms.r")

# Read in assessment data from source defined in data_source in global_config.r
# apply formating to variables and calculate analysis variables.
d <- read_data(data_source)

# Read in pupil_counts data from source defined in data_source in global_config.r
# apply formating to variables and calculate analysis variables.
pupil_counts <- read_pupil_counts_data(data_source)

# Add automatically generated chart tiles
reports <- add_auto_chart_labels2(reports)

# Update report_config to reorder charts (in report 1)
reports <- reordered_report_config(reports, list("report1"))

# Define groups within which percentage point change in assessment scores are to be calculated.
score_change_groups <- list("Child_ID", "Dev_Stage")
score_change_groups2 <- list("Child_ID")

# Create a data.table of percentage point score change for each child:
score_dt <- score_data(d, score_change_groups)     # within dev stage 
score_dt2 <- score_data(d, score_change_groups2)   # ignoring dev stage 

# Create a data.table of percentage point score change data for each child within dev stage.
score_change_dt <- score_change_data(score_dt, score_change_groups)

# Load in bar chart and pie chart plotting functions.
source("plot_functions.r")

# Remove existing plots.
delete_existing_plots()

# Create .png files of all charts and .RData results data files in report.
options(warn=-1)
for(i in reports) {
    for(j in i){
        if(grepl("bar", j[["type"]])) {
            do.call(get("bar_chart"), j)
        } else if(grepl("pie", j[["type"]])) {
            do.call(get("pie_chart"), j)
        } else if(grepl("data", j[["type"]])) {
            do.call(get("pupil_shares_data"), c(j, get_devstrand_categories(style_guide)))
        }
    }
}
options(warn=0)

print("main.r completed running")
