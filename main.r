# main
rm(list=ls())
setwd(file.path("C:", "Users", "Henard", "dev", "r", "evirht_reports"))

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

# j <- reports[[1]][[2]]
# j
# do.call(get(j[["type"]]), c(j, "data_set_name"="score_change_dt"))    
# 
j <- list("type"="bar_side_by_side",
              "title"="TEST TEST",
              "measure"="Overall_Score",
              "xaxis"="Assessment_n",
              "xgroup"="School_Year_Child_ID",
              "colour_by"="Dev_Stage",
              "filter"=list("Organisation_Nine"=list("column"="Organisation", "value"="Nine", "filter_type"="contains"),
                            "Assessment_n_2+"=list("column"="N_assessments", "lower"=2, "upper"=3, "filter_type"="range")))

do.call(get("bar_chart"), c(j, "data_set_name"="score_dt"))

j <- list("type"="pie",
          "title"="PIE TEST",
          "measure"="c",
          "xgroup"="",
          "colour_by"="Dev_Stage",
          "filter"=list("Assessment_n_2+"=list("column"="N_assessments", "lower"=2, "upper"=3, "filter_type"="range")))

do.call(get("pie_chart"), c(j, "data_set_name"="score_dt"))
