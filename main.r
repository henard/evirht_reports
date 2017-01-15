# main

working_dir = "/home/henard/dev/r/evirht"
setwd(working_dir)
# getwd()


# Load report_config which creates the following objects:
#   data_source
#   global_filters
#   report
source("report_config.r")

print(data_source)
print(global_filters)
print(report)
print(where_clause)

# Load global_config which creates the following objects:
#   style_guide
#   devstrand_colour_palette
#   devstrand_categories
source("global_config.r")
print(style_guide)
print(devstrand_colour_palette)
print(devstrand_categories)

# Load global_config which creates the following objects:
#   db_credentials
source("../../db_credentials.r")
credentials <- db_credentials[[data_source]]

source("get_data.r")

d$c <- 1
# xtabs(c ~ chip_version + map_grade, data=d)

summary(d)



