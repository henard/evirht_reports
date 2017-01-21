# main

# Load global_config which sets working directory and creates the following objects:
#   data_source
#   global_filters
#   style_guide
#   devstrand_colour_palette
#   devstrand_categories
source("global_config.r")
print(style_guide)
print(devstrand_colour_palette)
print(devstrand_categories)

# Load report_config which creates the following objects:
#   report
source("report_config.r")
print(data_source)
print(report_filters)
print(report)
print(where_clause)

# Load db_credentials which creates the following objects:
#   db_credentials
# No longer needed now we have ~/.my
# source("../db_credentials.r")

# Get data. (data will be grabbed from source defined in data_source in global_config.r)
source("get_data.r")

# d$c <- 1
# xtabs(c ~ chip_version + map_grade, data=d)
# summary(d)
library(data.table)
library(plyr)

# library(ggplot2)
# data(diamonds)

# Helper function
parseify_list <- function(list_of_colnames)
    return(as.quoted(paste("list(", paste(list_of_colnames, collapse=", "),")"))[[1]])

# Convert to data.table
DT <- data.table(d)
DT[, c:=1]


# Start function here

# function inputs
by_cols <- list("Child_ID", "Dev_Stage")


# prep function inputs and wire-in
by_cols_parse <- parseify_list(by_cols)

# rank within groups
DT[, Assessment_n := rank(Completed_Date, ties.method="first"), by=by_cols_parse]
DT[, .("N" = sum(c)), by=.(Assessment_n)]

# Max rank within groups
DT[, N_assessments := max(Assessment_n), by=by_cols_parse]
DT[, .("N" = sum(c)), by=.(N_assessments)]

# rank within groups reverse
DT[, Assessment_n_rev := -1*(N_assessments-Assessment_n)-1]
DT[, .("N" = sum(c)), by=.(Assessment_n_rev)]


t1 <- DT[Assessment_n==1 | Assessment_n_rev ==-1, ppt_change := max(Overall_Score)-min(Overall_Score), by=by_cols_parse][Assessment_n==1]

head(t1)

# data.table reference stuff
# DT[, Child_ID]
# setkey(DT, assess_n, assess_n_rev)
# nrow(DT)
# t1 <- DT[assess_n==1 | assess_n_rev ==-1, ppt_change := max(Overall_Score)-min(Overall_Score), by=Child_ID][order(Child_ID)]

# data.table melt/ cast syntax
# ans <- dcast(DT, ID + Month ~ Category, fun=sum)

# data.table merge syntax
# x[,list(x,y)]

getwd()
