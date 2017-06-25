# Report Configuration file

# The following section details the arguments that must be defined to set up each report.
# These arguments are to be edited by Thrive.

# Report-wide arguments
# Set the academic year by editing the following start & end dates.
yr_start = "2016-09-01"
yr_end = "2017-09-01"
# Set the school years that are to be extracted by adding/removing from the following list.
school_yrs = list("-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")

# Report 1 arguments
# Report 1 can be set up to have as many charts as the user wants, simply add or remove a chart from each argument below.
# Set the account or organisation IDs for each chart requested by editing the following.
report1_ids_in <- list(chart1 = 1483,
                       chart2 = 418,
                       chart3 = 1319,
                       chart4 = 676,
                       chart5 = 418)
# Specify alternative chart titles if you would like to overide the automated Chart titles.
# Specify "" (e.g. chart1 = "") to use an automatically generated chart title.
report1_labels_in <- list(chart1 = "",
                          chart2 = "",
                          chart3 = "",
                          chart4 = "",
                          chart5 = "")
# Define the levels for each chart requested.
# Account or Organisation will produce a bar chart for percentage point change in assessments for the whole organisation or account
# defined by its ID. Pupil will produce a bar chart for pupil ID by school year and must be at an Organisation level (i.e., an organisation 
# ID is given in report1_ids_in for that chart).
report1_levels_in <- list(chart1 = "Account",
                          chart2 = "Organisation",
                          chart3 = "Organisation",
                          chart4 = "Organisation",
                          chart5 = "Pupil")
# Define the main heading for the report
report1_heading <- "Headstart Schools"
# For each chart, define chunk size - the maximum number of x-axis categories permitted per bar chart. A chart with 90 x-axis categories and 
# a chunk_size = 30 would result in 3 separate bar charts named "..._chunk1.png", "..._chunk2.png" and "..._chunk3.png".
report1_chunk_size_in <- list(chart1 = 60,
                              chart2 = 60,
                              chart3 = 60,
                              chart4 = 60,
                              chart5 = 60)

# Report 2 arguments
# Report 2 can be set up to have as many charts as the user wants, simply add or remove a chart from each argument below.
# Report 2 is set to produce pie charts by organisation or account. Set the organisation or account IDs for each chart requested by editing the following.
report2_ids_in <- list(chart1 = 1483,
                       chart2 = 418)
# Specify alternative chart titles if you would like to overide the automated Chart titles.
# Specify "" (e.g. chart1 = "") to use an automatically generated chart title.
report2_labels_in <- list(chart1 = "",
                          chart2 = "")
# Define the levels for each chart requested (these can be either account or organisation).
report2_levels_in <- list(chart1 = "Account",
                          chart2 = "Organisation")
# Define the main heading for the report
report2_heading <- "Headstart Schools"

# Report 3 arguments
# Report 3 filters by organisation only and is set up to produce one or more charts at a time.
# Set the organisation ID below.
report3_ids_in <- list(chart1 = c(675,1077),
                       chart2 = c(675,1077)) 
# Report 3 can additionally be filter by by school year
# List the school year you would like to include below. Filtering is applied in addition to filtering specified in school_yrs above
# e.g. chart1 = c(1, 2, 3) to include school years 1, 2 & 3; chart1 = c() to include all school years in data.
report3_additional_filter_in <- list(chart1 = c(),
                                  chart2 = c(1, 2, 3))
# Specify an alternative chart title if you would like to overide the automated Chart title.
# Specify "" (e.g. chart1 = "") to use an automatically generated chart title.
report3_labels_in <- list(chart1 = "",
                          chart2 = "")
# Define the main heading for the report
report3_heading <- "Headstart Schools"
# Define chunk size - the maximum number of x-axis categories permitted per bar chart. A chart with 90 x-axis categories and 
# a chunk_size = 30 would result in 3 separate bar charts named "..._chunk1.png", "..._chunk2.png" and "..._chunk3.png".
report3_chunk_size_in <- list(chart1 = 60,
                              chart2 = 60)

# Report 4 arguments
# Report 4 filters by organisation or account and is set up to produce one chart at a time.
# Set the list of one or more organisation/account IDs below.
report4_ids_in <- list(chart1 = list(1483))
# Define the level for the chart.
# If the IDs listed in report4_ids_in are account ids specify "Account"; if they are Organisation IDs specify "Organisation"
# The chart is a bar for each organisation in the accounts or organisations listed in report4_ids_in
report4_levels_in <- list(chart1 = "Account")
# Specify alternative chart title if you would like to overide the automated Chart titles.
# Specify "" (e.g. chart1 = "") to use an automatically generated chart title.
report4_labels_in <- list(chart1 = "SELECTED SCHOOLS")
# Define the main heading for the report
report4_heading <- "Headstart Schools"
# # Define chunk size - the maximum number of x-axis categories permitted per bar chart. A chart with 90 x-axis categories and
# # a chunk_size = 30 would result in 3 separate bar charts named "..._chunk1.png", "..._chunk2.png" and "..._chunk3.png".
report4_chunk_size_in <- list(chart1 = 30)

# Report 5 arguments
# Report 5 can be set up to have as many charts as the user wants, simply add or remove a chart from each argument below.
# Report 5 is set to produce bar charts by account only. Set the account IDs for each chart requested by editing the following.
report5_ids_in <- list(chart1 = 1483)
# Provide a label for the chart requested (these are used within the chart title
report5_labels_in <- list(chart1 = "")
# Specify alternative chart title if you would like to overide the automated Chart titles.
# Specify "" (e.g. chart1 = "") to use an automatically generated chart title.
report5_heading <- "Headstart Schools"
# Define chunk size - the maximum number of x-axis categories permitted per bar chart. A chart with 90 x-axis categories and 
# a chunk_size = 30 would result in 3 separate bar charts named "..._chunk1.png", "..._chunk2.png" and "..._chunk3.png".
report5_chunk_size_in <- list(chart1 = 60)

######################################################################################################
# Filters - recommended not to edit without support

# Report-wide filter applied when querying data
report_filters <- list(
    "time_filter"=list(
        "column"="Completed_Date",
        "lower"=yr_start,
        "upper"=yr_end,
        "filter_type"="range"
    ),
    "school_year_filter"=list(
        "column"="School_Year",
        "values"=school_yrs,
        "filter_type"="in"
    )
)


academic_yr = paste(gsub("(.+)(-)(.+)(-)(.+)","\\1",yr_start),"/",gsub("(.+)(-)(.+)(-)(.+)","\\1",yr_end),sep="")

# report1: These are the bar charts in the pdf labelled 'Sample 1'. For the Headstart set of schools these include a bar
#         chart for all schools, schools in locality 1 and 6  and 3 individual Headstart school. (Now all in side-by-side format)
report1_template =  list(
    "type"="bar_side_by_side",
    "dataset" = "score_change_dt",
    "title_org"=NA,
    "auto_title"=paste("\nAverage percentage point change between first and last assessment scores\nduring academic year ",academic_yr,sep=""),
    "measure"="score_change",
    "xaxis"=NA,
    "xgroup"="",
    "colour_by"="Dev_Stage",
    "chunk_size"=60,
    "filter"=list("filter1"=list("column"=NA, "values"=NA, "filter_type"="in")))

report1 = list()
for(i in seq_along(report1_ids_in)){
    if(length(report1_ids_in) != length(report1_levels_in) ||
       length(report1_ids_in) != length(report1_labels_in) ||
       length(report1_ids_in) != length(report1_chunk_size_in))stop("Number of input ids, labels, levels or chunk_sizes in Report 1 are not the same length")
    if(report1_levels_in[[i]]=="Pupil") xtitle = "Child_ID" else xtitle = "School_Year"
    temp <- report1_template
    temp$title_org <- toupper(report1_labels_in[[i]])
    temp$xaxis <- xtitle
    temp$chunk_size <- report1_chunk_size_in[[i]]
    temp$filter$filter1$values <- list(report1_ids_in[[i]])
    if(report1_levels_in[[i]] == "Account"){
        temp$filter$filter1$column <- "AccountID"
    } else if(report1_levels_in[[i]] == "Organisation"){
        temp$filter$filter1$column <- "Organisation_ID"
    } else {
        temp$xgroup <- "School_Year"
        temp$filter$filter1$column <- "Organisation_ID"
    }
    report1[[i]] = assign(paste("chart",i,sep=""),temp)
}
names(report1) = paste("chart",seq_along(report1_ids_in),sep="")

report1_fullheading <- paste(report1_heading," - Academic Year ",academic_yr,sep="")
save(report1_fullheading, file=file.path(data_dir, "report1_fullheading.RData"))


# report2: This is pie charts of share of pupils across development stages given in the pdf labelled 'Sample 3'.
report2 = list()
index = 1
for(i in seq_along(report2_ids_in)){
    if(length(report2_ids_in) != length(report2_levels_in) || length(report2_ids_in) != length(report2_labels_in))stop("Number of input ids, labels or levels in Report 2 are not the same length")
    if(report2_levels_in[[i]] == "Account") col_level = "AccountID" else col_level = "Organisation_ID"
    temp1=list("type"="pie",
               "dataset" = "score_dt2",
               "title_org"=toupper(report2_labels_in[[i]]),
               "auto_title"=paste("\nShare of pupils in each Development Stage at\nfirst assessment in ",academic_yr,sep=""),
               "measure"="c",
               "xaxis"="",
               "xgroup"="",
               "colour_by"="Dev_Stage",
               "chunk_size"=10,
               "filter"=list("org"=list("column"=col_level, "values"=list(report2_ids_in[[i]]), "filter_type"="in"),
                             "N_assessment_2+"=list("column"="N_assessments", "lower"=2, "upper"=10, "filter_type"="range"),
                             "Assessment_n_1"=list("column"="Assessment_n", "values"=list(1), "filter_type"="in")))
    report2[[index]] = assign(paste("chart",(i*2-1),sep=""),temp1)
    index = index + 1
    temp2=list("type"="pie",
               "dataset" = "score_dt2",
               "title_org"=toupper(report2_labels_in[[i]]),
               "auto_title"=paste("\nShare of pupils in each Development Stage at\nlast assessment in ",academic_yr,sep=""),
               "measure"="c",
               "xaxis"="",
               "xgroup"="",
               "colour_by"="Dev_Stage",
               "chunk_size"=10,
               "filter"=list("org"=list("column"=col_level, "values"=list(report2_ids_in[[i]]), "filter_type"="in"),
                             "N_assessment_2+"=list("column"="N_assessments", "lower"=2, "upper"=10, "filter_type"="range"),
                             "Assessment_n_1"=list("column"="Assessment_n_rev", "values"=list(-1), "filter_type"="in")))
    report2[[index]] = assign(paste("chart",(i*2),sep=""),temp2)
    index = index + 1
    temp3=list("type"="data",
               "dataset" = "score_dt2",
               "title_org"=toupper(report2_labels_in[[i]]),
               "auto_title"=paste("\nChange in share of pupils in each Development Stage between\nfirst &last assessment in ",academic_yr,sep=""),
               "measure"="c",
               "by"="Dev_Stage",
               "filter"=list("org"=list("column"=col_level, "values"=list(report2_ids_in[[i]]), "filter_type"="in")))
    report2[[index]] = assign(paste("data",i,sep=""),temp3)
    index = index + 1
}
names(report2) = paste(rep(c("chart","chart","data"),length(report2_ids_in)),rep(seq_along(report2_ids_in),each=3)*rep(c(2,2,1),length(report2_ids_in))-(rep(c(1,0,0),length(report2_ids_in))),sep="")

report2_fullheading <- paste(report2_heading," - Academic Year ",academic_yr,sep="")
save(report2_fullheading, file=file.path(data_dir, "report2_fullheading.RData"))

# report3: This is the bar chart/scatter plot given in the pdf labelled 'Sample 2'. Henry will consider this plot and
#         provide alternative formats if there are any improved ways of visualising the data.

report3_template = list(
    "type"="bar_side_by_side",
    "dataset" = "score_dt",
    "title_org"=NA,
    "auto_title"=paste("\nIndividual pupil journeys during academic year ",academic_yr,sep=""),
    "measure"="Overall_Score",
    "xaxis"="Child_ID_Completed_date",
    "xgroup"="School_Year",
    "colour_by"="Dev_Stage",
    "chunk_size"=NA,
    "filter"=list("org"=list("column"="Organisation_ID", "values"=NA, "filter_type"="in")))

report3 = list()
for(i in seq_along(report3_ids_in)){
    if(length(report3_ids_in) != length(report3_additional_filter_in) ||
       length(report3_ids_in) != length(report3_labels_in) ||
       length(report3_ids_in) != length(report3_chunk_size_in))stop("Number of input ids, labels, additional_filter or chunk_sizes in Report 3 are not the same length")
    temp <- report3_template
    temp$title_org <- toupper(report3_labels_in[[i]])
    temp$chunk_size <- report3_chunk_size_in[[i]]
    temp$filter$org$values <- list(report3_ids_in[[i]])
    if(!is.null(report3_additional_filter_in[[i]])) temp$filter$filter2 <- list("column"="School_Year", "values"=list(report3_additional_filter_in[[i]]), "filter_type"="in")
    report3[[i]] = assign(paste("chart",i,sep=""),temp)
}
names(report3) = paste("chart",seq_along(report3_ids_in),sep="")
report3_fullheading <- paste(report3_heading," - Academic Year ",academic_yr,sep="")
save(report3_fullheading, file=file.path(data_dir, "report3_fullheading.RData"))

# report4: This is the TOL activity given in the spreadsheet called 'Headstart Data – 11 05 16 for RHead.xlsx').
#         (This plot should be prioritised over plot 3 if there are time constraints).
if(report4_levels_in == "Account") coltype = "AccountID" else coltype = "Organisation_ID"
report4 = list(
    "chart1"=list("type"="bar_stacked",
                  "dataset" = "pupil_counts",
                  "title_org"=toupper(report4_labels_in[[1]]),
                  "auto_title"=" - TOL activity",
                  "measure"="pct",
                  "xaxis"="Organisation",
                  "xgroup"="",
                  "colour_by"="pupil_count_type_dot",
                  "chunk_size"=report4_chunk_size_in[[1]],
                  "filter"=list("Org_seln"=list("column"=coltype, "values"=report4_ids_in[[1]], "filter_type"="in"))),
    "chart2"=list("type"="bar_side_by_side",
                  "dataset" = "pupil_counts",
                  "title_org"=toupper(report4_labels_in[[1]]),
                  "auto_title"=" - TOL activity",
                  "measure"="N",
                  "xaxis"="Organisation",
                  "xgroup"="",
                  "colour_by"="pupil_count_type",
                  "chunk_size"=report4_chunk_size_in[[1]],
                  "filter"=list("Org_seln"=list("column"=coltype, "values"=report4_ids_in[[1]], "filter_type"="in")))
)

report4_fullheading <- paste(report4_heading," - Academic Year ",academic_yr,sep="")
save(report4_fullheading, file=file.path(data_dir, "report4_fullheading.RData"))

# report5: This is the bar chart given in the pdf labelled 'Sample 3' and looks at individual pupil journeys by
#          development stage. The plots on individual progress are not required. (Now a in a new report of its own)
report5 = list()
for(i in seq_along(report5_ids_in)){
    if(length(report5_ids_in) != length(report5_labels_in))stop("Number of input ids, labels or levels in Report 5 are not the same length")
    temp=list("type"="bar_side_by_side",
              "dataset" = "score_change_dt",
              "title_org"=toupper(report5_labels_in[[i]]),
              "auto_title"=paste("\nAverage percentage point change between first and last assessment scores\nduring academic year ",academic_yr,sep=""),
              "measure"="score_change",
              "xaxis"="Organisation",
              "xgroup"="",
              "colour_by"="Dev_Stage",
              "chunk_size"=report5_chunk_size_in[[1]],
              "filter"=list("org"=list("column"="AccountID", "values"=list(report5_ids_in[[i]]), "filter_type"="in")))
    report5[[i]] = assign(paste("chart",i,sep=""),temp)
    
}
names(report5) = paste("chart",seq_along(report5_ids_in),sep="")

report5_fullheading <- paste(report5_heading," - Academic Year ",academic_yr,sep="")
save(report5_fullheading, file=file.path(data_dir, "report5_fullheading.RData"))

reports = list("report1"=report1,
               "report2"=report2,
               "report3"=report3,
               "report4"=report4,
               "report5"=report5)

################################## END OF CONFIG ######################################################################
################################## BELOW ARE FUNCTIONS ASSOCIATED WITH CONFIG ABOVE  - Do not edit below ##############

# Determine filename from report config
plot_filename <- function(chart_config, file_extention) {
    filename <- paste(lapply(setdiff(names(chart_config), c("title_org", "filter")), function(x) chart_config[[x]]), collapse="_")
    filename <- paste(filename, "_", names(chart_config[["filter"]]), sep="")
    return(paste(filename, file_extention, sep="."))
}

# Determine auto_chart_title where Account or Organisation names are determined from AccountID or Organisation_IDs
filter_values_text <- function(filter) {
    if(filter$column=="AccountID") {
        col_name = "Account"
    } else if(filter$column=="Organisation_ID") {
        col_name = "Organisation"
    } else {
        return("")
    }

    if(filter$filter_type=="in") {
        id_lookup <- unique(pupil_counts, by=filter$column)
        org_names <- id_lookup[get(filter$column) %in% unlist(filter$values), get(col_name)]
    } else {
        return("")
    }
    org_names <- org_names[lapply(org_names, function(x) nchar(as.character(x))) >0 ]
    return(paste(trimws(org_names), collapse=", "))
}

filter_values_texts <- function(filters) {
    org_names <- lapply(filters, function(x) filter_values_text(x))
    org_names <- org_names[lapply(org_names, function(x) nchar(as.character(x))) >0 ]
    return(paste(trimws(org_names), collapse=", "))
}

# Add automated filename to config
for(i in names(reports)) {
    for(j in names(reports[[i]])) {
        if(reports[[i]][[j]]$type!="text") {
            if(reports[[i]][[j]]$type=="data") file_extention <- "RData" else file_extention <- "png"
            reports[[i]][[j]]$long_filename <- paste(paste(i, j, sep="_"), plot_filename(reports[[i]][[j]], file_extention), sep="_")
            reports[[i]][[j]]$filename <- paste(paste(i, j, sep="_"), file_extention, sep=".")
        }
    }
}

# Add automated chart labels
add_auto_chart_labels <- function(reports) {
    for(i in names(reports)) {
        for(j in names(reports[[i]])) {
            if(reports[[i]][[j]]$type!="text") {
                if(reports[[i]][[j]]$type=="data") file_extention <- "RData" else file_extention <- "png"
                reports[[i]][[j]]$auto_title <- paste(toupper(filter_values_texts(reports[[i]][[j]]$filter)), reports[[i]][[j]]$auto_title, sep="")
            }
        }
    }
    return(reports)
}

# Replace blank organisations in titles with automated organisation
add_auto_chart_labels2 <- function(reports) {
    for(i in names(reports)) {
        for(j in names(reports[[i]])) {
            if(reports[[i]][[j]]$type!="text") {
                if(reports[[i]][[j]]$title_org=="") {
                    reports[[i]][[j]]$auto_title <- paste(toupper(filter_values_texts(reports[[i]][[j]]$filter)), reports[[i]][[j]]$auto_title, sep="")
                } else {
                    reports[[i]][[j]]$auto_title <- paste(reports[[i]][[j]]$title_org, reports[[i]][[j]]$auto_title, sep="")
                }
            }
        }
    }
    return(reports)
}

# Function to extract info from report_filters to create a WHERE clause to use
# in the querying of thrive_online. Ensures only data required the report is
# grabbed
create_where_clause <- function(report_filters) {
    where_clauses <- list()
    for(i in report_filters) {
        if(i[["filter_type"]] %in% c("range")) {
            where_clause_i <- sprintf("%s > '%s' AND %s <= '%s'",
                                      sql_fieldname_lookup[[i[["column"]]]],
                                      i["lower"],
                                      sql_fieldname_lookup[[i[["column"]]]],
                                      i["upper"])
            where_clauses <- c(where_clauses, where_clause_i)
        }
        if(i[["filter_type"]] %in% c("in")) {
            in_values <- paste(unlist(i["values"]), collapse=', ')
            where_clause_i <- sprintf("%s IN (%s)", sql_fieldname_lookup[[i[["column"]]]], in_values)
            where_clauses <- c(where_clauses, where_clause_i)
        }
    }
    return(paste("WHERE",paste(unlist(where_clauses), collapse=' AND ')))
}

# Where clause of query to thrive_online needs to reference the fields as they
# exist in SQL, not there aliases used in analysis and specified in the global cofig
sql_fieldname_lookup <- list("AccountID"="o.accountId",
                             "Organisation_ID"="o.organisationId",
                             "Child_ID"="ip.childId",
                             "Status"="cs.status",
                             "Child_DOB"="c2.dob",
                             "School_Year"="ip.schoolYear",
                             "Age"="ip.age",
                             "Gender"="g.name",
                             "individualProfileId"="ip.individualProfileId",
                             "Profiled_ID"="c.childId",
                             "Completed_Date"="p.completedDate",
                             "Dev_Stage"="p.developmentalStageId")
