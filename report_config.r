# Report Configuration file

# Report-wide filter applied when querying data
report_filters <- list(
    "time_filter"=list(
        "column"="Completed_Date",
        "lower"="2015-09-01",
        "upper"="2016-09-01",
        "filter_type"="range"
    ),
    "school_year_filter"=list(
        "column"="School_Year",
        "values"=list(1, 2, 3, 4, 5, 6, 7, 8, 9),
        "filter_type"="in"
    )
)

create_where_clause <- function(report_filters) {
    where_clauses <- list()
    for(i in report_filters) {
        if(i[["filter_type"]] %in% c("range")) {
            where_clause_i <- sprintf("%s > '%s' AND %s <= '%s'",
                                    i["column"],
                                    i["lower"],
                                    i["column"],
                                    i["upper"])
            where_clauses <- c(where_clauses, where_clause_i)
        }
        if(i[["filter_type"]] %in% c("in")) {
            in_values <- paste(unlist(i["values"]), collapse=', ')
            where_clause_i <- sprintf("%s IN (%s)", i["column"], in_values)
            where_clauses <- c(where_clauses, where_clause_i)
        }
    }
    return(paste("WHERE",paste(unlist(where_clauses), collapse=' AND ')))
}

where_clause <- create_where_clause(report_filters)

# Plot 1: These are the bar charts in the pdf labelled ‘Sample 1’. For the Headstart set of schools these include a bar
#         chart for all schools and then each individual Headstart school. Some account holders may not include a large
#         number of schools. Henry will set up the script so that a user can input the level of detail required.
# Plot 2: This is the bar chart given in the pdf labelled ‘Sample 3’ and looks at individual pupil journeys by
#         development stage. The plots on individual progress are not required.
# Plot 3: This is the bar chart/scatter plot given in the pdf labelled ‘Sample 2’. Henry will consider this plot and
#         provide alternative formats if there are any improved ways of visualising the data.
# Plot 4: This is the TOL activity given in the spreadsheet called ‘Headstart Data – 11 05 16 for RHead.xlsx’).
#         (This plot should be prioritised over plot 3 if there are time constraints).

report1 = list(
    "title"=list("type"="text",
                 "text"="Headstart Schools - Academic Year 2014/15"),
    "chart1"=list("type"="bar_stacked",
                  "title"="ALL HEADSTART SCHOOLS\nAverage change in percentage points between first and last assessment\nduring academic year",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("all"=list("column"=NA, "values"=NA, "filter_type"="all"))),
    "chart2"=list("type"="bar_stacked",
                  "title"="ALL HEADSTART SCHOOLS\nAverage change in percentage points between first and last assessment\nduring academic year in Locality 1",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("locality1"=list("column"="locality", "values"=list(1), "filter_type"="in"))),
    "chart3"=list("type"="bar_stacked",
                  "title"="ALL HEADSTART SCHOOLS\nAverage change in percentage points between first and last assessment\nduring academic yearr in Locality 6",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("locality6"=list("column"="locality", "values"=list(6), "filter_type"="in")))
)

report2 = list(
    "title"=list("type"="text",
                 "text"="Headstart Schools - Academic Year 2014/15"),
    "chart1"=list("type"="bar_stacked",
                  "title"="ALL HEADSTART SCHOOLS\nAverage change in percentage points between first and last assessment\nduring academic year",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("all"=list("column"=NA, "values"=NA, "filter_type"="all"))),
    "chart2"=list("type"="bar_stacked",
                  "title"="ALL HEADSTART SCHOOLS\nAverage change in percentage points between first and last assessment\nduring academic year",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("locality7"=list("column"="locality", "values"=list(7), "filter_type"="in"))),
    "chart3"=list("type"="bar_stacked",
                  "title"="ALL HEADSTART SCHOOLS\nAverage change in percentage points between first and last assessment\nduring academic year",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("locality8"=list("column"="locality", "values"=list(8), "filter_type"="in")))
)

reports = list("report1"=report1,
               "report2"=report2)
