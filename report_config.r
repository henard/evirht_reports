# Report Configuration file

# Report-wide filter applied when querying data
report_filters <- list(
    "time_filter"=list(
        "column"="Completed_Date",
        "lower"="2014-09-01",
        "upper"="2015-09-01",
        "filter_type"="range"
    ),
    "school_year_filter"=list(
        "column"="School_Year",
        "values"=list("-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
        "filter_type"="in"
    )
)

# report1: These are the bar charts in the pdf labelled ‘Sample 1’. For the Headstart set of schools these include a bar
#         chart for all schools, schools in locality 1 and 6  and 3 individual Headstart school. (Now all in side-by-side format)
report1 = list(
    "chart1"=list("type"="bar_side_by_side",
                  "dataset" = "score_change_dt",
                  "title"="ALL HEADSTART SCHOOLS\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("all"=list("column"=NA, "values"=NA, "filter_type"="all"))),
    "chart2"=list("type"="bar_side_by_side",
                  "dataset" = "score_change_dt",
                  "title"="HEADSTART LOCALITY 1\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("locality1"=list("column"="Locality", "values"=list(1), "filter_type"="in"))),
    "chart3"=list("type"="bar_side_by_side",
                  "dataset" = "score_change_dt",
                  "title"="HEADSTART LOCALITY 6\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("locality6"=list("column"="Locality", "values"=list(6), "filter_type"="in"))),
    "chart4"=list("type"="bar_side_by_side",
                  "dataset" = "score_change_dt",
                  "title"="BRUNEL PRIMARY SCHOOL\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("org_brunell"=list("column"="Organisation", "value"="Brunel Primary", "filter_type"="contains"))),
    "chart5"=list("type"="bar_side_by_side",
                  "dataset" = "score_change_dt",
                  "title"="CALLINGTON COMMUNITY COLLEGE\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("org_callington"=list("column"="Organisation", "value"="Callington Community", "filter_type"="contains"))),
    "chart6"=list("type"="bar_side_by_side",
                  "dataset" = "score_change_dt",
                  "title"="BODRIGGY ACADEMY\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="School_Year",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("org_callington"=list("column"="Organisation", "value"="Bodriggy", "filter_type"="contains"))),
    "chart7"=list("type"="bar_side_by_side",
                  "dataset" = "score_change_dt",
                  "title"="BRUNEL PRIMARY SCHOOL\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="Child_ID",
                  "xgroup"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("org_brunell"=list("column"="Organisation", "value"="Brunel Primary", "filter_type"="contains")))
)

# report2: This is pie charts of share of pupils across development stages given in the pdf labelled ‘Sample 3’.
report2 = list(
    "chart1"=list("type"="pie",
                  "dataset" = "score_dt2",
                  "title"="ALL HEADSTART SCHOOLS\nShare of pupils in each Development Stage at\nfirst assessment in 2014/15",
                  "measure"="c",
                  "xaxis"="",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("N_assessment_2+"=list("column"="N_assessments", "lower"=2, "upper"=10, "filter_type"="range"),
                                "Assessment_n_1"=list("column"="Assessment_n", "values"=list(1), "filter_type"="in"))),
    "chart2"=list("type"="pie",
                  "dataset" = "score_dt2",
                  "title"="ALL HEADSTART SCHOOLS\nShare of pupils in each Development Stage at\nlast assessment in 2014/15",
                  "measure"="c",
                  "xaxis"="",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("N_assessment_2+"=list("column"="N_assessments", "lower"=2, "upper"=10, "filter_type"="range"),
                                "Assessment_n_1"=list("column"="Assessment_n_rev", "values"=list(-1), "filter_type"="in"))),
    "data1"=list("type"="data",
                 "dataset" = "score_dt2",
                 "title"="ALL HEADSTART SCHOOLS\nChange in share of pupils in each Development Stage between\nfirst &last assessment in 2014/15",
                 "measure"="c",
                 "by"="Dev_Stage",
                 "filter"=list("all"=list("column"=NA, "values"=NA, "filter_type"="all"))),
    "chart3"=list("type"="pie",
                  "dataset" = "score_dt2",
                  "title"="HEADSTART LOCALITY 1\nShare of pupils in each Development Stage at\nfirst assessment in 2014/15",
                  "measure"="c",
                  "xaxis"="",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("N_assessment_2+"=list("column"="N_assessments", "lower"=2, "upper"=10, "filter_type"="range"),
                                "Assessment_n_1"=list("column"="Assessment_n", "values"=list(1), "filter_type"="in"),
                                "locality1"=list("column"="Locality", "values"=list(1), "filter_type"="in"))),
    "chart4"=list("type"="pie",
                  "dataset" = "score_dt2",
                  "title"="HEADSTART LOCALITY 1\nShare of pupils in each Development Stage at\nlast assessment in 2014/15",
                  "measure"="c",
                  "xaxis"="",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("N_assessment_2+"=list("column"="N_assessments", "lower"=2, "upper"=10, "filter_type"="range"),
                                "Assessment_n_1"=list("column"="Assessment_n_rev", "values"=list(-1), "filter_type"="in"),
                                "locality1"=list("column"="Locality", "values"=list(1), "filter_type"="in"))),
    "data2"=list("type"="data",
                 "dataset" = "score_dt2",
                 "title"="HEADSTART LOCALITY 1\nChange in share of pupils in each Development Stage between\nfirst &last assessment in 2014/15",
                 "measure"="c",
                 "by"="Dev_Stage",
                 "filter"=list("locality1"=list("column"="Locality", "values"=list(1), "filter_type"="in")))
)

# report3: This is the bar chart/scatter plot given in the pdf labelled ‘Sample 2’. Henry will consider this plot and
#         provide alternative formats if there are any improved ways of visualising the data.
report3 = list(
    "chart1"=list("type"="bar_stacked",
                  "dataset" = "score_change_dt",
                  "title"="HEADSTART SCHOOL - Acorn Academy - Nine Maidens\nIndividual pupil journeys during academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="Child_ID_Completed_date",
                  "xgroup"="School_Year",
                  "colour_by"="Dev_Stage",
                  "filter"=list("org_brunell"=list("column"="Organisation", "value"="Brunel Primary", "filter_type"="contains")))
)

# report4: This is the TOL activity given in the spreadsheet called ‘Headstart Data – 11 05 16 for RHead.xlsx’).
#         (This plot should be prioritised over plot 3 if there are time constraints).
report4 = list(
    "chart1"=list("type"="bar_side_by_side",
                  "dataset" = "pupil_counts",
                  "title"="HEADSTART KERNOW - TOL activity",
                  "measure"="pct",
                  "xaxis"="Organisation",
                  "xgroup"="",
                  "colour_by"="pupil_count_type",
                  "filter"=list("Org_seln"=list("column"="Organisation_ID", "values"=c(1780, 871, 676, 1362, 1076, 671, 885, 1777, 1363,
                                                                                       874, 886, 1359, 1075, 875, 1355, 887, 883, 1356,
                                                                                       1361, 876, 893, 890, 877, 675, 1077, 813, 888, 878,
                                                                                       673, 867, 870, 889, 884, 894, 1443, 891, 868, 869,
                                                                                       879, 881, 1196, 892, 880, 882), "filter_type"="in")))
)

# report5: This is the bar chart given in the pdf labelled ‘Sample 3’ and looks at individual pupil journeys by
#          development stage. The plots on individual progress are not required. (Now a in a new report of its own)
report5 = list(
    "chart5"=list("type"="bar_stacked",
                  "dataset" = "score_change_dt",
                  "title"="HEADSTART SCHOOLS\nAverage percentage point change between first and last assessment scores\nduring academic year 2014/15",
                  "measure"="score_change",
                  "xaxis"="Organisation",
                  "xgroup"="",
                  "colour_by"="Dev_Stage",
                  "filter"=list("org_selection1"=list("column"="Organisation", "values"=list("Acorn Academy Cornwall",
                                                                                             "Antony C of E",
                                                                                             "Bodriggy Academy",
                                                                                             "Braddock CE Primary",
                                                                                             "Brunel Primary and Nursery Academy",
                                                                                             "Burraton Primary School",
                                                                                             "Callington Community College",
                                                                                             "Calstock Primary School",
                                                                                             "Dobwalls Community Primary School",
                                                                                             "Duloe School",
                                                                                             "Liskeard Hillfort Primary School",
                                                                                             "Looe Primary School",
                                                                                             "Millbrook C of E Primary",
                                                                                             "Nancledra School",
                                                                                             "Newlyn School",
                                                                                             "Nine Maidens APA - Primary",
                                                                                             "Nine Maidens APA - Secondary",
                                                                                             "Pelynt School",
                                                                                             "Pendeen School",
                                                                                             "Pensans Primary School",
                                                                                             "Pensilva Primary School",
                                                                                             "Sir Robert Gefferys",
                                                                                             "St Cleer Primary School",
                                                                                             "St Hilary School",
                                                                                             "St Levan School",
                                                                                             "St Martins C of E VA Primary School",
                                                                                             "St Marys C of E Primary School Penzance",
                                                                                             "St Stephens Primary Saltash",
                                                                                             "Stoke Climsland School",
                                                                                             "Trenode C of E Primary School"), "filter_type"="in")))
)

reports = list("report1"=report1,
               "report2"=report2,
               "report3"=report3,
               "report4"=report4,
               "report5"=report5)

################################## END OF CONFIG ######################################################################
################################## BELOW ARE FUNCTIONS ASSOCIATED WITH CONFIG ABOVE  - Do not edit below ##############

# Determine filename from report config
plot_filename <- function(chart_config, file_extention) {
    filename <- paste(lapply(setdiff(names(chart_config), c("title", "filter")), function(x) chart_config[[x]]), collapse="_")
    return(paste(filename, "_", names(chart_config[["filter"]]), file_extention, sep=""))
}

# Add automated filename to config
for(i in names(reports)) {
    for(j in names(reports[[i]])) {
        if(reports[[i]][[j]]$type!="text") {
            if(reports[[i]][[j]]$type=="data") file_extention <- "Rdata" else file_extention <- "png"
            reports[[i]][[j]]$long_filename <- paste(paste(i, j, sep="_"), plot_filename(reports[[i]][[j]], file_extention), sep="_")
            reports[[i]][[j]]$filename <- paste(paste(i, j, sep="_"), file_extention, sep=".")
        }
    }
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
