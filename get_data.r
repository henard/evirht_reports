library(RMySQL)

# Import data.
if(data_source=="local_file") {
    folder_file_name <- paste(credentials$folder, credentials$filename, sep="/")
    d <- read.csv(folder_file_name)
    d <- filter_df(d, report_filters)
} else {
    where_clause <- create_where_clause(report_filters)
    drv <- dbDriver("MySQL")
    db <- dbConnect(drv, default.file = login_credentials_location, group = data_source, user = NULL, password = NULL)
    q = dbSendQuery(db, paste("select * from comb_data", where_clause, sep=" "))
    d = fetch(q, n=-1)
}

# dbClearResult(dbListResults(db)[[1]])

# Function to disconnect connection to MySQL database. Rarely needs to be used.
disconnect_all_mysql_conns <- function() {
    all_cons <- dbListConnections(MySQL())
    for(con in all_cons)
        dbDisconnect(con)
}

# disconnect_all_mysql_conns()

# Apply filters in report_config to dataframe.
# Only applied to data imported from csv since it is not queried with a WHERE
# clause applied.
filter_df <- function(df, filters) {
    for(i in filters) {
        if(i[["filter_type"]] %in% c("range")) {
            filt <- df[i[["column"]]] > i[["lower"]] &
                    df[i[["column"]]] <= i[["end_time"]]
        }
        if(i[["filter_type"]] %in% c("in")) {
            filt <- d[i[["column"]]] %in% i[["values"]]
        }
    }
    return(df[filt])
}

# Apply filter in report_config to data in data.table form.
# Used to apply filters in chart config part of report_config to data
filter_dt <- function(DT, filters) {
    for(i in filters) {
        if(i[["filter_type"]] %in% c("range")) {
            DT <- DT[get(i[["column"]])>=unlist(i[["lower"]]) & get(i[["column"]])<unlist(i[["upper"]])]
        }
        if(i[["filter_type"]] %in% c("in")) {
            DT <- DT[get(i[["column"]]) %in% unlist(i[["values"]])]
        }
    }
    return(DT)
}
