library(RMySQL)

# Import data and format.
read_data <- function(data_src) {
    if(data_src=="local_file") {
        d <- read.csv(csv_file_location, colClasses=c_classes)
        d <- format_csv_df(d)
        d <- filter_df(d, report_filters)
    } else {
        where_clause <- create_where_clause(report_filters)
        drv <- dbDriver("MySQL")
        db <- dbConnect(drv, default.file = login_credentials_location, group = data_source, user = NULL, password = NULL)
        q = dbSendQuery(db, paste("select * from comb_data", where_clause, sep=" "))
        d = fetch(q, n=-1)
        d <- format_dataframe(d)
    }
    return(d)
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
        colname <- i[["column"]]
        if(i[["filter_type"]] %in% c("range")) {
            lower <- i[["lower"]]
            upper <- i[["upper"]]
            if(inherits(df[[colname]], "Date")) {
                lower <- as.Date(lower, "%Y-%m-%d")
                upper <- as.Date(upper, "%Y-%m-%d")
            }
            df <- df[df[[colname]] > lower & df[[colname]] <= upper, ]
        }
        if(i[["filter_type"]] %in% c("in")) {
            df <- df[df[[colname]] %in% i[["values"]], ]
        }
    }
    return(df)
}

# Apply filter in report_config to data in data.table form.
# Used to apply filters in chart config part of report_config to data
filter_dt <- function(DT, filters) {
    for(i in filters) {
        colname <- i[["column"]]
        if(i[["filter_type"]] %in% c("range")) {
            lower <- i[["lower"]]
            upper <- i[["upper"]]
            if(inherits(df[[colname]], "Date")) {
                lower <- as.Date(lower, "%Y-%m-%d")
                upper <- as.Date(upper, "%Y-%m-%d")
            }
            DT <- DT[get(colname)>=lower & get(colname)<upper]
            # DT <- DT[get(i[["column"]])>=unlist("lower"]]) & get(i[["column"]])<unlist(i[["upper"]])]
        }
        if(i[["filter_type"]] %in% c("in")) {
            DT <- DT[get(colname) %in% unlist(i[["values"]])]
        }
    }
    return(DT)
}
