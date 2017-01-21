library(RMySQL)

if(data_source=="local_file") {
    folder_file_name <- paste(credentials$folder, credentials$filename, sep="/")
    print(folder_file_name)
    d <- read.csv(folder_file_name)
    d <- filter_df(d, report_filters)
} else {
    drv <- dbDriver("MySQL")
    db <- dbConnect(drv, default.file = login_credentials_location, group = data_source, user = NULL, password = NULL)
    # db <- dbConnect(drv, user="burrondh", password="Y5bAbX7NOkqrXafjBWh8", dbname="thrive_online", host="dw.thriveftc.com")
    # db <- dbConnect(drv, user="root", password="Edwardb1", dbname="evirht", host="127.0.0.1")
    q = dbSendQuery(db, paste("select * from comb_data", where_clause, sep=" "))
    d = fetch(q, n=-1)
}

disconnect_all_mysql_conns <- function() {
    all_cons <- dbListConnections(MySQL())
    for(con in all_cons)
        dbDisconnect(con)
}

# disconnect_all_mysql_conns()

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

# dbClearResult(dbListResults(db)[[1]])



