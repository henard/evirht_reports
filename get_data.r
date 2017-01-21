library(RMySQL)

if(data_source=="local_file") {
    folder_file_name <- paste(credentials$folder, credentials$filename, sep="/")
    print(folder_file_name)
    d <- read.csv(folder_file_name)
    d <- filter_csv(d, report_filters)
} else {
    drv <- dbDriver("MySQL")
    db <- dbConnect(drv, default.file = login_credentials_location, group = data_source, user = NULL, password = NULL)
    # db <- dbConnect(drv, user="burrondh", password="Y5bAbX7NOkqrXafjBWh8", dbname="thrive_online", host="dw.thriveftc.com")
    # db <- dbConnect(drv, user="root", password="Edwardb1", dbname="evirht", host="127.0.0.1")
    q = dbSendQuery(db, paste("select * from comb_data", where_clause, sep=" "))
    d = fetch(q, n=-1)
}

filter_csv <- function(df, filters) {
    for(i in names(filters)) {
        if(i %in% c("time_filter")) {
            filt <- df[filters[[i]]["column"]] > filters[[i]]["start_time"] &
                    df[filters[[i]]["column"]] <= filters[[i]]["end_time"]
            df <- df[filt]
        }
        if(i %in% c("school_year_filter")) {
            school_year_values <- paste(unlist(filters[[i]]["values"]), collapse=', ')
            filt <- d[filters[[i]]["column"]] %in% school_year_values
            df <- df[filt]
        }
    }
    return(df)
}

# dbClearResult(dbListResults(conn)[[1]])



