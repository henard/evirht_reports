# install.packages("RMySQL")
library(RMySQL)

if(data_source=="local_file") {
    folder_file_name <- paste(credentials$folder, credentials$filename, sep="/")
    print(folder_file_name)
    d <- read.csv(folder_file_name)
    
    # for(i in names(global_filters)) {
    #     if(i %in% c("time_filter")) {
    #         filt <- d[global_filters[[i]]["column"]] > global_filters[[i]]["start_time"] &
    #                 d[global_filters[[i]]["column"]] <= global_filters[[i]]["end_time"]
    #         d <- d[filt]
    #     }
    #     if(i %in% c("school_year_filter")) {
    #         school_year_values <- paste(unlist(global_filters[[i]]["values"]), collapse=', ')
    #         filt <- d[global_filters[[i]]["column"]] %in% school_year_values
    #         where_clauses <- c(where_clauses, where_clause_i)
    #         d <- d[filt]
    #     }
    # }
} else {
    credentials <- db_credentials[[data_source]]
    db = dbConnect(MySQL(), user=unlist(credentials["username"]),
                            password=unlist(credentials["password"]),
                            dbname=unlist(credentials["database"]),
                            host=unlist(credentials["host"]))
    # db = dbConnect(MySQL(), user="burrondh", password="TGqbsLqwBv!Q2%^k!AA", dbname="thrive_online", host="dw.thriveftc.com")
    # db = dbConnect(MySQL(), user="root", password="Edwardb1", dbname="evirht", host="127.0.0.1")
    # db = do.call(dbConnect, credentials)

    q = dbSendQuery(db, paste("select * from comb_data", where_clause, sep=" "))
    d = fetch(q, n=-1)
}

# dbClearResult(dbListResults(conn)[[1]])


