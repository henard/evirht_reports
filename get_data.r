# install.packages("RMySQL")
# library(RMySQL)

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
    credentials[["func"]] <- "MySQL()"
    credentials
    
    mydb = do.call(dbConnect, credentials)
    q = dbSendQuery(mydb, "select chip_version, map_grade, flowcell_session_datetime from vw_production where flowcell_session_datetime>='2016-01-01'")
    d = fetch(q, n=-1)
}

# dbClearResult(dbListResults(conn)[[1]])


