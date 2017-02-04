library(RODBC)

# Import data and format.
read_data <- function(data_src) {
    if(data_src=="csv") {
        d <- read.csv(data_sources[[data_src]], colClasses=c_classes)
        d <- format_csv_df(d)
        d <- format_dataframe(d)
        d <- filter_df(d, report_filters)
        save(d, file=new_rdata_folderfilename())
        delete_old_rdata(filename_to_keep=get_most_recent_rdata_filename())
    } else if(data_src=="thrive") {
        filter_where_clause <- create_where_clause(report_filters)
        lines <- readLines(file.path(sql_dir, "pupil_assessments.sql"))
        query <- paste(add_where_clause(lines, filter_where_clause), collapse=" ")
        conn <- odbcConnect(data_sources[[data_src]])
        d <- sqlQuery(conn, query)
        d <- format_sql_df(d)
        d <- format_dataframe(d)
        save(d, file=new_rdata_folderfilename())
        delete_old_rdata(filename_to_keep=get_most_recent_rdata_filename())
    } else {
        load(data_sources[[data_src]])
        d <- filter_df(d, report_filters)
    }
    return(d)
}

add_where_clause <- function(query_lines, filter_where_clause) {
    if(any(grepl("WHERE", query_lines))) {
        existing_where_clause_position <- (1:length(query_lines))[grepl("WHERE", query_lines)]
        existing_where_clause <- query_lines[[existing_where_clause_position]]
        where_clause <- paste(filter_where_clause, gsub("WHERE", "AND", existing_where_clause), sep=" ")
        query_lines[[existing_where_clause_position]] <- where_clause
    } else {
        query_lines[[length(query_lines)]] <- paste(where_clause, ";", sep="")
    }
    return(query_lines)
}

# Apply filters in report_config to dataframe.
# Only applied to data imported from csv since it is not queried with a WHERE
# clause applied.
filter_df <- function(df, filters) {
    for(i in filters) {
        colname <- i[["column"]]
        if(i[["filter_type"]] %in% c("range")) {
            lower <- i[["lower"]]
            upper <- i[["upper"]]
            if(inherits(df[colname], "Date")) {
                lower <- as.Date(lower, "%Y-%m-%d")
                upper <- as.Date(upper, "%Y-%m-%d")
            }
            df <- df[df[[colname]] > lower & df[[colname]] <= upper, ]
        }
        if(i[["filter_type"]] %in% c("in")) {
            df <- df[df[[colname]] %in% i[["values"]], ]
        }
        if(i[["filter_type"]] %in% c("contains")) {
            df <- df[grepl(i[["value"]], df[[colname]]), ]
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
            if(inherits(DT[, get(colname)], "Date")) {
                lower <- as.Date(lower, "%Y-%m-%d")
                upper <- as.Date(upper, "%Y-%m-%d")
            }
            DT <- DT[get(colname)>=lower & get(colname)<upper]
            # DT <- DT[get(i[["column"]])>=unlist("lower"]]) & get(i[["column"]])<unlist(i[["upper"]])]
        }
        if(i[["filter_type"]] %in% c("in")) {
            DT <- DT[get(colname) %in% unlist(i[["values"]])]
        }
        if(i[["filter_type"]] %in% c("contains")) {
            DT <- DT[get(colname) %like% i[["value"]]]
        }
    }
    return(DT)
}
