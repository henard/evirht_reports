library(data.table)
library(plyr)

# Apply changes to format of data and derive analysis variables (irrespective of data_source)
format_dataframe <- function(df) {
    devstrand_categories <- get_devstrand_categories(style_guide)
    df$Overall_Score <- df$Overall_Score/100
    
    df$Status <- factor(df$Status, c("active", "inactive", "historical", "transferred"), c("Active", "Inactive", "Historical", "Transferred"))
    df$School_Year <- factor(df$School_Year)
    
    df$Child_ID <- as.character(df$Child_ID)
    df$Organisation <- as.character(df$Organisation)
    df$School_Year_Child_ID <- as.character(interaction(df$School_Year, df$Child_ID, sep="-"))
    df$Child_ID_School_Year <- as.character(interaction(df$Child_ID, df$School_Year, sep="-"))
    df$Completed_Date_yy_mm_dd <- strftime(df$Completed_Date, "%y-%m-%d")
    df$Child_ID_Completed_date <- as.character(interaction(df$Completed_Date_yy_mm_dd, df$Child_ID, sep="-"))
    
    # Apply cleaning described in dissclaimer text in the Sample reports
    df <- df[df$Dev_Stage %in% unlist(devstrand_categories), ]
    df$Dev_Stage <- factor(df$Dev_Stage, unlist(devstrand_categories))
    df <- df[!grepl("Trainers - ", df$Organisation), ]
    df <- df[df$Age>=0 & df$Age<=100, ]
    dedup_colnames <- c("Child_ID", "Overall_Score", "Completed_Date", "Dev_Stage")
    df <- df[!duplicated(df[, dedup_colnames]), ]
    return(df)
}

# Apply changes to format of data specific to data imported from SQL
format_sql_df <- function(df) {
    df$Dev_Stage <- factor(df$Dev_Stage, 6:1, c("Interdependence", "Skill & Structure", "Power & Identity", "Thinking", "Doing", "Being"))
    return(df)
}

# Apply changes to format of data specific to pupil_counts data imported (from SQL)
format_pupil_counts_df <- function(df) {
    pupil_count_categories <- get_colourby_categories(style_guide, "pupil_counts_colours")
    df$Organisation <- as.character(df$Organisation)
    df$pct_Allocated <- df$N_Allocated / df$N_Allocated
    df[df$N_Allocated==0 & !is.na(df$N_Allocated), "pct_Allocated"] <- 0
    df$pct_Active <- (df$N_Active-df$N_Profiled) / df$N_Allocated
    # df$pct_Active <- df$N_Active / df$N_Allocated
    df[df$N_Allocated==0 & !is.na(df$N_Allocated), "pct_Active"] <- 0
    df$pct_Profiled <- df$N_Profiled / df$N_Allocated
    df[df$N_Allocated==0 & !is.na(df$N_Allocated), "pct_Profiled"] <- 0
    dfl <- reshape(df, varying=c("N_Allocated", "N_Active", "N_Profiled", "pct_Allocated", "pct_Active", "pct_Profiled"),
                   direction="long", idvar="Organisation_ID", sep="_")
    names(dfl)[names(dfl) == 'time'] <- "pupil_count_type"
    # dfl <- dfl[dfl$pupil_count_type %in% c("Active", "Profiled"), ]
    dfl[!dfl$pupil_count_type %in% c("Active", "Profiled"), "pct"] <- NA
    dfl[!dfl$pupil_count_type %in% c("Active", "Profiled", "Allocated"), "N"] <- NA
    dfl <- data.table(dfl)
    dfl$pupil_count_type <- factor(dfl$pupil_count_type, levels=unlist(pupil_count_categories))
    return(dfl)
}

# Apply changes to format of data specific to data imported from csv
format_csv_df <- function(df) {
    df <- df[, -(13:31)]
    df$School_Year <- factor(df$School_Year, c("AY", "EY", "Nursery", "Reception", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"),
                             c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))
    df$Child_DOB <- as.Date(df$Child_DOB, "%d/%m/%Y")
    df$Completed_Date <- as.Date(df$Completed_Date, "%d/%m/%Y")
    df$Dev_Stage <- factor(df$Dev_Stage, c("Interdependence", "Skill & Structure", "Power & Identity", "Thinking", "Doing", "Being"),
                           c("Interdependence", "Skill & Structure", "Power & Identity", "Thinking", "Doing", "Being"))
    return(df)
}

# Function to identify levels present in factor variables.
# Used on columns in filtered data where some levels may heve been excluded or lost.
levels_present <- function(column) {
    levels_present <- sort(unique(as.character(column)))
    if(inherits(column, "factor")) {
        levels_present <- levels(column)[levels(column) %in% levels_present]
    }
    return(levels_present)
}

# When data is aggregated for ploting, factor levels can be lost.
# This expands the dataframe row-wise so all combinations of factors exist as a 
# row
expand_dataframe <- function(df, measure_colname, factors_only=TRUE) {
    # List to hold the levels over which to expand the dataframe by
    ll <- list()

    # Determine th list of columns to include to identify the levels
    if(factors_only) {
        columns <- setdiff(names(df)[sapply(df, is.factor)], measure_colname)
    } else {
        columns <- setdiff(names(df), measure_colname)
    }

    # Identify the levels
    for(column in columns) {
        if(inherits(df[, column], "factor")) {
            ll[[column]] <- levels(df[, column])
        } else {
            ll[[column]] <- levels(factor(df[, column]))
        }
    }

    if(length(ll)>0) {
        df <- merge(expand.grid(ll), df, all.x=TRUE)
        for(column in measure_colname) {
            df[is.na(df[, column]), column] <- 0.00001
        }
    }
    return(df)
}

# Helper function to parse a list of parameters to data.table
parseify_list <- function(list_of_colnames)
    return(as.quoted(paste("list(", paste(list_of_colnames, collapse=", "),")"))[[1]])

# Take dataframe df, convert to data.table of assessment scores
score_data <- function(df, groups) {
    # Convert to data.table
    DT <- data.table(df)
    
    # Add a variable that can be summed to indicate the number of rows
    DT[, c:=1]
    
    # Slim down dataset (optional)
    # DT[, (cols_to_remove):=NULL]

    # Convert by_cols to a form that can be parsed in.
    by_cols_parse <- parseify_list(groups)
    
    # rank within groups
    DT[, Assessment_n := rank(Completed_Date, ties.method="first"), by=by_cols_parse]
    DT[, .("N" = sum(c)), by=.(Assessment_n)]
    
    # Max rank within groups
    DT[, N_assessments := max(Assessment_n), by=by_cols_parse]
    DT[, .("N" = sum(c)), by=.(N_assessments)]
    
    # rank within groups reverse
    DT[, Assessment_n_rev := -1*(N_assessments-Assessment_n)-1]
    DT[, .("N" = sum(c)), by=.(Assessment_n_rev)]

    return(DT)
}

# Takes data.table of Assessment scores a calculates percentage point change in
# assessment scores with details of their last assessment.
score_change_data <- function(DT, groups) {

    # Convert by_cols to a form that can be parsed in.
    by_cols_parse <- parseify_list(groups)
    
    # Filter to first and last assessments for those with 2+ assessements
    DT <- DT[(Assessment_n==1 | Assessment_n_rev==-1) & N_assessments>=2]    

    # Calculate 
    DT[Assessment_n==1, score_first := Overall_Score, by=by_cols_parse]
    DT[, score_first := max(score_first, na.rm=TRUE), by=by_cols_parse]
    DT[Assessment_n_rev==-1, score_last := Overall_Score, by=by_cols_parse]
    DT[, score_last := max(score_last, na.rm=TRUE), by=by_cols_parse]
    DT[, score_change := (score_last-score_first), by=by_cols_parse]

    return(DT[Assessment_n_rev==-1])
}

movsum <- function(x, n=2) {filter(x,rep(1,n), sides=1, circular=TRUE)}

add_xposoffset <- function(data, measure) {
    if(nrow(data)>=2) {
        data$space <- (movsum(data[, measure], 2) + rev(movsum(rev(data[, measure]), 2)))/4
    } else {
        data$space <- 1
    }
    data$xlaboffset <- 0
    data[data$space<=0.05, "xlaboffset"] <- 1
    return(data)
}

pupil_shares_data <- function(type, title, measure, by, filter, filename, long_filename, dataset, devstrand_categories, auto_title) {

    df <-  get(dataset)

    # Create model formular from config for aggregate function
    formula_str <- paste(measure, "~", by, sep=" ")

    first <- aggregate(formula(formula_str), data=df[df$N_assessments>=2 & df$Assessment_n==1], FUN=sum)
    first$pct <- first$c/sum(first$c)
    names(first) <- c("Dev_Stage", "first_n", "first_pct")
    last <- aggregate(c ~ Dev_Stage, data=df[df$N_assessments>=2 & df$Assessment_n_rev==-1], FUN=sum)
    last$pct <- last$c/sum(last$c)
    names(last) <- c("Dev_Stage", "last_n", "last_pct")
    first_last_empty <- merge(first, last, all=TRUE)
    first_last_empty[, c("first_n", "first_pct", "last_n", "last_pct")] = 0

    # Filter data as per config for chart
    if(!("all" %in% unlist(names(filter)))) df <- filter_dt(df, filter)

    # Create an indicator of when the sample size is too small, if so use empty plot dataframe
    min_sample_size <- 1
    sample_size_too_small <- nrow(df)<min_sample_size
    if(sample_size_too_small) {
        first_last <- first_last_empty
    } else {
        # Aggregate filtered data to create data for plotting
        first <- aggregate(formula(formula_str), data=df[df$N_assessments>=2 & df$Assessment_n==1], FUN=sum)
        first$pct <- first$c/sum(first$c)
        names(first) <- c("Dev_Stage", "first_n", "first_pct")
        last <- aggregate(c ~ Dev_Stage, data=df[df$N_assessments>=2 & df$Assessment_n_rev==-1], FUN=sum)
        last$pct <- last$c/sum(last$c)
        names(last) <- c("Dev_Stage", "last_n", "last_pct")
        first_last <- merge(first, last, all=TRUE)
    }

    first_last$Dev_Stage <- factor(first_last$Dev_Stage, levels=devstrand_categories)
    first_last <- expand_dataframe(first_last, c("first_n", "first_pct", "last_n", "last_pct"))
    first_last <- first_last[rev(order(first_last$Dev_Stage)), ]
    first_last$pct_change <- paste(round(100*(abs(first_last$last_pct-first_last$first_pct)), 0), "%", sep="")
    first_last[first_last$pct_change=="0%", "pct_change"] <- ""
    first_last$dir_change <- as.character(factor(first_last$last_pct>first_last$first_pct, c(TRUE, FALSE), c("increase","reduction")))
    first_last[first_last$pct_change=="", "dir_change"] <- "unchanged"
    first_last$change_text <- paste(first_last$pct_change, first_last$dir_change, sep=" ")
    first_last$change_text2 <- paste("Pupils at", toupper(first_last$Dev_Stage), first_last$pct_change, first_last$dir_change, sep=" ")
    save(first_last, file=file.path("rdata", filename))
}
