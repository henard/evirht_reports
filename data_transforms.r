library(data.table)
library(plyr)

# Apply changes to format of data and derive analysis variables
format_dataframe <- function(df) {
    devstrand_categories <- get_devstrand_categories(style_guide)
    df$Overall_Score <- df$Overall_Score/100
    df$Dev_Stage <- factor(df$Dev_Stage, unlist(devstrand_categories))
    df$locality <- df$Profile_ID-10*floor(df$Profile_ID/10)

    df$School_Year <- factor(df$School_Year, c("AY", "EY", "Nursery", "Reception", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"),
                             c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))
    df$School_Year_Child_ID <- interaction(df$School_Year, df$Child_ID, sep="-")
    df$Child_ID_School_Year <- interaction(df$Child_ID, df$School_Year, sep="-")
    df$Completed_Date_yy_mm_dd <- strftime(df$Completed_Date, "%y-%m-%d")
    return(df)
}

# Apply changes to format of data and derive analysis variables
format_csv_df <- function(df) {
    devstrand_categories <- get_devstrand_categories(style_guide)
    
    df$Dev_Stage <- factor(df$Dev_Stage, unlist(devstrand_categories))
    df$locality <- df$Profile_ID-10*floor(df$Profile_ID/10)
    df$Child_DOB <- as.Date(df$Child_DOB, "%d/%m/%Y")
    df$Completed_Date <- as.Date(df$Completed_Date, "%d/%m/%Y")
    return(df)
}

# Helper function to parse a list of parameters to data.table
parseify_list <- function(list_of_colnames)
    return(as.quoted(paste("list(", paste(list_of_colnames, collapse=", "),")"))[[1]])

# Take dataframe df, convert to data.table of 
# assessment scores
score_data <- function(df, groups) {
    # Convert to data.table
    DT <- data.table(df)
    
    # Add a variable that can be summed to indicate the number of rows
    DT[, c:=1]
    
    # Slim down dataset (optional)
    cols_to_remove = names(DT)[13:31]
    # print(cols_to_remove)
    DT[, (cols_to_remove):=NULL]

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
