library(data.table)
library(plyr)


format_dataframe <- function(df) {
    df$Dev_Stage <- factor(df$Dev_Stage, unlist(devstrand_categories))
    df$locality <- df$Profile_ID-10*floor(df$Profile_ID/10)
    return(df)
}

# Helper function
parseify_list <- function(list_of_colnames)
    return(as.quoted(paste("list(", paste(list_of_colnames, collapse=", "),")"))[[1]])

# Take dataframe df, convert to data.table of percentage point change in assessment scores
# with details of their last assessment.
score_change_data <- function(df, groups) {
    # Convert to data.table
    DT <- data.table(df)
    
    # Add a variable that can be summed to indicate the number of rows
    DT[, c:=1]
    
    # Slim down dataset (optional)
    cols_to_remove = names(DT)[13:31]
    print(cols_to_remove)
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

    # Filter to first and last assessments for those with 2+ assessements
    DT <- DT[(Assessment_n==1 | Assessment_n_rev==-1) & N_assessments>=2]    

    # Calculate 
    DT[Assessment_n==1, score_first := Overall_Score/100, by=by_cols_parse]
    DT[, score_first := max(score_first, na.rm=TRUE), by=by_cols_parse]
    DT[Assessment_n_rev==-1, score_last := Overall_Score/100, by=by_cols_parse]
    DT[, score_last := max(score_last, na.rm=TRUE), by=by_cols_parse]
    DT[, score_change := (score_last-score_first), by=by_cols_parse]

    return(DT[Assessment_n_rev==-1])
}



# d$c <- 1
# xtabs(c ~ N_assessments + Assessment_n_rev, data=DT)
# summary(d)

# library(ggplot2)
# data(diamonds)

# data.table reference stuff
# DT[, Child_ID]
# setkey(DT, assess_n, assess_n_rev)
# nrow(DT)
# t1 <- DT[assess_n==1 | assess_n_rev ==-1, ppt_change := max(Overall_Score)-min(Overall_Score), by=Child_ID][order(Child_ID)]

# data.table melt/ cast syntax
# ans <- dcast(DT, ID + Month ~ Category, fun=sum)

# data.table merge syntax
# x[,list(x,y)]

