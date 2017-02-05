todays_date <- format(Sys.Date(), "%Y%m%d")
rdata_filename <- "TOL_Individual_Profile_Data.Rdata"
rdata_folder <- file.path("..")
rdata_locality_folderfilename <- file.path("..", "orgid_locality_lookup.Rdata")

get_most_recent_rdata_filename <- function() {
    rdata_filenames <- sort(dir(path=rdata_folder, pattern=rdata_filename))
    return(rdata_filenames[length(rdata_filenames)])
}

get_rdata_age_days <- function(most_recent_rdata_filename) {
    return(as.numeric(difftime(as.Date(todays_date, "%Y%m%d"),
                               as.Date(substring(most_recent_rdata_filename, 1, 8), "%Y%m%d"), units="days")))
}

delete_old_rdata <- function(filename_to_keep) {
    # Identify any old copies of Rdata and delete
    rdata_filenames <- dir(path=rdata_folder, pattern=rdata_filename)
    old_rdata_filenames <- setdiff(rdata_filenames, filename_to_keep)
    if(length(old_rdata_filenames)>0) {
        lapply(old_rdata_filenames, function(x) file.remove(file.path(rdata_folder, x)))
    }
    return()
}

new_rdata_folderfilename <- function() {
    return(file.path(rdata_folder, paste(todays_date, rdata_filename, sep="_")))
}

update_data_source_if_recent <- function(data_source, data_sources) {
    most_recent_rdata_filename <- get_most_recent_rdata_filename()
    if(length(most_recent_rdata_filename)==0) {
        data_source <- "thrive"
    } else if (get_rdata_age_days(most_recent_rdata_filename)>if_recent_days) {
        data_source <- "thrive"
    } else {
        data_source <- "rdata"
        data_sources$rdata <- file.path("..", most_recent_rdata_filename)
    }
    return(list("data_source"=data_source, "data_sources"=data_sources))
}

