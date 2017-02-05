# Install required packages as necesssary
required_packages = c("RODBC", "data.table", "plyr", "ggplot2", "scales")
for(package in required_packages) {
    if(!(package %in% installed.packages()[,"Package"])) install.packages(package)  
}

# Specify location to look for a locally stored copy of data in Rdata format
todays_date <- format(Sys.Date(), "%Y%m%d")
rdata_filename <- "TOL_Individual_Profile_Data.Rdata"
rdata_folder <- "rdata"
rdata_locality_folderfilename <- file.path(rdata_folder, "orgid_locality_lookup.Rdata")

# Check for a locally stored copy of data in Rdata format
get_most_recent_rdata_filename <- function() {
    rdata_filenames <- sort(dir(path=rdata_folder, pattern=rdata_filename))
    return(rdata_filenames[length(rdata_filenames)])
}

# Check how many days old it is using the date prefixed to the filename
get_rdata_age_days <- function(most_recent_rdata_filename) {
    return(as.numeric(difftime(as.Date(todays_date, "%Y%m%d"),
                               as.Date(substring(most_recent_rdata_filename, 1, 8), "%Y%m%d"), units="days")))
}

# Remove any old copies lying around
delete_old_rdata <- function(filename_to_keep) {
    # Identify any old copies of Rdata and delete
    rdata_filenames <- dir(path=rdata_folder, pattern=rdata_filename)
    old_rdata_filenames <- setdiff(rdata_filenames, filename_to_keep)
    if(length(old_rdata_filenames)>0) {
        lapply(old_rdata_filenames, function(x) file.remove(file.path(rdata_folder, x)))
    }
    return()
}

# Specify filename of new Rdata being saved
new_rdata_folderfilename <- function() {
    return(file.path(rdata_folder, paste(todays_date, rdata_filename, sep="_")))
}

# Update data_sources in global_config.r with details of most recent Rdata copy of data
update_data_source_if_recent <- function(data_source, data_sources) {
    most_recent_rdata_filename <- get_most_recent_rdata_filename()
    if(length(most_recent_rdata_filename)==0) {
        data_source <- "thrive"
    } else if (get_rdata_age_days(most_recent_rdata_filename)>if_recent_days) {
        data_source <- "thrive"
    } else {
        data_source <- "rdata"
        data_sources$rdata <- file.path(rdata_folder, most_recent_rdata_filename)
    }
    return(list("data_source"=data_source, "data_sources"=data_sources))
}
