# Specify location to look for a locally stored copy of data in RData format
todays_date <- format(Sys.Date(), "%Y%m%d")
rdata_filename <- "TOL_Individual_Profile_Data.RData"
rdata_locality_folderfilename <- file.path(data_dir, "orgid_locality_lookup.RData")
rdata_pupil_counts_folderfilename <- file.path(data_dir, "pupil_counts_by_type_and_organisation.RData")

# Check for a locally stored copy of data in RData format
get_most_recent_rdata_filename <- function() {
    rdata_filenames <- sort(dir(path=data_dir, pattern=rdata_filename))
    return(rdata_filenames[length(rdata_filenames)])
}

# Check how many days old it is using the date prefixed to the filename
get_rdata_age_days <- function(most_recent_rdata_filename) {
    return(as.numeric(difftime(as.Date(todays_date, "%Y%m%d"),
                               as.Date(substring(most_recent_rdata_filename, 1, 8), "%Y%m%d"), units="days")))
}

# Remove any old copies lying around
delete_old_rdata <- function(filename_to_keep) {
    # Identify any old copies of RData and delete
    rdata_filenames <- dir(path=data_dir, pattern=rdata_filename)
    old_rdata_filenames <- setdiff(rdata_filenames, filename_to_keep)
    if(length(old_rdata_filenames)>0) {
        lapply(old_rdata_filenames, function(x) file.remove(file.path(data_dir, x)))
    }
    return()
}

# Specify filename of new RData being saved
new_rdata_folderfilename <- function() {
    return(file.path(data_dir, paste(todays_date, rdata_filename, sep="_")))
}

# Update data_sources in global_config.r with details of most recent RData copy of data
update_data_source_if_recent <- function(data_source, data_sources) {
    most_recent_rdata_filename <- get_most_recent_rdata_filename()
    if(length(most_recent_rdata_filename)==0) {
        data_source <- "thrive"
    } else if (get_rdata_age_days(most_recent_rdata_filename)>if_recent_days) {
        data_source <- "thrive"
    } else {
        data_source <- "rdata"
        data_sources$rdata <- file.path(data_dir, most_recent_rdata_filename)
    }
    return(list("data_source"=data_source, "data_sources"=data_sources))
}

# Update data_source if recent RData has been requested
if(try_use_rdata_if_recent) {
    updated_sources <- update_data_source_if_recent(data_source, data_sources)
    data_source <- updated_sources$data_source
    data_sources <- updated_sources$data_sources
}
sprintf("Using data source: %s", data_source)

# All existing results data are deleted each time main.r is run.
delete_existing_results_data <- function() {
    # Identify all report rdata in data directory
    plot_filenames <- dir(path=data_dir, pattern="report*")
    plot_filenames <- plot_filenames[!grepl("fullheading", plot_filenames)]
    if(length(plot_filenames)>0) {
        lapply(plot_filenames, function(x) file.remove(file.path(data_dir, x)))
    }
    return()
}

delete_existing_results_data()
