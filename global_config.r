working_dir <- getwd()
plots_dir <- file.path(working_dir, "plots")
sql_dir <- file.path(working_dir, "sql")
ifelse(!dir.exists(plots_dir), dir.create(plots_dir), FALSE)

# data_source
data_sources = list(
    "thrive"="ThriveServer",
    "csv"=file.path("\\", "LS-WVL7F6", "share", "Documents", "Select", "Thrive", "In", "TOL Individual Profile Data Sample for SSS5.csv")
)

# data_source determines whether data is to be retrived from:
# thrive_online - the live database
# csv - a local copy of the sample csv data
data_source = "thrive"

# which was created the last time data was retrived from thrive_online
# rdata 
if_recent_days <- 2
try_use_rdata_if_recent = TRUE

# Define storage types of csv data
c_classes = c("character", "numeric", "numeric", "character", "character",
              "character", "numeric", "character", "numeric", "character",
              "character", "character", "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric", "numeric", "numeric")

# Style guide
style_guide <- list(
    "fonts" = list(
        "font_main_headings"="Museo Slab",
        "font"="ITC Franklin Gothic"
    ),
    "colour_palette" = list(
        "purple"="#BD457B",
        "blue"="#3369A5",
        "green"="#8BB63A",
        "yellow"="#F1CA00",
        "orange"="#D57B16",
        "red"="#C33415"
    ),
    "devstrand_colours" = list(
        "Power & Identity"="green",
        "Thinking"="yellow",
        "Doing"="orange",
        "Being"="red"
    )
)

chart_col_labels = list(
    "Organisation"="Organisation", 
    "Account_ID"="Account ID",
    "Child_ID"="Pupil ID",
    "Status"="Status",
    "Child_DOB"="Date of birth",
    "School_Year"="School Year",
    "Age"="Age",
    "Gender"="Gender",
    "Profile_ID"="Profile ID",
    "Completed_Date"="Date of assessment",
    "Profile_Type"="Profile type",
    "Dev_Stage"="Development stage",
    "Overall_Score"="Score",
    "locality"="Locality",
    "score_change"="Average change in score",
    "School_Year_Child_ID"="School Year-Pupil ID",
    "Child_ID_School_year"="Pupil ID-School Year",
    "Completed_Date_yy_mm_dd"="Date of assessment"
)

# Update data_source if recent Rdata has been requested
if(try_use_rdata_if_recent) {
    updated_sources <- update_data_source_if_recent(data_source, data_sources)
    data_source <- updated_sources$data_source
    data_sources <- updated_sources$data_sources
}

sprintf("Using data source: %s", data_source)

# DO NOT EDIT BELOW.
# Functions to extract info out of the style guide config.
get_devstrand_colour_palette <- function(style_guide) {
    return(lapply(style_guide[["devstrand_colours"]], function(x) style_guide[["colour_palette"]][[x]]))
}

get_devstrand_categories <- function(style_guide) {
    return(list(names(style_guide[["devstrand_colours"]])))
}

get_column_labels <- function(col_labels, cols) {
    aa <- sapply(cols, function(x) col_labels[[x]])
    return(paste(aa, collapse=" / "))
}
