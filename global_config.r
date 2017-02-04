working_dir = getwd()
plots_dir = file.path(working_dir, "plots")

todays_date = format(Sys.Date(), "%Y%m%y")
rdata_filename = "_TOL_Individual_Profile_Data.Rdata"

# data_source
data_sources = list(
    "thrive"="ThriveServer",
    "rdata"=paste(todays_date, rdata_filename),
    "csv"="TOL Individual Profile Data Sample for SSS5.csv"
)

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

todays_date = "zip"
rdata_filename = "Report"

up_to_date_rdata_exists <- function() {
    rdata_filenames = dir(pattern=rdata_filename)
    return(grepl(todays_date, rdata_filenames))
}
up_to_date_rdata_exists()
