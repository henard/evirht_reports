# Set up references to folders to use when saving plots or data or querying 
# sql queries to retrieve data from thrive online.
working_dir <- getwd()
plots_dir <- file.path(working_dir, "plots")
data_dir <- file.path(working_dir, "rdata")
sql_dir <- file.path(working_dir, "sql")
ifelse(!dir.exists(plots_dir), dir.create(plots_dir), FALSE)

# Specify reference to the thive_online data sources.
# "ThriveServer" is the name given to the ODBC connection
data_sources = list(
    "thrive"="ThriveServer"
)

# data_source determines whether data is to be retrived from:
# "thrive" is the live thrive_online database
data_source = "thrive"

# A second data source is a copy of the data last grabbed from thrive online
# which is saved locally in folder called RData in your working direrctory.
# The filename of the RData copy of data begins with a date which is used to
# identify how many days since it was last updated. If
# try_use_rdata_if_recent = TRUE; and
# No. days since it was last updated is < if_recent_days
# then rdata local copy will be used
if_recent_days <- 10
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
        "red"="#C33415",
        "allocated_green"="#009900",
        "active_green"="#006600",
        "profiled_green"="#003300",
        "white"="#FFFFFF"
    ),
    "devstrand_colours" = list(
        "Power & Identity"="green",
        "Thinking"="yellow",
        "Doing"="orange",
        "Being"="red"
    ),
    "pupil_pct_colours" = list(
        "Active"="active_green",
        "Profiled"="profiled_green",
        "indProfiled"="white"
    ),
    "pupil_counts_colours" = list(
        "Allocated"="allocated_green",
        "Active"="active_green",
        "Profiled"="profiled_green",
        "indProfiled"="red"
    )
)

# A look-up of column name used in chart and corresponding x and y-axis labels.
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
    "Completed_Date_yy_mm_dd"="Date of assessment",
    "Child_ID_Completed_date"="Pupil ID-Date of Assessment",
    "pct"="Percentage of allocated pupils",
    "N"="Number of pupils",
    "pupil_count_type"="Active / profiled"
)

################################## END OF CONFIG ######################################################################
################################## BELOW ARE FUNCTIONS ASSOCIATED WITH CONFIG ABOVE  - Do not edit ####################

# Functions to extract info out of the style guide config.
get_colour_palette <- function(style_guide, colours) {
    return(lapply(style_guide[[colours]], function(x) style_guide[["colour_palette"]][[x]]))
}

get_devstrand_categories <- function(style_guide) {
    return(list(names(style_guide[["devstrand_colours"]])))
}

get_colourby_categories <- function(style_guide, colourby_colours) {
    return(list(names(style_guide[[colourby_colours]])))
}

get_column_labels <- function(col_labels, cols) {
    aa <- sapply(cols, function(x) col_labels[[x]])
    return(paste(aa, collapse=" / "))
}
