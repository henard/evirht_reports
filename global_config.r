# Set working directory
# working_dir = "/home/henard/dev/r/evirht_reports"
working_dir = file.path("/", "home", "henard", "dev", "r", "evirht_reports")
setwd(working_dir)
plots_dir = file.path("/", "home", "henard", "dev", "r", "evirht_reports", "plots")
# getwd()

# data_source
data_sources = list("thrive", "local", "local_file")
data_source = unlist(data_sources[[2]])

# db logn credentials file path
login_credentials_location = "~/.my.cnf"
csv_file_location = "/media/sf_nas_share/Documents/Select/Thrive/In/TOL Individual Profile Data Sample for SSS2.csv"

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

get_devstrand_colour_palette <- function(style_guide) {
    return(lapply(style_guide[["devstrand_colours"]], function(x) style_guide[["colour_palette"]][[x]]))
}

devstrand_colour_palette <- get_devstrand_colour_palette(style_guide)
devstrand_colour_palette

get_devstrand_categories <- function(style_guide) {
    return(list(names(style_guide[["devstrand_colours"]])))
}

devstrand_categories <- get_devstrand_categories(style_guide)
devstrand_categories