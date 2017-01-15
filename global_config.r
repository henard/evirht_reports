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
        "Power and Identity "="green",
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