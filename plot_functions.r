library(ggplot2)
library(scales)

bar_stacked <- function(type, title, measure, xaxis, colour_by, filter, data_set_name) {

    data_set <-  get(data_set_name)

    # Filter data as per config for chart
    if(unlist(names(filter))=="all") {
        filter_text <- "all"
    } else {
        filter_text <- paste(filter[[1]][["column"]], paste(filter[[1]][["values"]], collapse=""), sep="_")
        data_set <- filter_dt(data_set, filter)
    }

    # Use config to create a unique filename for chart
    filename <- paste(type, measure, xaxis, colour_by, filter_text, sep="_")
    filename <- paste(filename, "png", sep=".")
    folder_filename <- file.path(plots_dir, filename)
    sprintf("Saving plot: %s", folder_filename)

    # Creae model formular fro mconfig for aggregate function
    formula_str <- paste(measure, "~", xaxis, "+", colour_by, sep=" ")
    dp <- aggregate(formula(formula_str), data_set, mean)

    # Plot setings
    default_font_size = 10
    devstrand_colour_palette <- get_devstrand_colour_palette(style_guide)

    # Plot and save
    ggplot(data=dp, aes(x=get(xaxis), y=get(measure), fill=get(colour_by))) +
        geom_bar(stat = "identity") +
        xlab("School year") + ylab("Average change in score") +
        ggtitle(title)  +
        theme(plot.title = element_text(lineheight=.8, face="bold", size=default_font_size)) +
        scale_fill_manual(values=unlist(devstrand_colour_palette)) + 
        scale_x_discrete(limits=as.character(report_filters[["school_year_filter"]][["values"]])) +
        scale_y_continuous(labels=percent) +
        theme(panel.background = element_rect(fill = "white")) +
        theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
        guides(fill = guide_legend(title = "Development stage", title.position = "top")) +
        theme(axis.title = element_text(size=default_font_size)) +
        theme(axis.text.x = element_text(angle = 0, vjust=0.5, hjust=0.5, size=default_font_size)) +
        theme(axis.text.y = element_text(angle = 0, vjust=0.5, hjust=0.5, size=default_font_size)) +
        theme(legend.title = element_text(colour="black", size=default_font_size, face="bold"), legend.position = "right", legend.text = element_text(colour="black", size=default_font_size))
    ggsave(folder_filename, width = 20, height = 8, units = "cm")
}
