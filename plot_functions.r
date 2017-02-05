library(ggplot2)
library(scales)
# library(dplyr)

bar_chart <- function(type, title, measure, xaxis, xgroup, colour_by, filter, data_set_name) {
    
    # Determine stacked or side-by-side bar chart
    postn = "stack"
    if(grepl("side", type)) {
        postn = "dodge"
    }
    
    data_set <-  get(data_set_name)

    # Configure whether to group x-axis categories by a second variable
    if(xgroup %in% names(data_set)) {
        grouped = TRUE
        xlab <- c(xgroup, xaxis)
    } else {
        grouped = FALSE
        xlab <- xaxis
    }

    # Filter data as per config for chart
    if("all" %in% unlist(names(filter))) {
        filter_text <- "all"
    } else {
        filter_text <- paste(filter[[1]][["column"]], paste(filter[[1]][["values"]], collapse=""), sep="_")
        data_set <- filter_dt(data_set, filter)
    }
    
    # Use config to create a unique filename for chart
    filename <- paste(type, measure, xaxis, xgroup, colour_by, filter_text, sep="_")
    filename <- paste(filename, "png", sep=".")
    folder_filename <- file.path(plots_dir, filename)
    sprintf("Saving plot: %s", folder_filename)
    
    # Create model formular from config for aggregate function
    formula_str <- paste(measure, "~", paste(c(xlab, colour_by), collapse=" + "), sep=" ")
    dp <- aggregate(formula(formula_str), data_set, mean)

    # Make x-axis variables factors with levels limited to those still present in the data after filtering
    {if(grouped) dp[, xgroup] <- factor(dp[, xgroup], levels=levels_present(dp[, xgroup]))}
    dp[, xaxis] <- factor(dp[, xaxis], levels=levels_present(dp[, xaxis]))

    # Plot setings
    default_font_size = 10
    devstrand_colour_palette <- get_devstrand_colour_palette(style_guide)
    
    # Plot and save
    p = ggplot(data=dp, aes(x=get(xaxis), y=get(measure), fill=get(colour_by))) +
        geom_bar(stat = "identity", position=postn, colour="black", size=0.2) +
        xlab(get_column_labels(chart_col_labels, xlab)) +
        ylab(get_column_labels(chart_col_labels, measure)) +
        {if(grouped) facet_grid(~get(xgroup), switch = "x", scales = "free_x", space = "free_x")} +
        {if(grouped) theme(panel.spacing = unit(0, "lines"), strip.background = element_blank(), strip.placement = "outside")} +
        ggtitle(title) +
        theme(plot.title = element_text(lineheight=.8, face="bold", size=default_font_size)) +
        theme(panel.grid.major.y = element_line(colour = "grey", linetype = "solid", size=0.2), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) + 
        scale_fill_manual(values=unlist(devstrand_colour_palette)) + 
        scale_colour_discrete(drop = FALSE) +
        scale_x_discrete(drop = FALSE) +
        scale_y_continuous(labels=percent) +
        theme(panel.background = element_rect(fill = "white")) +
        theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
        guides(fill = guide_legend(title = get_column_labels(chart_col_labels, colour_by), title.position = "top")) +
        theme(axis.title = element_text(size=default_font_size)) +
        theme(axis.text.x = element_text(angle = 0, vjust=0.5, hjust=0.5, size=default_font_size)) +
        theme(axis.text.y = element_text(angle = 0, vjust=0.5, hjust=0.5, size=default_font_size)) +
        theme(legend.title = element_text(colour="black", size=default_font_size, face="bold"), legend.position = "right", legend.text = element_text(colour="black", size=default_font_size))

    ggsave(folder_filename, plot = p, width = 20, height = 8, units = "cm")
}

pie_chart <- function(type, title, measure, xgroup, colour_by, filter, data_set_name) {
    
    data_set <-  get(data_set_name)
    
    # Configure whether to group x-axis categories by a second variable
    if(xgroup %in% names(data_set)) {
        grouped = TRUE
        xlab <- xgroup
    } else {
        grouped = FALSE
        xlab <- ""
    }
    
    # Filter data as per config for chart
    if("all" %in% unlist(names(filter))) {
        filter_text <- "all"
    } else {
        filter_text <- paste(filter[[1]][["column"]], paste(filter[[1]][["values"]], collapse=""), sep="_")
        data_set <- filter_dt(data_set, filter)
    }
    
    # Use config to create a unique filename for chart
    filename <- paste(type, measure, xgroup, colour_by, filter_text, sep="_")
    filename <- paste(filename, "png", sep=".")
    folder_filename <- file.path(plots_dir, filename)
    sprintf("Saving plot: %s", folder_filename)
    
    # Create model formular from config for aggregate function
    formula_str <- paste(measure, "~", paste(c(xlab, colour_by), collapse=" + "), sep=" ")
    dp <- aggregate(formula(formula_str), data_set, sum)
    dp[, measure] <- dp[, measure]/sum(dp[, measure])
    dp[, "y_pos"] <- 1-(cumsum(dp[, measure]) - dp[, measure]/2)
    print(dp)

    # Make x-axis variables factors with levels limited to those still present in the data after filtering
    {if(grouped) dp[, xgroup] <- factor(dp[, xgroup], levels=levels_present(dp[, xgroup]))}
    dp[, xaxis] <- factor(dp[, xaxis], levels=levels_present(dp[, xaxis]))
    
    # Plot setings
    default_font_size = 10
    devstrand_colour_palette <- get_devstrand_colour_palette(style_guide)
    
    blank_theme <- theme_minimal() +
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank(),
            panel.grid  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(lineheight=.8, face="bold", size=default_font_size)
        )

    p <- ggplot(dp, aes(x="", y=get(measure), fill=get(colour_by))) +
        geom_bar(width = 1, stat = "identity", colour="black", size=0.2) +
        coord_polar("y", start=0) + 
        ggtitle(title)  +
        geom_text(aes(x = 1.2, y = dp[, "y_pos"], label = percent(dp[, measure])), size=3) +
        theme(axis.text.x=element_blank()) +
        scale_fill_manual(values=unlist(devstrand_colour_palette)) + 
        blank_theme +
        guides(fill = guide_legend(title = get_column_labels(chart_col_labels, colour_by), title.position = "top")) +
        theme(legend.title = element_text(colour="black", size=default_font_size, face="bold"), legend.position = "right", legend.text = element_text(colour="black", size=default_font_size))
         
    # Plot and save
    ggsave(folder_filename, plot = p, width = 10, height = 8, units = "cm")
}

delete_existing_plots <- function() {
    # Identify all plots in plot directory
    plot_filenames <- dir(path=plots_dir, pattern="*.png")
    if(length(plot_filenames)>0) {
        lapply(plot_filenames, function(x) file.remove(file.path(plots_dir, x)))
    }
    return()
}

# pie_chart2 <- function(title, measure, xgroup, colour_by, filter, data_set_name) {
# pie_chart2 <- function(xgroup, colour_by, data_set_name) {
# pie_chart2 <- function(df, main, labels = NULL, condition = NULL) {
#     df <-  get(data_set_name)
# 
#     # Convert by_cols to a form that can be parsed in.
#     by_cols_parse <- parseify_list(list(xgroup, colour_by))
#     
#     # convert the data into percentages. group by conditional variable if needed
#     df <- group_by_(df, .dots = by_cols_parse) %>%
#         summarize(counts = n()) %>%
#         mutate(perc = counts / sum(counts)) %>%
#         arrange(desc(perc)) %>%
#         mutate(label_pos = cumsum(perc) - perc / 2,
#                perc_text = paste0(round(perc * 100), "%"))
#     
#     # reorder the category factor levels to order the legend
#     # df[[colour_by]] <- factor(df[[colour_by]], levels = unique(df[[colour_by]]))
#     
#     # if labels haven't been specified, use what's already there
#     # if (is.null(labels)) labels <- as.character(df[[colour_by]])
#     
#     p <- ggplot(data = df, aes_string(x = factor(1), y = "perc", fill = get(colour_by))) +
#         
#         # make stacked bar chart with black border
#         geom_bar(stat = "identity", color = "black", width = 1) +
#         
#         # add the percents to the interior of the chart
#         geom_text(aes(x = 1.25, y = label_pos, label = perc_text), size = 4) +
#         
#         # add the category labels to the chart
#         # increase x / play with label strings if labels aren't pretty
#         geom_text(aes(x = 1.82, y = label_pos, label = labels), size = 4) +
#         
#         # convert to polar coordinates
#         coord_polar(theta = "y") +
#         
#         # formatting
#         scale_y_continuous(breaks = NULL) +
#         scale_fill_discrete(name = "", labels = unique(labels)) +
#         theme(text = element_text(size = 22),
#               axis.ticks = element_blank(),
#               axis.text = element_blank(),
#               axis.title = element_blank())
#     
#     # facet wrap if that's happening
#     if (!is.null(xgroup)) p <- p + facet_wrap(xgroup)
#     
#     ggsave(folder_filename, plot = p, width = 20, height = 8, units = "cm")
# }

