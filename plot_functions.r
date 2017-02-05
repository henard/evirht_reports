library(ggplot2)
library(scales)

bar_chart <- function(type, title, measure, xaxis, xgroup, colour_by, filter, filename, long_filename, data_set_name) {
    
    # Determine if plotting child_ids - handled differently
    plotting_child_ids <- any(grepl("Child_ID", c(xaxis, xgroup)))

    # Determine stacked or side-by-side bar chart
    if(grepl("side", type)) postn <- "dodge" else postn <- "stack"

    data_set <-  get(data_set_name)

    # Determine whether x-axis categories are to be grouped by a second variable
    grouped <- (xgroup %in% names(data_set))
    if(grouped) xlab <- c(xgroup, xaxis) else xlab <- xaxis

    # Create model formular from config for aggregate function
    formula_str <- paste(measure, "~", paste(c(xlab, colour_by), collapse=" + "), sep=" ")

    # Create an 'empty' plot dataframe for when sample size <10 due to filtering applied
    dp_empty <- aggregate(formula(formula_str), data_set, mean)
    dp_empty[measure] <- 0.00001

    # Filter data as per config for chart
    if(!("all" %in% unlist(names(filter)))) data_set <- filter_dt(data_set, filter)

    # Check if sample size <10 , if so use empty plot dataframe
    if(plotting_child_ids) min_sample_size <- 3 else min_sample_size <- 10
    sample_size_too_small <- nrow(data_set)<min_sample_size
    if(sample_size_too_small) {
        dp <- dp_empty
    } else {
        dp <- aggregate(formula(formula_str), data_set, mean)
    }
    
    if(plotting_child_ids) {
        # limit number of Child_IDs, convert to character, rotate axis labels
        dp <- dp[dp$Child_ID %in% unique(dp$Child_ID)[1:min(20,length(unique(dp$Child_ID)))], ]
        dp$Child_ID <- as.character(dp$Child_ID)
        xaxis_text_angle = 90
    } else {
        dp[, xaxis] <- factor(dp[, xaxis], levels=levels_present(dp[, xaxis]))
        # Ensure the width of bars is consistent when plot type="side_by_side"
        dp <- expand_dataframe(dp, measure)
        xaxis_text_angle = 0
    }
    
    # Make x-axis variables factors with levels limited to those still present in the data after filtering
    {if(grouped) dp[, xgroup] <- factor(dp[, xgroup], levels=levels_present(dp[, xgroup]))}

    x_mid <- 0.5*length(unique(dp[, xaxis]))
    
    # Plot setings
    default_font_size = 10
    devstrand_colour_palette <- get_devstrand_colour_palette(style_guide)
    
    # Plot and save
    p = ggplot(data=dp, aes(x=get(xaxis), y=get(measure), fill=get(colour_by))) +
        geom_bar(stat = "identity", position=postn, colour="black", size=0.2) +
        {if(grouped & !plotting_child_ids) facet_grid(~get(xgroup), switch = "x", space = "free_x")} +
        {if(grouped & plotting_child_ids) facet_grid(~get(xgroup), switch = "x", scales="free_x", space = "free_x")} +
        {if(grouped) theme(panel.spacing = unit(0, "lines"), strip.background = element_blank(), strip.placement = "outside")} +
        xlab(get_column_labels(chart_col_labels, xlab)) +
        ylab(get_column_labels(chart_col_labels, measure)) +
        ggtitle(title) +
        theme(plot.title = element_text(lineheight=.8, face="bold", size=default_font_size)) +
        theme(panel.grid.major.y = element_line(colour = "grey", linetype = "solid", size=0.2), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) + 
        scale_fill_manual(values=unlist(devstrand_colour_palette)) + 
        scale_colour_discrete(drop = FALSE) +
        scale_x_discrete(drop = FALSE) +
        {if(sample_size_too_small) coord_cartesian(ylim = c(0, 1))} +
        {if(sample_size_too_small) annotate("text", x=x_mid, y=0.5, label= "Insufficient sample")} + 
        scale_y_continuous(labels=percent) +
        theme(panel.background = element_rect(fill = "white")) +
        theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
        guides(fill = guide_legend(title = get_column_labels(chart_col_labels, colour_by), title.position = "top")) +
        theme(axis.title = element_text(size=default_font_size)) +
        theme(axis.text.x = element_text(angle = xaxis_text_angle, vjust=0.5, hjust=0.5, size=default_font_size)) +
        theme(axis.text.y = element_text(angle = 0, vjust=0.5, hjust=0.5, size=default_font_size)) +
        theme(legend.title = element_text(colour="black", size=default_font_size, face="bold"), legend.position = "right", legend.text = element_text(colour="black", size=default_font_size))

    ggsave(file.path(plots_dir, filename), plot = p, width = 20, height = 8, units = "cm")
    sprintf("Saving plot: %s", file.path(plots_dir, filename))
}

# type <- "pie"
# title <- "ALL HEADSTART SCHHOLS\nShare of pupils in each Development Stage at first assesssment in 2014/15"
# measure <- "c"
# xaxis <- ""
# xgroup <- ""
# colour_by <- "Dev_Stage"
# filter <- list("N_assessment_2+"=list("column"="N_assessments", "lower"=2, "upper"=10, "filter_type"="range"),
# "Assessment_n_1"=list("column"="Assessment_n_rev", "values"=list(-1), "filter_type"="in"))
# filename <- "pie_chart_test"
# long_filename <- "pie_chart_test"
# data_set_name <- "score_dt2"

pie_chart <- function(type, title, measure, xaxis, xgroup, colour_by, filter, filename, long_filename, data_set_name) {
    
    data_set <-  get(data_set_name)
    
    # Determine whether x-axis categories are to be grouped by a second variable
    grouped <- (xgroup %in% names(data_set))
    if(grouped) xlab <- xgroup else xlab <- ""
    
    # Filter data as per config for chart
    if(!("all" %in% unlist(names(filter)))) data_set <- filter_dt(data_set, filter)
    
    # Create model formular from config for aggregate function
    formula_str <- paste(measure, "~", paste(c(xlab, colour_by), collapse=" + "), sep=" ")
    dp <- aggregate(formula(formula_str), data_set, sum)
    dp[, measure] <- dp[, measure]/sum(dp[, measure])
    dp[, "y_pos"] <- 1-(cumsum(dp[, measure]) - dp[, measure]/2)

    # Make x-axis variables factors with levels limited to those still present in the data after filtering
    {if(grouped) dp[, xgroup] <- factor(dp[, xgroup], levels=levels_present(dp[, xgroup]))}
    # dp[, xaxis] <- factor(dp[, xaxis], levels=levels_present(dp[, xaxis]))
    
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
         
    ggsave(file.path(plots_dir, filename), plot = p, width = 10, height = 8, units = "cm")
    sprintf("Saving plot: %s", file.path(plots_dir, filename))
}

# All existing plats are deleted each time main.r is run.
delete_existing_plots <- function() {
    # Identify all plots in plot directory
    plot_filenames <- dir(path=plots_dir, pattern="*.png")
    if(length(plot_filenames)>0) {
        lapply(plot_filenames, function(x) file.remove(file.path(plots_dir, x)))
    }
    return()
}
