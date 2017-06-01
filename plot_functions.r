library(ggplot2)
library(scales)
library(grid)

bar_chart <- function(type, measure, xaxis, xgroup, colour_by, filter, filename, long_filename, dataset, auto_title, title_org) {
    
    plot_size_scale = 1.5
    
    # Determine if plotting child_ids - handled differently
    plotting_child_ids <- any(grepl("Child_ID", c(xaxis, xgroup)))
    plotting_org_ids <- any(grepl("Organisation", c(xaxis, xgroup)))
    plotting_ids <- plotting_child_ids | plotting_org_ids
    plotting_ids_on_xaxis <- grepl("Child_ID", xaxis) | grepl("Organisation", xaxis)
    
    # Determine whether to plot sample_sizes
    plotting_sample_sizes <- !plotting_child_ids & measure=="score_change"

    # Determine units of measure
    measure_units <- "Pupils"
    if(measure=="Overall_Score") measure_units <- "Profiles"
    
    # Determine stacked or side-by-side bar chart
    if(grepl("side", type)) postn <- "dodge" else postn <- "stack"

    data_set <- get(dataset)

    # Determine whether x-axis categories are to be grouped by a second variable
    grouped <- (xgroup %in% names(data_set))
    if(grouped) xlab <- c(xaxis, xgroup) else xlab <- xaxis
    
    # Create model formular from config for aggregate function
    formula_str <- paste(measure, "~", paste(c(colour_by, xlab), collapse=" + "), sep=" ")

    # Create an 'empty' plot dataframe for when sample size <10 due to filtering applied
    dp_empty <- aggregate(formula(formula_str), data_set, FUN = function(x) c(score_change = mean(x), n_pupils = length(x)))
    dp_empty <- do.call(data.frame, dp_empty)
    names(dp_empty) <- sub(".*\\.", "", names(dp_empty))
    names(dp_empty)[names(dp_empty) == 'mean'] <- measure
    dp_empty[measure] <- 0.00001

    # Filter data as per config for chart
    if(!("all" %in% unlist(names(filter)))) data_set <- filter_dt(data_set, filter)
    if(grouped) levs_present <- levels_present(data_set[, get(xgroup)])

    # Create an indicator of when the sample size is too small, if so use empty plot dataframe
    min_sample_size <- 1
    sample_size_too_small <- nrow(data_set)<min_sample_size
    if(sample_size_too_small) {
        dp <- dp_empty
    } else {
        # Aggregate filtered data to create data for plotting
        dp <- aggregate(formula(formula_str), data_set, FUN = function(x) c(mean = mean(x), n_pupils = length(x)))
        dp <- do.call(data.frame, dp)
        names(dp) <- sub(".*\\.", "", names(dp))
        names(dp)[names(dp) == 'mean'] <- measure
    }

    # Capture sample size measurements then remove sample size column (n_pupils) from dataframe
    sample_sizes <- list("sum"=sum(dp$n_pupils), "min"=min(dp$n_pupils), "max"=max(dp$n_pupils))
    sample_size_text <- paste(measure_units,": ", sample_sizes$sum, sep="")

    # Reverse the order of Dev_Stage when display side-by-side charts
    if(postn == "dodge" & colour_by == "Dev_Stage") {
        dp[, colour_by] <- factor(dp[, colour_by], levels=rev(levels(dp[, colour_by])))
    }

    # Make colour_by variable a factor with levels limited to those still present
    if(measure=="pct") dp[, colour_by] <- factor(dp[, colour_by])

    # Assign each bar/ category in plot data a chunk number 'chunk' depending upon chunk size
    # For chunk_size = 100, chunck 1 is 1-100, chunk 2 is 101-200 and so on.
    chunk_size <- 100
    # if(postn=="stack") {
    if(!grouped) {
        dp_chunk <- dp[!duplicated(dp[, xlab, drop=F]), xlab, drop=F]
    } else {
        dp_chunk <- dp[, xlab, drop=F]
    }

    number_of_categories <- nrow(dp_chunk)
    dp_chunk$chunk <- floor((1:number_of_categories)/chunk_size)+1
    dp <- merge(dp, dp_chunk, all.x=TRUE)
    too_many_categories <- number_of_categories>chunk_size
    
    # Restrict plotting to chunk 1.
    # Future task may be to extend this to handle chunks 2 onwards
    dp <- dp[dp$chunk==1, ]

    # Make xgroup variable a factor with levels limited to those still present in the data after filtering
    if(grouped) dp[, xgroup] <- factor(dp[, xgroup], levels=levs_present)
    
    # Plot setings
    default_font_size = 10

    # Ensure the width of bars is consistent when plot type="side_by_side"
    if(!grouped) dp <- expand_dataframe(dp, measure)

    # Configure x-axis label settings for different  x-axis variables 
    if(plotting_child_ids) {
        xaxis_text_angle = -90
        x_vjust = 0.5
        x_hjust = 0
        plot_height = 10
        x_font_size=default_font_size-floor(((number_of_categories-1)/20))
        # dp[, xaxis] <- as.character(dp[, xaxis])
    } else if(plotting_org_ids) {
        xaxis_text_angle = -45
        x_vjust = 1
        x_hjust = 0
        plot_height = 12
        x_font_size=default_font_size-floor(((number_of_categories-1)/20))
    } else {
        # Ensure the width of bars is consistent when plot type="side_by_side"
        dp <- expand_dataframe(dp, measure)
        xaxis_text_angle = 0
        x_vjust = 0.5
        x_hjust = 0.5
        plot_height = 8
        x_font_size=default_font_size-floor(((number_of_categories-1)/20))
    }

    # Aggregating converts character variables into factors - revert this for id variables
    if(plotting_ids_on_xaxis) dp[, xaxis] <- as.character(dp[, xaxis])

    # If not all categories are being included say so in the x-axis lable
    xlab_text <- get_column_labels(chart_col_labels, xlab)
    if(too_many_categories & !sample_size_too_small) xlab_text <- paste(xlab_text, " (showing ", chunk_size, " out of ", number_of_categories, " categories.)")
    x_mid <- 0.5*length(unique(dp[, xaxis]))
    
    # Set colour palette according to colour_by variable
    if(colour_by=="pupil_count_type") {
        colour_palette <- get_colour_palette(style_guide, "pupil_counts_colours")
    } else {
        colour_palette <- get_colour_palette(style_guide, "devstrand_colours")
    }

    # Configure grob for plotting sample size text in top right hand corner.
    # (This method enables you to specify the relative position of the text
    # - no need to specify the coordinates which change from one chart to the next)
    grob=grobTree(textGrob(sample_size_text, x=1, y=1, hjust=1, vjust=1, gp=gpar(col="black", fontsize=10)))

    # Plot and save
    p = ggplot(data=dp, aes_string(x=xaxis, y=measure, fill=colour_by)) +
        geom_bar(stat = "identity", position=postn, colour="black", size=0.2) +
        {if(plotting_sample_sizes) geom_text(aes(label=n_pupils, vjust=ifelse(score_change >= 0, -0.25, 1.25)), position=position_dodge(width=0.9), size=0.35*x_font_size)} +
        {if(grouped & !plotting_ids) facet_grid(reformulate(xgroup), switch = "x", space = "free_x")} +
        {if(grouped & plotting_ids) facet_grid(reformulate(xgroup), switch = "x", scales="free_x", space = "free_x")} +
        {if(grouped) theme(panel.spacing = unit(0, "lines"), strip.background = element_blank(), strip.placement = "outside")} +
        xlab(xlab_text) +
        ylab(get_column_labels(chart_col_labels, measure)) +
        ggtitle(auto_title) +
        theme(plot.title = element_text(lineheight=.8, face="bold", size=default_font_size)) +
        theme(panel.grid.major.y = element_line(colour = "grey", linetype = "solid", size=0.3), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(colour = "grey", linetype = "dotted", size=0.3)) + 
        scale_fill_manual(values=unlist(colour_palette)) + 
        scale_colour_discrete(drop = FALSE) +
        scale_x_discrete(drop = FALSE) +
        {if(sample_size_too_small) coord_cartesian(ylim = c(0, 1))} +
        {if(sample_size_too_small) annotate("text", x=x_mid, y=0.5, label= "No data in specified chart filter")} + 
        {if(colour_by!="pupil_count_type" & !grouped) annotation_custom(grob)} +
        {if(measure != "N") scale_y_continuous(labels=percent)} +
        theme(panel.background = element_rect(fill = "white")) +
        theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
        guides(fill = guide_legend(title = get_column_labels(chart_col_labels, colour_by), title.position = "top")) +
        theme(axis.title = element_text(size=default_font_size)) +
        theme(axis.text.x = element_text(angle = xaxis_text_angle, vjust=x_vjust, hjust=x_hjust, size=x_font_size)) +
        theme(axis.text.y = element_text(angle = 0, vjust=0.5, hjust=0.5, size=default_font_size)) +
        theme(legend.title = element_text(colour="black", size=default_font_size, face="bold"), legend.position = "right", legend.text = element_text(colour="black", size=default_font_size))

    ggsave(file.path(plots_dir, filename), plot = p, width = plot_size_scale*20, height = plot_size_scale*plot_height, units = "cm")
    sprintf("Saving plot: %s", file.path(plots_dir, filename))
}

pie_chart <- function(type, measure, xaxis, xgroup, colour_by, filter, filename, long_filename, dataset, auto_title, title_org) {
    
    data_set <- get(dataset)
    
    # Determine whether x-axis categories are to be grouped by a second variable
    grouped <- (xgroup %in% names(data_set))
    if(grouped) xlab <- xgroup else xlab <- ""
    
    # Create model formular from config for aggregate function
    formula_str <- paste(measure, "~", paste(c(colour_by, xlab), collapse=" + "), sep=" ")
    formula_str <- paste(measure, "~", colour_by, sep=" ")
    
    # Create an 'empty' plot dataframe for when sample size <10 due to filtering applied
    dp_empty <- aggregate(formula(formula_str), data_set, sum)
    dp_empty[, measure] <- dp_empty[, measure]/sum(dp_empty[, measure])
    dp_empty[, "y_pos"] <- 1-(cumsum(dp_empty[, measure]) - dp_empty[, measure]/2)
    dp_empty <- add_xposoffset(dp_empty, measure)
    dp_empty[measure] <- 0
    
    # Filter data as per config for chart
    if(!("all" %in% unlist(names(filter)))) data_set <- filter_dt(data_set, filter)
    
    # Create an indicator of when the sample size is too small, if so use empty plot dataframe
    min_sample_size <- 1
    sample_size_too_small <- nrow(data_set)<min_sample_size
    if(sample_size_too_small) {
        dp <- dp_empty
    } else {
        # Aggregate filtered data to create data for plotting
        dp <- aggregate(formula(formula_str), data_set, sum)
        dp[, measure] <- dp[, measure]/sum(dp[, measure])
        dp[, "y_pos"] <- 1-(cumsum(dp[, measure]) - dp[, measure]/2)
        dp <- add_xposoffset(dp, measure)
    }

    # Make x-axis variables factors with levels limited to those still present in the data after filtering
    {if(grouped) dp[, xgroup] <- factor(dp[, xgroup], levels=levels_present(dp[, xgroup]))}
    # dp[, xaxis] <- factor(dp[, xaxis], levels=levels_present(dp[, xaxis]))
    
    # Plot setings
    default_font_size = 10
    if(measure=="pct") {
        colour_palette <- get_colour_palette(style_guide, "pupil_pct_colours")
    } else if(measure=="N") {
        colour_palette <- get_colour_palette(style_guide, "pupil_counts_colours")
    } else {
        colour_palette <- get_colour_palette(style_guide, "devstrand_colours")
    }
    
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
        ggtitle(auto_title)  +
        {if(!sample_size_too_small) geom_text(aes(x = 1.2-dp[, "xlaboffset"]*0.25, y = dp[, "y_pos"], label = percent(dp[, measure])), size=3)} +
        {if(sample_size_too_small) annotate("text", x=0, y=0, label= "No data in specified chart filter")} + 
        theme(axis.text.x=element_blank()) +
        scale_fill_manual(values=unlist(colour_palette)) + 
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
