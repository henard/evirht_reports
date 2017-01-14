# Report Configuration file

# Global filter applied to all results
global_filters <- list(
    start_time="2015-09-01",
    end_time="2016-09-01"
)
# global_filters

# Plot 1: These are the bar charts in the pdf labelled ‘Sample 1’. For the Headstart set of schools these include a bar
#         chart for all schools and then each individual Headstart school. Some account holders may not include a large
#         number of schools. Henry will set up the script so that a user can input the level of detail required.
# Plot 2: This is the bar chart given in the pdf labelled ‘Sample 3’ and looks at individual pupil journeys by
#         development stage. The plots on individual progress are not required.
# Plot 3: This is the bar chart/scatter plot given in the pdf labelled ‘Sample 2’. Henry will consider this plot and
#         provide alternative formats if there are any improved ways of visualising the data.
# Plot 4: This is the TOL activity given in the spreadsheet called ‘Headstart Data – 11 05 16 for RHead.xlsx’).
#         (This plot should be prioritised over plot 3 if there are time constraints).

report1 = list(
    title="Headstart Schools - Academic Year 2014/15",
    chart1=list(type="barstacked",
                filter=NA,
                xaxis="schoolyear",
                yaxis="pptchange",
                colourby="devstage"),
    chart2=list(type="barstacked",
                filter=list(column="locality", values=list(1)),
                xaxis="schoolyear",
                yaxis="pptchange",
                colourby="devstage"),
    chart3=list(type="barstacked",
                filter=list(column="locality", values=list(6)),
                xaxis="schoolyear",
                yaxis="pptchange",
                colourby="devstage")
)
# report1["chart2"]
