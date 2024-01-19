#additional graphs
##############
#data
Median_income <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/Neighboring zips .xlsx",
                                sheet = "Median income (2)")

Herriman_college_enrollment <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/Neighboring zips .xlsx",
                                              sheet = "College enrollment age 18-24")

#educational attainment
Herriman_ed_attain <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/education attainment all zips.xlsx",
                                     sheet = "Herriman_84096")
SJord_education_attainment <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/education attainment all zips.xlsx",
                                            sheet = "Sjord_84095")
Kearns_education_attainment <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/education attainment all zips.xlsx",
                                             sheet = "Kearns_84118")
West_Valley_education_attainment <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/education attainment all zips.xlsx",
                                            sheet = "WV_84128")
West_Jordan_education_attainment <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/education attainment all zips.xlsx",
                                                 sheet = "Wjord_84081")
Bluffdale_education_attainment <- read_excel("/Volumes/dept$/Planning/Josh's Folder/RES-3697 Demographic Trends near Herriman/education attainment all zips.xlsx",
                                               sheet = "Bluff_84065")

#############
#graph median income
median_income.plot <- ggplot(Median_income, aes(x = Year)) +
    # Add lines for each city/zip code
    geom_line(aes(y = Sjord_84095, color = "South Jordan")) +
    geom_line(aes(y = Dayb_84009, color = "Daybreak")) +
    geom_line(aes(y = Kearns_84118, color = "Kearns")) +
    geom_line(aes(y = WV_84128, color = "West Valley")) +
    geom_line(aes(y = Wjord_84081, color = "West Jordan")) +
    geom_line(aes(y = Bluff_84065, color = "Bluffdale & Riverton")) +
    geom_line(aes(y = Herr_84096, color = "Herriman")) +
    labs(
        title = "Median Income Trends by City/Zip Code",
        x = "Year",
        y = "Median Income ($1000)",
        color = "City/Zip Code"
    ) +
    scale_color_manual(values = c(
        "South Jordan" = dsa.colors(1),
        "Daybreak" = dsa.colors(2),
        "Kearns" = dsa.colors(3),
        "West Valley" = dsa.colors(4),
        "West Jordan" = dsa.colors(5),
        "Bluffdale & Riverton" = dsa.colors(6),
        "Herriman" = dsa.colors(7)
    )) +
    scale_x_continuous(breaks = unique(Herriman_college_enrollment$Year)) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, NA))

# Display the plot
print(median_income.plot)
#cache("median_income.plot")
##############################
#college enrollment
Herriman.college.enrollment.plot <- ggplot(Herriman_college_enrollment, aes(x= Year)) +
    geom_line(aes(y = `Pop 18-24`, color = "Total population ages 18-24")) +
    geom_line(aes (y = `Enrolled in college or graduate school`, color = "College enrollment")) +
    geom_line(aes(y = percent / 100 * max(Herriman_college_enrollment$`Pop 18-24`), color = "Percentage"), linetype = "longdash") +
    labs(title = "Population 18-24 yr Enrolled in College (ACS 5-year)",
         x = "Year",
         y = "Count",
         color = "Legend") +
    scale_color_manual(values = c(
        "Total population ages 18-24" =dsa.colors(1),
        "College enrollment" = dsa.colors(2),
        "Percentage" = dsa.colors(7)),
        breaks = c("Total population ages 18-24", "College enrollment", "Percentage")
        ) +
    scale_x_continuous(breaks = unique(Herriman_college_enrollment$Year)) +
    ylim(0, 5000) +
    scale_y_continuous(
        sec.axis = sec_axis(~./max(Herriman_college_enrollment$`Pop 18-24`) * 100, name = "Percentage")
    ) +
    guides(color = guide_legend(
        override.aes = list(
            linetype = c("solid", "solid", "longdash"),  # Set linetype for each line
            color = c(dsa.colors(1), dsa.colors(2), dsa.colors(7))  # Set color for each line
        )
    )) +
    theme_minimal()
Herriman.college.enrollment.plot
#cache("Herriman.college.enrollment.plot")
############################
#education achievement

custom_order_ed_attain_color <- c("Population (18-24)" =dsa.colors(1),
                                  "Less than High School (18-24)" = dsa.colors(2),
                                  "High school graduate or equivalency (18-24)" = dsa.colors(3),
                                  "Some college or Associates (18-24)"= dsa.colors(4),
                                  "Bachelors (18-24)" = dsa.colors(5),
                                  "Population (25+)" = dsa.colors(6),
                                  "Less than High School (18-24)" = dsa.colors(7),
                                  "High school graduate or equivalency (25+)" = "#843F23",
                                  "Some college (25+)" = "black",
                                  "Associates (25+)" = "gray",
                                  "Bachelors or greater (25+)" = "#CA8B2A")

#######################
# Create a function to generate ggplot objects AGE
create_ed_attain_plot <-function(data, title) {
    ggplot(data, aes(x = Year)) +
        geom_line(aes(y = `pop_18_24`, color = "Population (18-24)")) +
        geom_line(aes(y = `less_than_hs_18_24`, color = "Less than High School (18-24)")) +
        geom_line(aes(y = `hs_18_24`, color = "High school graduate or equivalency (18-24)" )) +
        geom_line(aes(y = `some_or_assoc_18_24`, color = "Some college or Associates (18-24)")) +
        geom_line(aes(y = `bach_18_24`, color = "Bachelors (18-24)")) +
        geom_line(aes(y = `pop_25+`, color = "Population (25+)")) +
        geom_line(aes(y = `less_than_hs_25+`, color = "Less than High School (25+)")) +
        geom_line(aes(y = `hs_25+`, color = "High school graduate or equivalency (25+)" )) +
        geom_line(aes(y = `some_25+`, color = "Some college (25+)")) +
        geom_line(aes(y = `assoc_25+`, color = "Associates (25+)")) +
        geom_line(aes(y = `bach+_25+`, color = "Bachelors or greater (25+)")) +
        labs(title = title,
             x = "Year",
             y = "Population Count",
             color = "Legend") +
        scale_color_manual(values = custom_order_ed_attain_color,
                           breaks = names(custom_order_ed_attain_color)
        ) +
        scale_x_continuous(breaks = unique(Herriman_college_enrollment$Year)) +
        theme_minimal()
}


#Create individual plots
Ed_attain_WJordan_plot <-create_ed_attain_plot(West_Jordan_education_attainment,"Population by Educational Attainment - West Jordan")
Ed_attain_WV_plot <-create_ed_attain_plot(West_Valley_education_attainment,"Population by Educational Attainment - West Valley")
Ed_attain_SJordan_plot <-create_ed_attain_plot(SJord_education_attainment,"Population by Educational Attainment - South Jordan")
Ed_attain_Bluff_plot <-create_ed_attain_plot(Bluffdale_education_attainment,"Population by Educational Attainment - Bluffdale & Riverton")
Ed_attain_Kerns_plot <-create_ed_attain_plot(Kearns_education_attainment,"Population by Educational Attainment - Kearns")
Ed_attain_Herriman_plot <-create_ed_attain_plot(Herriman_ed_attain,"Population by Educational Attainment - Herriman")

# Arrange the plots in a single plot
combined_plot_ed_attain <- plot_grid(Ed_attain_WV_plot, Ed_attain_Kerns_plot, Ed_attain_WJordan_plot,
                                     Ed_attain_SJordan_plot, Ed_attain_Herriman_plot, Ed_attain_Bluff_plot,
                               ncol = 2)
print(combined_plot_ed_attain)
#cache("combined_plot_ed_attain")

Ed_attain_Herriman_plot
#cache("Ed_attain_Herriman_plot")



