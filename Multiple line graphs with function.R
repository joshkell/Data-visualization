#Line graphs of educational attainment in multiple cities
#data was pulled from ACS survey - included counts of each category for 10 years 
##############
#import data


############################
#education achievement
#color pallet - at SLCC we have a custom package that has SLCC colors and the function is dsa.colors(1-8)
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
# Create a function to generate ggplot objects educational attainment
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




