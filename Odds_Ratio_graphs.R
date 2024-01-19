# odds ratio outputs
retention.cluster.OR
grad.cluster.OR

################ RETENTION ODDS RATIO GRAPH ################3
label <- c("PACE vs. Non-PACE", "Ever Pell Eligible vs. Never Pell Eligible",
           "Not First Gen vs. First Gen Status", "First Gen Unknown vs. First Gen Status", #"Age",
           "Race: Other vs. White", "Race: Prefer Not to Say vs. White",
           "Ethnicity: Non-Hispanic vs. Hispanic", "Male vs. Female",
           "1-3 Prior Credits vs. 0", "4-6 Prior Credits vs. 0", "7-9 Prior Credits vs. 0",
           "More than 10 Prior Credits vs. 0",
           "Per 1.0 Unit Increase in High School GPA", "Missing vs. Not Missing HS GPA")


retention.OR.df <- as.data.frame(retention.cluster.OR) %>%
    rownames_to_column("Variable") %>%
    filter(Variable != "(Intercept)") %>%
    rename(LL = `2.5 %`, UL = `97.5 %`, OR = V1) %>%
    cbind(label)

#color significant
#if UL is less than one then red
#if LL is greater than one blue else gray
for(i in 1:nrow(retention.OR.df)) {
    if(retention.OR.df$UL[i] <= 1) {
        retention.OR.df$color[i] <- "Less Likely to Return"
    } else if(retention.OR.df$LL[i] >= 1 ) {
        retention.OR.df$color[i] <- "More Likely to Return"
    } else {retention.OR.df$color[i] <- "Not Significantly Different"
    }
}


plot1 <- ggplot(retention.OR.df, aes(y = label, x=OR, color=color)) +
    geom_point(shape = 18, size = 7) +  #, color=dsa.colors(1)) +
    geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) + #, color=dsa.colors(1)) +
    geom_vline(xintercept = 1, color = "black", linetype = "dashed", cex = 1, alpha = 0.5) +
    #scale_y_continuous(name = "", breaks=1:17, labels = OR_df$label, trans = "reverse") +
    scale_y_discrete(limits = rev(retention.OR.df$label), labels = rev(retention.OR.df$label)) +
    scale_x_log10() +
    xlab("Odds Ratio (95% CI)") +
    ylab(" ") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 14, colour = "black"),
          axis.text.x.bottom = element_text(size = 14, colour = "black"),
          axis.title.x = element_text(size = 14, colour = "black"))
plot1
retention.ORplot <- plot1 +
    ggtitle("Odd Ratio for Retention Outcome") +
    labs(color="Significant Association") +
    theme(
        #legend.position = c(0.35, 1), #this puts it on the top left/middle
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        plot.title = element_text(size=16)
    ) +
    scale_color_manual(values=c(dsa.colors(7), dsa.colors(1), dsa.colors(6)))
retention.ORplot
 cache("retention.ORplot")



####################GRADUATION GRAPH##############
# this dataframe is putting the first column as the row names - need to move to first column
# and the first row is just the column names (drop column names)
grad.OR.df <- as.data.frame(grad.cluster.OR) %>%
    rownames_to_column("Variable") %>%
    filter(Variable != "(Intercept)") %>%
    rename(LL = `2.5 %`, UL = `97.5 %`, OR = V1) %>%
    cbind(label)


#color significant
#if UL is less than one then red
#if LL is greater than one blue else gray
for(i in 1:nrow(grad.OR.df)) {
    if(grad.OR.df$UL[i] <= 1) {
        grad.OR.df$color[i] <- "Less Likely to Graduate"
    } else if(grad.OR.df$LL[i] >= 1 ) {
        grad.OR.df$color[i] <- "More Likely to Graduate"
    } else {grad.OR.df$color[i] <- "Not Significantly Different"
    }
}


plot2 <- ggplot(grad.OR.df, aes(y = label, x=OR, color=color)) +
    geom_point(shape = 18, size = 7) +  #, color=dsa.colors(1)) +
    geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) + #, color=dsa.colors(1)) +
    geom_vline(xintercept = 1, color = "black", linetype = "dashed", cex = 1, alpha = 0.5) +
    #scale_y_continuous(name = "", breaks=1:17, labels = OR_df$label, trans = "reverse") +
    scale_y_discrete(limits = rev(grad.OR.df$label), labels = rev(grad.OR.df$label)) +
    scale_x_log10() +
    xlab("Odds Ratio (95% CI)") +
    ylab(" ") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 14, colour = "black"),
          axis.text.x.bottom = element_text(size = 14, colour = "black"),
          axis.title.x = element_text(size = 14, colour = "black"))
plot2
grad.ORplot <- plot2 +
    ggtitle("Odd Ratio for Graduation Outcome") +
    labs(color="Significant Association") +
    theme(
        #legend.position = c(0.35, 1), #this puts it on the top left/middle
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        plot.title = element_text(size=16)
    ) +
    scale_color_manual(values=c(dsa.colors(7), dsa.colors(1), dsa.colors(6)))
grad.ORplot
# cache("grad.ORplot")

retention.ORplot
grad.ORplot




