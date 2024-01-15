# script to plot the significance in % of each predictor in the SHAP-XGB analysis

library(ggplot2)

# Create a dataframe with the data (see paper)
data <- data.frame(
  Ecosystem = rep(c('Forests', 'Savannahs', 'Croplands', 'Grasslands'), each = 2),
  Area = rep(c('USA', 'World'), times = 4),
  Value = c(0.89, 1.05, # forests
            0.25, 0.60, # savannas
            0.15, 0.15,  # croplands
            0.37, 0.41) # grasslands
  # Fill = rep(c('#1671AF', '#1C8C46'), times = 4)
)

# Reorder the PFTs for plotting
data$Ecosystem <- factor(data$Ecosystem, levels = c('Forests', 'Savannahs', 'Grasslands', 'Croplands'))
data$Area <- factor(data$Area, levels = c('World', 'USA'))

# Plot
p <- ggplot(data, aes(x = Ecosystem, y = Value, fill = Area)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c('#1671AF', '#6eb8ea')) +
  labs(y = 'Relative importance of groundwater vs aridity (%)') +
  theme_minimal(base_size = 16) + #
  theme(
    # panel.border = element_rect(colour = "black", fill=NA, size=1), # Add a box around the plot
    # panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    # axis.line = element_line(colour = "black"), # Add axes lines
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 20, color = "black"),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_blank(),

    # legend
    # legend.background = element_rect(color = "black", fill = "white"), # Add background to the legend with a border
    legend.position = c(0.85, 0.93), # Position the legend inside the plot, adjust as needed (c(x, y) where 0 < x, y < 1)
    legend.justification = c("right", "top"), # Anchor point for the legend position
    legend.box.just = "right", # Justify the legend box inside the plotting area
    legend.box.margin = margin(6, 6, 6, 6) # Margin around the legend box
  ) +
  guides(fill = guide_legend(reverse = TRUE)) # Reverse legend order to match the plot order

# Print the plot
print(p)

# save final plot
ggsave("significance_SHAP_plot.png", p, width = 10, height = 8, dpi = 600)

