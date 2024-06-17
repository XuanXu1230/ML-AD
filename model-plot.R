####Load the ggrepel library for text annotations in plots####
library(ggrepel)

# Create a plot using autoplot to visualize model fits, focusing on the 'rsq' metric
# Add text labels with non-overlapping placement using geom_text_repel
autoplot(wf_fits, metric = "rsq") +
  geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none") # Remove the legend from the plot

# Collect individual R-squared estimates, summarizing is set to FALSE to get individual metrics
rsq_indiv_estimates <-
  collect_metrics(wf_fits, summarize = FALSE) %>%
  filter(.metric == "rsq")

# Reshape the data frame to have one column per workflow ID (wflow_id)
rsq_wider <-
  rsq_indiv_estimates %>%
  dplyr::select(wflow_id, .estimate, id) %>%
  pivot_wider(id_cols = "id", names_from = "wflow_id", values_from = ".estimate")

# Calculate the correlation matrix for the R-squared values across models
# The 'quiet = TRUE' argument suppresses non-essential output
corrr::correlate(rsq_wider %>% dplyr::select(-id), quiet = TRUE)

# Create a line plot to visualize the R-squared estimates for each model
# Reorder wflow_id based on .estimate for better visualization
rsq_indiv_estimates %>%
  mutate(wflow_id = reorder(wflow_id, .estimate)) %>%
  ggplot(aes(x = wflow_id, y = .estimate, group = id, color = id)) +
  geom_line(alpha = .5, linewidth = 1.25) + # Add semi-transparent lines with increased width
  theme(legend.position = "none") # Remove the legend from the plot

#### boruta.imp is designed to process and summarize the importance of variables as determined by the Boruta algorithm ####
library(Boruta)
#设置随机种子
set.seed(123)
boruta_output <- Boruta(Group ~ ., data=na.omit(merged_df), pValue = 0.01, mcAdj = TRUE, doTrace = 2)
summary(boruta_output)
importance <- boruta_output$ImpHistory 

boruta.imp <- function(x) {
  # Melt the importance history matrix to a long format and remove NA values
  imp <- reshape2::melt(x$ImpHistory, na.rm = T)[, -1]
  # Rename the columns to "Variable" and "Importance"
  colnames(imp) <- c("Variable", "Importance")
  # Remove rows with non-finite importance values
  imp <- imp[is.finite(imp$Importance),]
  
  # Create a data frame with final decision variables from the Boruta algorithm
  variableGrp <- data.frame(Variable = names(x$finalDecision), 
                            finalDecision = x$finalDecision)
  
  # Create a data frame for shadow variables that need to be displayed
  showGrp <- data.frame(Variable = c("shadowMax", "shadowMean", "shadowMin"),
                        finalDecision = c("shadowMax", "shadowMean", "shadowMin"))
  
  # Append the shadow variables to the variable group data frame
  variableGrp <- rbind(variableGrp, showGrp)
  
  # Merge the importance data with the final decision and shadow variables
  boruta.variable.imp <- merge(imp, variableGrp, all.x = T)
  
  # Calculate median importance for each variable and sort by it
  sortedVariable <- boruta.variable.imp %>% group_by(Variable) %>%
    summarise(median = median(Importance)) %>% arrange(median)
  # Extract the sorted variable names as a vector
  sortedVariable <- as.vector(sortedVariable$Variable)
  
  # Reorder the factors in the 'Variable' column according to the sorted vector
  boruta.variable.imp$Variable <- factor(boruta.variable.imp$Variable, levels = sortedVariable)
  
  # Return the processed importance data invisibly
  invisible(boruta.variable.imp)
}

####Using the UpSetR package to create an UpSet plot####
library(UpSetR)
library(VennDiagram)
library(RColorBrewer)
library(ggvenn)

# Create an UpSet plot with a list of sets, specifying that there are 6 sets in total
upset(fromList(venn_list), nsets = 6, order.by = "freq")

# Create a more customized UpSet plot with various parameters
upset(fromList(venn_list),     
      nsets = length(data), # Show all data from the datasets; adjust the number of visualized datasets with nsets
      nintersects = 30, # Show information for the top 30 intersections
      keep.order = TRUE, # Keep the order of sets as they appear in the input; use keep.order = TRUE to maintain the order
      number.angles = 0, # Set the angle of the numbers on the intersection bars to 0 degrees (horizontal)
      point.size = 4, # Set the size of the points in the plot
      line.size = 1, # Set the thickness of the lines in the plot
      mainbar.y.label = "Intersection size", # Set the label for the y-axis of the main bar plot
      main.bar.color = 'black', # Set the color of the main bar plot to black
      matrix.color = "black", # Set the color of the points in the matrix plot to black
      sets.x.label = "Set size", # Set the label for the x-axis of the sets bar plot
      sets.bar.color = brewer.pal(6, "Set1"), # Set the colors of the sets bar plot using a color palette; "Set1" has 9 colors
      mb.ratio = c(0.7, 0.3), # Set the ratio of the height of the bar plot and the matrix plot
      order.by = "freq", # Set the order of the y-axis matrix by frequency or degree
      text.scale = c(1.5, 1.5, 1.5, 1.5, 1.5, 1), # Set the scaling for various text elements in the plot
      shade.color = "red" # Set the color of the shaded areas in the plot
)

inter <- get.venn.partitions(venn_list)
for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
write.table(inter , '.venn.txt', row.names = FALSE, sep = '\t', quote = FALSE)

for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
openxlsx::write.xlsx(inter, 'venn.xlsx', row.names = FALSE, sep = ',', quote = FALSE)



####This script is focusing on creating informative and visually appealing plots for model performance comparison across different 'omics' types.
# Create a data frame 'data1' with RMSE values for different models grouped by 'omics' type
data1 = data.frame(
  RMSE = round(c(methy[, "rmse_mean"], mrna[, "rmse_mean"], mirna[, "rmse_mean"], 
                 omic3[, "rmse_mean"], omic3_prs[, "rmse_mean"]), 3),
  Group = c(rep('methy', 10), rep('mrna', 10), rep('mirna', 10), 
            rep('omic3', 10), rep('omic3_prs', 10)),
  Models = c(rownames(methy), rownames(mrna), rownames(mirna), 
             rownames(omic3), rownames(omic3_prs))
)

# Create a data frame 'data2' with RSQ values for different models grouped by 'omics' type
data2 = data.frame(
  RSQ = round(c(methy[, "rsq_mean"], mrna[, "rsq_mean"], mirna[, "rsq_mean"], 
                omic3[, "rsq_mean"], omic3_prs[, "rsq_mean"]), 3),
  Group = c(rep('methy', 10), rep('mrna', 10), rep('mirna', 10), 
            rep('omic3', 10), rep('omic3_prs', 10)),
  Models = c(rownames(methy), rownames(mrna), rownames(mirna), 
             rownames(omic3), rownames(omic3_prs))
)

# Load the patchwork library for combining plots
library(patchwork)

# Create a bar plot 'p1' for RMSE values
p1 <- ggplot(data1, aes(x = Models, y = RMSE, fill = Group)) +
  geom_bar(stat = 'identity', position = 'fill') +
  geom_text(aes(label = RMSE), size = 5, position = position_fill(vjust = 0.5)) +
  labs(x = NULL, y = 'RMSE') +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(colour = 'black')) +
  # Color setting friendly for red-green color blindness
  scale_fill_manual(values = rep(c('#d9d2e9', '#d9ead3', '#c2a8b5', '#96bdc4', '#f2e8cf', '#CC79A7'), 10))

# Create a bar plot 'p2' for ACC (RSQ) values
p2 <- ggplot(data2, aes(x = Models, y = RSQ, fill = Group)) +
  geom_bar(stat = 'identity', position = 'fill') +
  geom_text(aes(label = RSQ), size = 5, position = position_fill(vjust = 0.5)) +
  labs(x = NULL, y = 'ACC') +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(colour = 'black')) +
  # Color setting friendly for red-green color blindness
  scale_fill_manual(values = rep(c('#feb7d9', '#ffdeb5', '#f0f3a4', '#96d4e9', '#9fd0cc', '#CC79A7'), 10))

# Combine the plots p1 and p2 using patchwork
p1 + p2

# Alternative creation of bar plots 'p1' and 'p2' with dodged positions for side-by-side bars
# ...

# Load the cowplot library for advanced plot combination and saving
library(cowplot)

# Combine the plots p1 and p2 into a grid with labels and alignment
p3 <- plot_grid(p1, p2, labels = c("A", "B"), nrow = 2, align = "v")

# Save the combined plot as a PDF file with a specific naming convention
ggsave("modelsbar-.tune.pdf", p3, width = 13, height = 9)