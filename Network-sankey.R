# Load necessary libraries for data manipulation and visualization
library(reshape2)
library(ggalluvial)

# Create a unique identifier column by concatenating the values in each row
ceRNA$unique_id <- apply(ceRNA, 1, function(x) paste(x, collapse = ","))

# Group the data by the different types of features (mRNA, miRNA, lncRNA, circRNA)
# Summarize to count the occurrences and create a data frame
ceRNA <- cedata %>%
  group_by(mRNA, miRNA, lncRNA, circRNA) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  data.frame()

# Pre-define colors for the plot; 36 unique colors are needed for the 36 features
# color <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', 
#            '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', 
#            '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', 
#            '#F781BF', '#66C2A5', '#6181BD', '#F34800', '#64A10E', '#FF00FF', 
#            '#c7475b', '#049a0b', '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', 
#            '#4253ff', '#ff4308', '#D8D155', '#F0027F', '#9FAED4', '#F7CDBD')

# Load the randomcoloR library to generate a distinct set of colors
# Generate a palette of distinct colors; the count should match the number of categories
palette <- randomColor(count = 254)  
palette <- distinctColorPalette(41)

# Prepare the data for ggplot2 by adding a 'link' column and melting the data frame
ceRNA$link <- 1
ceRNA <- melt(ceRNA, id = 'link')

# Summarize the 'variable' column to get the count of each variable
variable <- summary(ceRNA$variable)

# Create a 'flow' column that repeats the sequence of counts for each variable
ceRNA$flow <- rep(1:variable[1], length(variable))

# View the structure of the prepared data
head(ceRNA)

# Initialize a PDF file for the sankey plot with specified dimensions
pdf("./sankeyplot.pdf", height = 20, width = 15)

# Create the sankey plot using ggplot2, with specified aesthetics and geoms
ggplot(ceRNA, aes(x = variable, y = link, stratum = value, alluvium = flow, fill = value)) +
  geom_stratum() +  # Add stacked bars to the plot
  geom_flow(aes.flow = 'forward') +  # Add connecting lines between the bars
  scale_fill_manual(values = palette) +  # Assign colors to the plot
  geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  # Add labels for each category
  scale_x_discrete(limits = c('target_symbol', 'mature_mirna_id', 'lncRNA', "circRNA")) +  # Set the order of categories
  theme(legend.position = 'none', panel.background = element_blank(), line = element_blank(), axis.text.y = element_blank()) +
  labs(x = '', y = '')

# Close the PDF file device to output the plot
dev.off()

# Alternative method for creating the sankey plot using a different approach

# Adjust the 'link' values based on the frequency of each category
link <- 1 / table(ceRNA$value)
for (i in names(link)) ceRNA[which(ceRNA$value == i), 'link'] <- link[i]

# Create the sankey plot with an alternative method, adjusting the plot dimensions and other parameters
ceRNA$link <- 1
ceRNA <- reshape::melt(ceRNA, id = 'link')

variable <- summary(ceRNA$variable)
ceRNA$flow <- rep(1:variable[1], length(variable))

link <- 1 / table(ceRNA$value)
for (i in names(link)) ceRNA[which(ceRNA$value == i),'link'] <- link[i]

#Create the sankey plot using ggplot2, with specified aesthetics and geoms
pdf("./sankeyplot.pdf",height = 10,width = 8)
ggplot(ceRNA, aes(x = variable, y = link, stratum = value, alluvium = flow, fill = value)) +
  geom_stratum() +  #冲击图中的堆叠柱形图
  geom_flow(aes.flow = 'forward') +  #冲击图连线绘制
  scale_fill_manual(values = palette) +  #颜色赋值
  geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  #添加 lncRNA、miRNA 和 mRNA 标签
  scale_x_discrete(limits = c('target_symbol', 'mature_mirna_id', 'lncRNA',"circRNA")) +  #定义 lncRNA、miRNA 和 mRNA 列的展示顺序
  theme(legend.position = 'none', panel.background = element_blank(), line = element_blank(), axis.text.y = element_blank()) +
  labs(x = '', y = '')
dev.off()

# Load the tidyverse suite of packages for data manipulation and visualization
library(tidyverse)
library(ggsankey)
library(ggplot2)
library(cols4all)

# Convert the data to lodes form using to_lodes_form function from ggalluvial
convertion <- to_lodes_form(
  cedata,
  axes = 1:4,
  key = "x",
  value = "stratum",
  id = "alluvium"
)

# Define a color palette for the plot
color <- c("#9079ad","#7ebea5","#d8a373","#f09199","#8d6449","#4c6cb3","#fef263","#ce5242")

# Initialize a PDF file for the sankey plot with specified dimensions
pdf("./sankeyplot.pdf", height = 8, width = 8)

# Create the sankey plot using ggplot2, with specified aesthetics and geoms
ggplot(convertion,
       aes(x = x, stratum = stratum, alluvium = alluvium, fill = stratum, label = stratum)) +
  scale_fill_manual(values = color) +
  geom_flow(width = 0.4, aes.flow = "forward") + # Set flow direction and line color
  geom_stratum(alpha = 0.8, width = 0.4) +
  geom_text(stat = "stratum", size = 2, color = "black", family = "serif") +
  theme_classic() +
  # Customize the plot theme to remove axis lines, ticks, and gridlines
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank()) +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  theme(axis.text.x = element_text(size = 8, family = "serif", color = "black")) +
  xlab("") + ylab("") + ggtitle("") + guides(fill = "none")

# Close the PDF file device to output the plot
dev.off()

# Group the data by source, target, and group, summarize counts, and create a data frame
ce_sub <- ce_sub %>%
  group_by(source, target, group) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  data.frame()

# Check if the data is in alluvial form for ggalluvial plotting
is_alluvia_form(as.data.frame(ce_sub), axes = 1:3, silent = TRUE)

# Initialize a PDF file for another ceRNA sankey plot with specified dimensions
pdf("./ceRNA-sankey.1.pdf", width = 10, height = 10)

# Create the alluvial (sankey) plot using ggplot2
ggplot(as.data.frame(ce_sub),
       aes(y = count, axis1 = source, axis2 = target)) +
  geom_alluvium(aes(fill = group), width = 1/12) +
  geom_stratum(width = 1/12, fill = "#9D8CB8", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("source", "target"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Accent") +
  theme_bw() +
  ggtitle("ceRNA Subnetwork")

# Close the PDF file device to output the plot
dev.off()

# Transform the data for plotting with the Sankeywheel package
vaccinations <- transform(cedata,
                          source = factor(source, levels = source)) # Reorder factor levels

# Read data from the clipboard into a variable 'subgene'
subgene = readClipboard()
# Subset cedata to include only rows where 'source' and 'target' are in 'subgene' and 'count' is greater or equal to 2
ce_sub <- cedata[cedata$source %in% subgene & cedata$target %in% subgene,]
ce_sub <- cedata[cedata$count >= 2,]

# Install and load the Sankeywheel and highcharter packages for interactive sankey diagrams
install.packages("sankeywheel")
install.packages('highcharter')
library(highcharter)
library(sankeywheel)

# Create a highchart interactive sankey diagram with the ce_sub data
highchart() %>%
  hc_add_series(data = ce_sub,
                type = "sankey",
                hcaes(from = source, to = target, weight = count)) %>%
  hc_add_theme(hc_theme_sparkline())

# Create a highchart interactive dependency wheel diagram with the ce_sub data
highchart() %>%
  hc_add_series(data = ce_sub,
                type = "dependencywheel",
                hcaes(from = source, to = target, weight = count)) %>%
  hc_add_theme(hc_theme_sparkline())
