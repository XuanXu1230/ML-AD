library('survival')
library('survminer')

####create survival plots for a list of genes####
# Initialize a list to store survival plots
splots <- list()

# Define a custom function for generating survival plots
surv_plot <- function(gene) {
  # Initialize a counter for the plots
  i <- 1
  
  # Loop through each gene of interest
  for (g in gene) {
    # Attempt to find the probe associated with the gene
    # Note: hgu219anno and gpl_anno are assumed to be data frames with gene annotations
    hub_gene <- gpl_anno[gpl_anno$Gene.symbol.1 == g, ]['Probe']
    
    # Check if there are multiple probes for the gene
    if (length(hub_gene[, 1]) > 1) {
      # Extract expression data for the probes and calculate the mean
      plotdf <- as.data.frame(t(expma[hub_gene[, 1], ]))
      plotdf$mean <- rowMeans(plotdf)
      
      # Merge the mean expression data with metadata
      surv_a <- plotdf[match(rownames(plotdf), rownames(metdata)), ]
      surv_a$AD <- metdata$AD
      surv_a$AD <- ifelse(surv_a$AD == 'AD', 1, 0) # Convert AD status to binary
      surv_a$AGE_AD <- as.numeric(metdata$AGE)
      # Create quantiles from the mean expression data
      surv_a$quantiles <- as.numeric(cut_number(surv_a$mean, n = 4))
      
      # Create a survival object
      surv_object <- Surv(time = surv_a$AGE_AD, event = surv_a$AD)
      # Fit a survival model
      fit.surv <- survfit(surv_object ~ quantiles, data = surv_a)
      
      # Create a ggsurvplot
      splots[[i]] <- ggsurvplot(
        fit.surv, data = surv_a, pval = TRUE, 
        xlim = c(54, 95), pval.coord = c(65, 0.4),
        risk.table = FALSE, conf.int = FALSE, 
        ylab = '1 - Probability of AD Onset', xlab = 'Age', 
        break.x.by = 10, font.legend = 8, title = g
      )
      i <- i + 1
    } else if (length(hub_gene[, 1]) == 1) {
      # If there is only one probe, use it directly
      surv_a$mean <- expma[hub_gene[, 1], ]
      # ... (The rest of the code is the same as when there are multiple probes)
    } else {
      # If there is no probe for the gene, print a message
      print(paste0(g, ' No probe!'))
    }
  }
  # Return the list of survival plots
  return(splots)
}

# Call the custom function with a vector of gene symbols
# Assume 'genes_of_interest' is a predefined vector containing gene symbols
splots <- surv_plot(genes_of_interest)

# Arrange and print the survival plots in a grid
arrange_ggsurvplots(splots, print = TRUE, nrow = 2, ncol = 3)

#### Perform a multivariable Cox proportional hazards analysis on the 'surdata' dataset####
# The dot '.' in the formula indicates that all variables in 'surdata' (except 'mean') should be included in the analysis
tdmultiCox <- coxph(Surv(time, status) ~ ., data = surdata[, !colnames(surdata) %in% "mean"])

# Obtain a summary of the Cox model results
tdmultiCoxSum <- summary(tdmultiCox)

# Initialize an empty data frame to store the output results
outResult <- data.frame()

# Bind the hazard ratios (HR), 95% confidence intervals (CI), and p-values into the 'outResult' data frame
outResult <- cbind(
  HR = tdmultiCoxSum$conf.int[, "exp(coef)"],
  L95CI = tdmultiCoxSum$conf.int[, "lower .95"],
  H95CIH = tdmultiCoxSum$conf.int[, "upper .95"],
  pvalue = tdmultiCoxSum$coefficients[, "Pr(>|z|)"]
)

# Add an 'id' column to the 'outResult' data frame, using row names as identifiers
outResult <- as.data.frame(cbind(id = row.names(outResult), outResult))

# Write the 'outResult' data frame to a text file, specifying separators and other options
write.table(outResult, file = "./multiCoxClinical.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Extract the gene names from the 'outResult' data frame
gene <- rownames(outResult)
# It seems there is a redundancy here; 'gene' is reassigned the value of 'outResult$id' which are the row names
gene <- outResult$id

# Convert the HR, CI, and p-value columns to numeric types
outResult$HR <- as.numeric(outResult$HR)
outResult$L95CI <- as.numeric(outResult$L95CI)
outResult$H95CIH <- as.numeric(outResult$H95CIH)
outResult$pvalue <- as.numeric(outResult$pvalue)

# Format the HR, lower and upper CI to three decimal places as character strings
hr <- sprintf("%.3f", outResult$"HR")
hrLow <- sprintf("%.3f", outResult$"L95CI")
hrHigh <- sprintf("%.3f", outResult$"H95CI")

# Create a character string for the hazard ratio with formatted values
Hazard.ratio <- paste0(hr, "(", hrLow, "-", hrHigh, ")")

# Format the p-values to either "<0.001" or three decimal places
pValue <- ifelse(outResult$pvalue < 0.001, "<0.001", sprintf("%.3f", outResult$pvalue))

# Initialize a PDF file for the forest plot with specified dimensions
pdf(file = "./UniCoxSurForestPlot.pdf", width = 10, height = 10)

# Set up the layout for the plot
n <- nrow(outResult)
nRow <- n + 1
ylim <- c(1, nRow)
layout(matrix(c(1, 2), ncol = 2), width = c(2.5, 2))

# Set up plot limits and parameters
xlim <- c(0, 2.5)
par(mar = c(4, 2.5, 2, 1))

# Create the forest plot segments for gene names and p-values
# ... (Plotting code for gene names and p-values)

# Create the forest plot segments for hazard ratios and confidence intervals
# ... (Plotting code for hazard ratios and confidence intervals)

# Close the PDF file device
dev.off()