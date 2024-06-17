#This script involves multiple steps, including data conversion, gene set enrichment analysis for both GO terms and KEGG pathways, 
#and various visualization techniques such as bubble plots, circular plots, chord plots, and cluster plots. The script also includes functions to save the plots as PDF files for further use or reporting. Adjustments to parameters like color schemes, 
#label thresholds, and font sizes are made to customize the appearance of the plots.

####mRNA####
# Load required libraries
library(clusterProfiler)
library(org.Hs.eg.db)

# Check available key types in org.Hs.eg.db package for data conversion
keytypes(org.Hs.eg.db)

# Define a vector of Ensembl IDs from the row names of a gene expression data frame 'exp'
Ensembl_ID <- rownames(exp)

# Convert Ensembl IDs to gene symbols and ENTREZ IDs using bitr() function
gene_symbol <- bitr(Ensembl_ID, fromType="ENSEMBL", toType=c("SYMBOL", "ENTREZID"), OrgDb="org.Hs.eg.db")

# Preview the conversion result
head(gene_symbol)

# Filter differentially expressed genes based on P.Value <= 0.01
gene_symbol <- degs2[degs2$P.Val <= 0.01,]

# Create a vector of logFC (log fold changes) with gene symbols as names
genelist <- gene_symbol$logFC
names(genelist) <- gene_symbol$Gene

# Sort the genelist in decreasing order
genelist <- sort(genelist, decreasing = TRUE)

# Perform Gene Set Enrichment Analysis (GSEA) for GO terms
gsea_res <- gseGO(gene = genelist,
                  OrgDb = "org.Hs.eg.db",
                  keyType = "SYMBOL",
                  ont = "ALL",
                  pvalueCutoff = 0.05,
                  pAdjustMethod = "BH",
                  minGSSize = 10,
                  maxGSSize = 500
)

# Preview the GSEA results
head(gsea_res, 3)

# Save the GSEA results to a CSV file
write.csv(gsea_res, "./gsea_res.csv")

# Visualize the GSEA results using cnetplot
cnetplot(gsea_res, showCategory=3, color.params = list(foldChange = genelist))

# Perform enrichment analysis for KEGG pathways
# Filter genes based on P.Value <= 0.01 and convert gene symbols to ENTREZ IDs
gene_ENTREZ <- bitr(gene_symbol$Gene, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")

# Remove duplicate ENTREZ IDs
gene_ENTREZ <- dplyr::distinct(gene_ENTREZ, SYMBOL, .keep_all=TRUE)

# Merge gene expression data with ENTREZ IDs and sort by logFC
data2 <- gene_symbol %>%
  inner_join(gene_ENTREZ, by=c("Gene"="SYMBOL"))
data_sort <- data2 %>%
  arrange(desc(logFC))

# Prepare a named gene list for KEGG enrichment analysis
gene_list <- data_sort$logFC
names(gene_list) <- data_sort$ENTREZID

# Perform KEGG enrichment analysis
resKEGG <- enrichKEGG(
  names(gene_list),    # Gene set based on logFC ranking
  organism = "hsa",    # Abbreviation for human
  pvalueCutoff = 0.05,
  pAdjustMethod = "BH",
  qvalueCutoff = 0.2
)

# Visualize KEGG enrichment results with enrichplot
ora_pt <- pairwise_termsim(resKEGG)
# Save a treeplot to a PDF file
pdf("./treeplotgsekegg.pdf", width = 15, height = 6)
treeplot(ora_pt, fontsize=3)
dev.off()

# Save a dotplot to a PDF file
pdf("ssplotgseGO.pdf", width = 10, height = 8)
ssplot(ora_pt, showCategory=30)
dev.off()

# Save a ridgeplot to a PDF file
pdf("gseaplot2.pdf", width = 10, height = 8)
ridgeplot(gsea_res) + labs(x = "enrichment distribution")
dev.off()

# Save a GSEA plot to a PDF file
pdf("gseaplot.pdf", width = 10, height = 8)
gseaplot2(gsea_res, 1:30)
dev.off()

# Save a volcano plot to a PDF file
volcanoGsea(data = gsea_res, nudge.y = c(-0.8, 0.8))
ggsave("volcanoGsea.pdf", width = 10, height = 10)

# Select top 3 mitochondrial-related GO terms and plot with gseaNb
terms <- gsea_res[grepl("mitochondrion", gsea_res@result$Description),]$ID
terms <- c(terms, gsea_res$ID[1:4])
pdf("dotplotGseaGO-gseaNb.pdf", width = 10, height = 10)
gseaNb(object = gsea_res, geneSetID = terms)
dev.off()

# Perform GO enrichment analysis with enrichGO and visualize with GOplot
ego_ALL <- enrichGO(gene = gene_symbol$Gene,
                    OrgDb = org.Hs.eg.db,
                    keyType = "SYMBOL",
                    ont = "ALL",
                    pAdjustMethod = "BH",
                    minGSSize = 1,
                    pvalueCutoff = 0.5,
                    qvalueCutoff = 0.2,
                    readable = TRUE)

# Convert the result to a data frame and modify column names for GOplot
ego_ALL <- as.data.frame(ego_ALL)
colnames(ego_ALL) <- c("Category", "ID", "Term", "GeneRatio", "BgRatio", "pvalue", "adj_pval", "qvalue", "Genes", "Count")
ego_ALL$Genes <- gsub("/", ",", ego_ALL$Genes)

# Generate a data frame for GOplot
circ <- circle_dat(as.data.frame(ego_ALL), as.data.frame(gene_symbol))

# Plot a bubble chart with GOplot
GOBubble(circ, title = 'Bubble plot', colour = c('orange', 'darkred', 'gold'), display = 'multiple', labels = 3)

# Reduce redundant terms and visualize
reduced_circ <- reduce_overlap(circ, overlap = 0.75)
top_5 <- reduced_circ %>%
  group_by(category) %>%
  top_n(10, -adj_pval) %>% 
  ungroup()

# Save the top 5 results to a new data frame
new_df <- data.frame(top_5)

# Save a bubble plot to a PDF file with customized colors
pdf("GOBubble.pdf", width = 18, height = 10)
GOBubble(new_df, labels = 0.7, colour = c("#50C78E", "#F5BCE1", "#FEC17A"))
dev.off()

# Save a circular plot to a PDF file
pdf("GOCircle.pdf", width = 18, height = 8)
GOCircle(circ, lfc.col = c('#94E1F9', '#FFDB7C'), zsc.col = c('#AAE2A8', '#DAFED1', '#F09494'))
dev.off()

# Create a chord plot with GOplot
chord <- chord_dat(data = circ, genes = gene_symbol, process = ego_ALL$Term[1:5])
GOChord(chord, space = 0.02, gene.order = 'logFC', gene.space = 0.25, gene.size = 5)

# Create a cluster plot with GOplot
GOCluster(circ, ego_ALL$Term[1:5], clust.by = 'Term')
ggsave("GOCluster.pdf", width = 15, height = 8)

####miRNA####
# Load previously saved R data file containing the results of a differential gene expression analysis
load("miRNA-deg.Rdata")

# Rename the column names of the 'degs' data frame and keep only unique rows
colnames(degs) <- c("Gene", "log2FoldChange", "Pvalue", "FDR")
degs <- unique(degs)

# Display the first few rows of 'degs1'
head(degs)

# Create a new data frame 'dg_lists' that combines significant genes (adjusted P-Value <= 0.05) 
# from three different conditions into a single data frame with three columns
dg_lists <- data.frame(
  AD_vs_CONTROL = degs1[degs1$adj.P.Val <= 0.05, ]$Gene,
  AD_vs_MCI = degs2[degs2$adj.P.Val <= 0.05, ]$Gene,
  MCI_vs_CONTROL = degs3[degs3$adj.P.Val <= 0.05, ]$Gene
)

# Set column names for better readability
colnames(dg_lists) <- c("AD vs. CONTROL", "AD vs. MCI", "MCI vs. CONTROL")

# Create an UpSet plot visualizing the intersection of differentially expressed gene sets
# and save it to a PDF file
pdf("./upsert.pdf", width = 8, height = 6)
upsetr_plot(
  data = dg_lists,
  sets_num = 3,
  keep_order = TRUE,
  order_by = "freq",
  decrease = TRUE,
  # Additional parameters for customization ...
)
dev.off()

# Create a Volcano plot for the differential expression data 'degs2'
# and save it to a PDF file
pdf("./volcano_plot.pdf", width = 8, height = 6)
volcano_plot(
  data = degs2,
  title = group_name,
  log2fc_cutoff = 0.02,
  pq_value = "pvalue",
  pq_cutoff = 0.05,
  # Additional parameters for customization ...
)
dev.off()

# Create an MA plot for the differential expression data 'degs'
# and save it to a PDF file
pdf("./maplot.pdf", width = 7, height = 6)
ma_plot(
  data = degs,
  foldchange = 0.02,
  fdr_value = 0.2,
  # Additional parameters for customization ...
)
dev.off()

# Create a Gene Rank plot for the differential expression data 'degs3'
# and save it to a PDF file
pdf("./gene_rank_plot.pdf", width = 7, height = 6)
gene_rank_plot(
  data = degs3,
  log2fc = 1,
  palette = 'PRGn',
  top_n = 10,
  # Additional parameters for customization ...
)
dev.off()

# Merge differential gene expression data from three conditions into a single data frame
merge_deg <- merge(
  merge(deg1[, c("logFC", "P.Value", "Gene")], deg2[, c("logFC", "P.Value", "Gene")], by = "Gene", all = TRUE),
  deg3[, c("logFC", "P.Value", "Gene")], by = "Gene", all = TRUE
)

# Export the merged differential gene expression data as a CSV file
write.csv(merge_deg, "mirna-merge_deg.csv")