####This code performs differential methylation analysis, GO, and KEGG pathway analysis, and then visualizes the results using various plots and charts.#### 
#It also simplifies and visualizes the GO enrichment results using the 'rrvgo' R package.
# Load the 'missMethyl' library which is used for DNA methylation analysis.
library(missMethyl)
load("methy-deg.Rdata")

# Store the names of differentially methylated probes into 'sig.cpg'. 
sig.cpg <- rownames(deg[deg$P.Val <= 0.01,])

# Store all probe names into 'all.cpg'.
all.cpg <- rownames(deg)

#### GO Analysis ####
# Perform Gene Ontology (GO) analysis with the 'gometh' function.
gst <- gometh(sig.cpg = sig.cpg, all.cpg = all.cpg, collection = "GO", 
              plot.bias = TRUE, prior.prob = TRUE, array.type = "450K")

# Create a frequency table of the P.DE values less than 0.01.
table(gst$P.DE < 0.01)  # For 'deg1': use gst$FDR < 0.05, for 'deg3': use gst$P.DE < 0.01, for 'deg2': use gst$P.DE < 0.01.

# Identify the top GO terms using 'topGSA' function.
GO.sig <- topGSA(gst)

# Write the significant GO terms to a CSV file.
write.csv(gst[gst$P.DE < 0.01,], 'GO.sig-mcicn.csv')

#### KEGG Analysis ####
# Perform Kyoto Encyclopedia of Genes and Genomes (KEGG) analysis with the 'gometh' function.
kegg <- gometh(sig.cpg = sig.cpg, all.cpg = all.cpg, collection = "KEGG", 
               plot.bias = TRUE, prior.prob = TRUE, array.type = "450K")

# Create a frequency table of the P.DE values less than 0.05.
table(kegg$P.DE < 0.05)

# Identify the top KEGG pathways using 'topGSA' function.
KEGG.sig <- topGSA(kegg)

# Write the significant KEGG pathways to a CSV file.
write.csv(KEGG.sig, 'KEGG.sig-mcicn.csv')

# Top 20 significant pathways bubble plot:
# Arrange the pathways by P-value.
p2 <- ggplot(data = KEGG.sig[1:20,],
             aes(x = DE, y = Description)) +
  geom_point(aes(size = DE, color = -log10(FDR))) +  # Set bubble size and color based on significance.
  scale_color_distiller(palette = "Spectral", direction = -1) +
  labs(x = "Gene Number",
       y = "",
       title = "Dotplot of Enriched KEGG Pathways",
       size = "gene number") +  # Legend title
  theme_bw()

# Display the plot.
p2

#### GO Visualization ####
# Load the 'simplifyEnrichment' library for simplifying GO term analysis.
library(simplifyEnrichment)

# Calculate a matrix of GO term similarities.
mat = GO_similarity(rownames(GO.sig), ont = 'BP')

# Simplify the GO terms.
simplifyGO(mat)

# Load additional libraries for visualization and analysis.
library(enrichplot)
library(clusterProfiler)
library(org.Hs.eg.db)

# Load the 'rrvgo' library for simplifying and visualizing GO enrichment results.
library(rrvgo)

# Calculate a similarity matrix for GO terms using 'calculateSimMatrix' function.
simMatrix <- calculateSimMatrix(rownames(gst[gst$P.DE < 0.01,]), 
                                orgdb = "org.Hs.eg.db",
                                ont = "BP",
                                method = "Rel")

# Set the scores as the negative logarithm of the P.DE values and name them with the GO term names.
scores <- setNames(-log10(gst[gst$P.DE < 0.01,]$P.DE), rownames(gst[gst$P.DE < 0.01,]))

# Reduce the similarity matrix to a set of non-redundant GO terms using 'reduceSimMatrix' function.
reducedTerms <- reduceSimMatrix(simMatrix,
                                scores,
                                threshold = 0.7,
                                orgdb = "org.Hs.eg.db")

# Create a heatmap of the GO term similarity matrix and save it as a PDF file.
pdf("./heatmapPlot.pdf", width = 8, height = 6)
heatmapPlot(simMatrix,
            reducedTerms,
            annotateParent = TRUE,
            annotationLabel = "parentTerm",
            fontsize = 6)
dev.off()

# Create a scatter plot of the GO term similarity matrix and save it as a PDF file.
pdf("./scatterPlot.pdf", width = 8, height = 8)
scatterPlot(simMatrix, reducedTerms)  # For 'deg3', remove onlyParents = TRUE if present.
dev.off()

# Create a treemap plot of the reduced GO terms and save it as a PDF file.
pdf("./treemapPlot.pdf", width = 10, height = 6)
treemapPlot(reducedTerms)
dev.off()
