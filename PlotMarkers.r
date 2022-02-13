# R script to read in a table of marker genes
# and color the cells of the umap according to the expression levels of those genes

glists.raw <- read.table("scRNA_data/gene_lists_human_180502.csv", sep=",",row.names=NULL,header=TRUE,as.is=TRUE); # gene lists
glists <- glists.raw[which(glists.raw$Name %in% rownames(scrna)),]; # filtered genelists

# plot individual genes in various classes:
  for (i in unique(glists$List)) {
    gplotlist=list();
    j=gsub(" ","_",i);
    j=gsub("/","_",j);
    genesToPlot = glists$Name[which(glists$List == i)];
    ng = length(genesToPlot); # number of genes
    outfile = sprintf("UMAP.%s.pdf",j);
    print(outfile);
    h = 0;
    w = 0;
    if (ng > 1) {
      w = 11; # two columns
      h = 5*(floor(ng/2) + ng %% 2); # number of rows
      pdf(outfile, height=h, width=w);
      fp <- FeaturePlot(object = scrna, features = genesToPlot, cols = c("gray","red3"), ncol=2, reduction = "umap");
      print(fp);
      dev.off();
    } else {
      w = 5;
      h = 5;
      pdf(outfile, height=h, width=w);
      fp <- FeaturePlot(object = scrna, features = genesToPlot, cols = c("gray","red3"), ncol=1, reduction = "umap");
      print(fp);
      dev.off();
    }
  }
