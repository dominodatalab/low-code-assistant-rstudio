if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("Biostrings", quietly = TRUE))
  BiocManager::install("Biostrings")

library(Biostrings)

DNASeq1 <- DNAString("ATACTGGGTAACCCATGCTCTAGCATGCTA")
DNASeq2 <- DNAString("ATACTGGCTAACCGAAGCTCTAGCATGCTA")

myAlign <- pairwiseAlignment(DNASeq1, DNASeq2)
print(myAlign)
