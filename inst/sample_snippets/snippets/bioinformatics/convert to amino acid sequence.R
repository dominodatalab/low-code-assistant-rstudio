if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("Biostrings", quietly = TRUE))
  BiocManager::install("Biostrings")

library(Biostrings)

dna_seq <- "ATTTCGGTAATACGG"
translate(DNAString(dna_seq))
