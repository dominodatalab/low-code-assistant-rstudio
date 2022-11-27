if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("Biostrings", quietly = TRUE))
  BiocManager::install("Biostrings")

if (!require("DECIPHER", quietly = TRUE))
  BiocManager::install("DECIPHER")

library(Biostrings)
library(DECIPHER)

# Multiple sequence alignment
DNASeq <- list()
DNASeq[[1]] <- DNAString("ATCGGCTACGTGGCATCAGTCAGTACTCATAC")
DNASeq[[2]] <- DNAString("ATCGGCTACGTGTCATCAGTCAGTACAGTTAC")
DNASeq[[3]] <- DNAString("ATCGGCTACGTCAGTTCAGTCAGTACATTTACCC")
seqs <- DNAStringSet(unlist(DNASeq))

aligned <- AlignSeqs(seqs, verbose = FALSE)
print(aligned)
