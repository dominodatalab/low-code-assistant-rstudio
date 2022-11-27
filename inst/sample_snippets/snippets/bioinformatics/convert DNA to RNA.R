dna_seq <- "ATTTCGGTAATACGG"
rna_seq <- chartr(old="T", new="U", dna_seq)
print(rna_seq)
