# A comparison of genomic features in _Pseudomonas putida_ GenBank and RefSeq annotations

This repository contains data derived from the RefSeq (NC_002947.4) and Genbank (AE015451.2) annotations of the _Pseudomonas putida_ genome.
This analysis is part of an upcoming publication comparing the usage of these different annotations across the literature.

## Main file structure
- `data/` : files downloaded from [NCBI's FTP server](https://ftp.ncbi.nlm.nih.gov/) for the GenBank (GCA_000007565.2) and RefSeq (GCF_000007565.2) versions of the _P. putida_ genome assembly.
- `output/`: formatted tables (.TSV) of the genomic coordinates and other relevant information of all annotated features present in the _*feature_table.txt_ files present in the `data/` folder

### Notebooks, scripts, and data
- `data-download.ipynb`, `preprocess-refseq.ipynb` and `preprocess-ncbi.ipynb` : Jupyter notebooks with code for downloading data from NCBI's FTP server and generating the parsed tables found in the `output/` directory.
- `createdb.R`: script that creates an SQLite database in R from the TSV files in `output/`.
- `figure*.R` and `table - size differences.R`: scripts that use the data imported in `createdb.R` to generate the figures and tables provided in the manuscript. 
- `aledb.csv`, `aledb - data.R` and `aledb - plot.R`: contain the dataset manually downloaded from [ALEdb](https://aledb.org/) and scripts for processing and visualizing it.
- `affected_newformat` and `affected_oldformat` : R vectors, stored as binary files, with the RefSeq and GenBank genomic loci codes of the 897 shifted genes described in the main text, respectively.
- `utils.R` and `kegg_pathway_genes.R`: smaller scripts containing functions for running the Fisher exact test and retrieving data from KEGG, respectively.
- `ppuCog.tsv` : table containing all available [COG](https://www.ncbi.nlm.nih.gov/research/cog/) annotations for _P. putida_ KT2440.

## Additional information
If you have any questions or feedback, please contact the first author Guilherme (Gui) Viana de Siqueira at gmvsiq@gmail.com.
