# A Scoping Review of Study Designs, Integration Methods, Reporting Quality and Reproducibility in Multi-Omics Analysis of *In Vitro* Microbiome Systems

This repository contains the R code for the analyses of the 102 studies included in the scoping review. The Figure 2 script reproduces the PRISMA study selection diagram from screening counts recorded in the script. The remaining analyses use the study-level data in Supplementary Appendix 2 to reproduce manuscript Figures 3–7, Supplementary Figures 1–3, and Supplementary Tables S1–S16.

## Reproduce Figure 2

From the repository root, run the [PRISMA flow diagram script](R/reproduce_PRISMA.R):

```sh
Rscript R/reproduce_PRISMA.R
```

Figure 2 runs independently of Supplementary Appendix 2. The script installs any missing R packages (`PRISMA2020`, `DiagrammeRsvg`, `xml2`, and `rsvg`) and writes a single vector PDF to `PRISMA_output/Fig2.pdf`.

## Download the study data for Figures 3–7 and the supplementary analyses

Download **Supplementary Appendix 2** (Excel workbook) from the article’s supplementary materials. Save it in the repository’s [data folder](data/) so the path from the repository root is:

```text
data/Supplementary Appendix 2.xlsx
```

The filename `Supplementary_Appendix_2.xlsx` is also accepted. Keep the `Data Extraction` worksheet and its two header rows intact. The workbook also contains the codebook and abbreviations.

## Run the study-level analyses

1. Install the required packages once in R.

   ```r
   install.packages(c(
     "readxl", "dplyr", "tidyr", "stringr", "forcats", "ggplot2", "scales",
     "patchwork", "openxlsx", "countrycode", "sf", "rnaturalearth", "rnaturalearthdata"
   ))
   ```

2. From the directory containing `run_all.R`, run the following command in a terminal.

   ```sh
   Rscript run_all.R
   ```

   In RStudio, set the working directory to the same directory and run `source("run_all.R")`. To use a workbook stored elsewhere, pass its path explicitly, for example `Rscript run_all.R "/path/to/Supplementary Appendix 2.xlsx"`.

## Find the results

| Output | Contents |
|---|---|
| `figures/` | PDF versions of the manuscript and supplementary figures. The complete versions of Figures 3 and 4 are `Figure_3_composed.pdf` and `Figure_4_preprocessing.pdf`. |
| `results/Supplementary_tables.xlsx` | Supplementary Tables S1–S16 in manuscript order, with one table per worksheet. Row 1 contains the caption, row 2 contains the column headers, and the data begin in row 3. |
| `results/*.html` | Six reports containing the section results and supporting tables. The Section 3.10 report includes the study-level evidence used to summarize data availability. |
| `results/sessionInfo.txt` | R and package versions recorded for the run. |

## Match a manuscript result to its script

| Results section | Analysis | Figures and supplementary tables |
|---|---|---|
| [3.1](R/Section_3_1_Study_selection_overview.R) | Study overview, publication years, and geographic distribution | Supplementary Figures 1–2 |
| [3.2](R/Section_3_2_Biological_systems_experimental_design_and_Omics_Layers.R) | Biological systems, experimental design, donors, replication, and omics layers | Figure 3a–b and Supplementary Tables S1–S8 |
| [3.3](R/Section_3_3_Data_Characteristics_and_Preprocessing.R) | Data characteristics and preprocessing of microbiome and metabolomics data | Figure 3c, the combined Figure 3, Figure 4, and Supplementary Tables S9–S15 |
| [3.4–3.8](R/Sections_3_4_to_3_8_Integration_analyses.R) | Integration depth and breadth, analytical method families, network analysis, and relationships with study design | Figures 5–7 and Supplementary Figure 3 |
| [3.9](R/Section_3_9_Software_and_Tools.R) | Software packages, tools, and analysis environments | Supplementary Table S16 |
| [3.10](R/Section_3_10_Validation_Reproducibility_and_Quality_Indicators.R) | Validation practices, reporting quality, and availability of data and code | Section 3.10 report |

The section scripts share a [workbook reader](R/read_extraction.R) and [figure style settings](R/00_style.R). Use `run_all.R` to assemble the three panels of Figure 3 and collect the supplementary tables into a single workbook.
