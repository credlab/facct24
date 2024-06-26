## Overview
Contains data, code, and results for ["Algorithmic Misjudgement in Google Search Results: Evidence from Auditing the US Online Electoral Information Environment"](https://facctconference.org/static/papers24/facct24-31.pdf)

Citation:
Brooke Perreault, Johanna Hoonsun Lee, Ropafadzo Shava, and Eni Mustafaraj. 2024. Algorithmic Misjudgement in Google Search Results: Evidence from Auditing the US Online Electoral Information Environment. In The 2024 ACM Conference on Fairness, Accountability, and Transparency (FAccT ’24), June 03–06, 2024, Rio de Janeiro, Brazil. ACM, New York, NY, USA, 11 pages. https://doi.org/10.1145/3630106.3658916

## Scraping Audit Materials
1. FAccT_Public_Audit_Locations.csv: locations where data was collected, with indicator for whether location was later dropped in analysis.
2. FAccT_Public_Queries.csv: search queries and query categories
3. facct_public_gov_domains.csv: non-federal government domains and geoprovenance information for domains included in analysis (i.e., after error locations were dropped)

## SERP links
Parsed SERP data (extracted organic results and ranks, organized by date) is available on Google Drive as [merged_rank.zip](https://drive.google.com/drive/folders/1mQR_ZwS2JPBtd6YKam962V7Ld0PfH39S?usp=drive_link):

## Analysis Results
### SERP level 
Analysis results come in two forms within the [facct_results.zip folder on Google Drive](https://drive.google.com/drive/folders/1mQR_ZwS2JPBtd6YKam962V7Ld0PfH39S?usp=drive_link):
- Txt files that include a line per SERP that labels each organic result with its type (non-govt, federal, state, county, local, rep, sen, native), whether it is in the correct location (correctly targeted) and whether it is in state.
- CSV files that include  a row per SERP with the number of government results of each type, and proportions for correctly targeted and mistargeted.

### Aggregate mistargeting results
- results_by_query_category.csv: table with government domain appearances and mistargeting rates by query category.

## Code
1. FAccT Proportions - Public.ipynb: notebook that processes organic result information and generates files found in facct_results folder. Currently includes some hardcoding.
2. facct_analysis: R file that processes facct_results CSVs to get mistargeting rates, generating relevant tables and graphics.


