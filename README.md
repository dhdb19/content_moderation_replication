# Replications instructions: Content moderation dynamics using the DSA transparency database

Below follow instructions to replicate our study. The replication files can be accessed on github: https://github.com/dhdb19/content_moderation_replication.

## Download files

Download scripts were graciously provided by Sebastian Nagel (see https://github.com/transparency-in-content-moderation/eu-dsa-transparency-db-sor-data-downloader/tree/main)

1. Execute the ``download_data/sor-download.sh`` script to download the daily dump files. Specify positional arguments ``directory``, ``date`` (format YYYY-mm-dd), and ``days``, with days the number of days after date that should also be included (default = 1)
2. Execute the ``download_data/sor-convert.sh`` script with ``year\=YYYY/month\=mm/day\=dd/sor-global-YYYY-mm-dd-full.zip`` as the argument to unzip the csv files. Replace YYYY, mm, dd with the appropriate dates.
3. Repeat for every date of interest

These steps will take considerable time. This produces a single compressed parquet file per day. 



## Create dataset

The daily dump files can be compiled into one large database in any way researchers prefer (researchers will need to install and load appropriate packages for their preferred choice). We compiled all the daily dump files into one duckdb database. Duckdb needs to be installed as a dependency for this. 

1. Run the ``data_prep/create_database.r`` R script to create the duckdb database. 

This script will create a database file (size ~ 5GB) and the appropriate tables for analysis. When using the dplyr and tidyverse packages, data R should employ datastreaming for data manipulation in most cases. For some function calls, data will need be loaded into memory, which will impact computer performance for up to a few minutes, depending on computer specifications (may be as short as 30 seconds).

## Statistical analysis

All our results, graphics, and tables can be replicated from one R script

1. Run the ``data_prep/models_graphs_tables.r`` R script
