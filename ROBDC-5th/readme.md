
## Overview

### _Scenario_

Imagine you have just been hired by a US Venture Capital firm as a data scientist.
The company is considering foreign grain markets to help meet its supply chain requirements for its recent investments in the microbrewery and microdistillery 
industry, which is involved with the production and distribution of craft beers and spirits.
Your first task is to provide a high level analysis of crop production in Canada. Your stakeholders want to understand the current and historical performance 
of certain crop types in terms of supply and price. For now they are mainly interested in a macro-view of Canada's crop farming industry, and how it relates to 
the relative value of the Canadian and US dollars.

You will be asked questions that will help you understand the data just like a data analyst or data scientist would. You will also be asked to create four tables 
in Db2, and load the tables using the provided datasets from R using the RODBC package. You will be assessed both on the correctness of your SQL queries and 
results, as well as the correctness of your table creation and data loading results.

An R based Jupyter notebook has been provided to help with completing this assignment. Follow the instructions to complete all the problems. Then share your 
solutions with your peers for reviewing.

### Objectives

    * Understand four datasets
    * Load the datasets into four separate tables in a Db2 database
    * Execute SQL queries using the RODBC R package to answer assignment questions

You have already encountered two of these datasets in the previous practice lab, and you will be able to reuse much of the work you did there to successfully 
prepare your database tables for executing SQL queries.

### _Task 1 : Understand the datasets_

To complete the assignment problems in the notebook you will be using subsetted snapshots of two datasets from Statistics Canada, and two samll datasets 
created from a third datasource from the Bank of Canada. The links to the prepared datasets are provided in the next section; interested students can explore 
the landing pages for the source datasets as follows:

    Canadian Principal Crops (Data & Metadata)
    Farm product prices (Data & Metadata)
    Bank of Canada daily average exchange rates

1. Canadian Principal Crops Data *

This dataset contains agricultural production measures for the principle crops grown in Canada, including a breakdown by province and teritory, for each year from 1908 to 2020.
For this assignment you will use a preprocessed snapshot of this dataset (see next section for the link).
A detailed description of this dataset can be obtained from the StatsCan Data Portal at: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210035901
Detailed information is included in the metadata file and as header text in the data file, which can be downloaded - look for the 'download options' link.

2. Farm product prices

This dataset contains monthly average farm product prices for Canadian crops and livestock by province and teritory, from 1980 to 2020.
For this assignment you will use a preprocessed snapshot of this dataset (see next section for the link).
A description of this dataset can be obtained from the StatsCan Data Portal at: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210007701 
The information is included in the metadata file, which can be downloaded - look for the 'download options' link.

3. Bank of Canada daily average exchange rates *

This dataset contains the daily average exchange rates for multiple foreign currencies. Exchange rates are expressed as 1 unit of the foreign currency 
converted into Canadian dollars. It includes only the latest four years of data, and the rates are published once each business day by 16:30 ET.
For this assignment you will use a snapshot of this dataset with only the USD-CAD exchange rates included (see next section). We have also prepared a monthly averaged version which you will be using below.
A brief description of this dataset and the original dataset can be obtained from the Bank of Canada Data Portal at: https://www.bankofcanada.ca/rates/exchange/daily-exchange-rates/

### _Dataset URLs_

    Annual Crop Data: Annual_Crop_Data.csv
    Daily FX Data: Daily_FX.csv
    Monthly Farm Prices: Monthly_Farm_Prices.csv
    Monthly FX Data: Monthly_FX.csv

IMPORTANT: You will be loading these datasets directly into R data frames from these URLs instead of from the StatsCan and Bank of Canada portals. The versions provided at these URLs are simplified and subsetted versions of the original datasets.

### _Task 2 : Load these datasets into four separate Db2 tables._

In this project, you will prepare the database so you can solve problems using SQL in the last portion of the assignemnt, Task 3. 
You will create four tables and load the datasets into them. Perform this step in the provided Jupyter notebook.

#### _Problem 1: Create tables_

Establish a connection to the Db2 database, and create the following four tables using the RODBC package in R.

    CROP_DATA
    FARM_PRICES
    DAILY_FX
    MONTHLY_FX

#### _Problem 2: Read Datasets and Load Tables_

You will read the datasets directly into R dataframes using the urls provided above, and use these to load the tables you created.

### _Task 3: Execute SQL queries using the RODBC R package_
#### _Problem 3_: How many records are in the farm prices dataset?<br>
#### _Problem 4_: Which provinces are included in the farm prices dataset?<br>
#### _Problem 5_: How many hectares of Rye were harvested in Canada in 1968?<br>
#### _Problem 6_: Query and display the first 6 rows of the farm prices table for Rye.<br>
#### _Problem 7_: Which provinces grew Barley?<br>
#### _Problem 8_: Find the first and last dates for the farm prices data.<br>
#### _Problem 9_: Which crops have ever reached a farm price greater than or equal to $350 per metric tonne?<br>
#### _Problem 10_: Rank the crop types harvested in Saskatchewan in the year 2000 by their average yield. Which crop performed best?<br>
#### _Problem 11_: Rank the crops and geographies by their average yield (KG per hectare) since the year 2000. Which crop and province had the highest average yield since the year 2000?<br>
#### _Problem 12_: Use a subquery to determine how much wheat was harvested in Canada in the most recent year of the data.<br>
#### _Problem 13_: Use an implicit inner join to calculate the monthly price per metric tonne of Canola grown in Saskatchewan in both Canadian and US dollars. Display the most recent 6 months of the data.<br>

Author(s)

Jeff Grossman
Other Contributor(s)

Rav Ahuja
Changelog
Date 	Version 	Changed by 	Change Description
2021-04-01 	0.2 	Jeff Grossman 	Replaced screenshots
2021-03-12 	0.1 	Jeff Grossman 	Initial version created
			
© IBM Corporation 2020. All rights reserved.
