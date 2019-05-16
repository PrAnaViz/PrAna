

Prescription Data Analysis
===
author: Kishore Jagadeesan
date: 16 May, 2019


<!-- NOTE: Styling and external images may be missing --> 
<p>Postdoctoral Research Associate
  <br/>
  Department of Chemistry
  <br/>
  University of Bath
</p>

Outline
===
* Prescription Data Analysis
  * Background
  * Data and Strucutre
  * Data management, aggregation
* Demo - Features
<!-- * Additional Features -->
* Summary

Prescription Data Analysis
===
Tool to calculate the total amount of Active Pharmaceutical Ingredients (APIs) prescribed and to monitor prescribing trends over different spatial and temporal regions.

Background
===

 * To predict the total amount of APIs, released to the environment and to calculate Predicted Environmental Concentration (PEC), information on the manufacturing or consumption is vital.
 * And for certain groups of APIs, assessment of their consumption trends over spatial and temporal regions helps to understand and correlate with various facts like drug abuse and antimicrobial resistance.
 
Data and Strucutre
===

* NHS Prescription Dataset
  * Monthly dataset ~ 1.3 GB
  * more than 500 million rows
<img src="img/rpres/nhs_prescription_01.PNG"  />  

Data and Strucutre
===

* NHS Data Model and Dictionary (NHS dm+d)
  * Virtual Therapeutic Moiety (VTM)      - Atenolol 
  * Virtual Medicinal Product (VMP)       - Atenolol 100 mg tablets 
  * Virtual Medicinal Product Pack (VMPP) - Atenolol 100 mg tablets x 28 tablet
  * Actual Medicinal Product (AMP)        - Atenolol 100 mg tablets (Almus Pharmaceuticals Ltd) 
  * Actual Medicinal Product Pack (AMPP)  - Atenolol 100 mg tablets (Sandoz Ltd) x 28 tablet

Active ingredient(s) Strength Dose form Pack level Identified supplier

Data management, aggregation
===

 * Cannot be directly combined based on different APIs.
 * Do not facilitate the calculation of the consumption data.
 * Do not facilitate the assessment of seasonal or location based trends.

Data management, aggregation
===
* R
* Packages used
  * shiny
  * shinydashboard
  * data.table
  * tidyr
  * reshape2
  * leaflet
  * plotly

Prescription Data Analysis
===

With this interactive tool, we have made it easy to caculate the total quantity of different APIs prescribed and to monitor prescribing trends over different spatial and temporal regions.

Demo
===

https://jkkishore85.shinyapps.io/PDA_1_0/

Demo - Login Page
===
* Login page to restrict users
<img src="img/rpres/prana_login.PNG"  />

Demo - Front page
===

<img src="img/rpres/prana_front.PNG"  />

###### 1 - User Identification; 2, 3 - Input files ; 4,5,6 - Different View Options

Demo - Input files
===

<img src="img/rpres/input_files.PNG"  />

#####  2 - Target APIs list in .csv format
#####  3 - Grouping of APIs based on the chemical class in .csv format. This can be modified based on the usage of data.

Demo - Time series
===

<img src="img/rpres/timeseries_01.png"  />

#####  5 - Select the STP catchment area
#####  6, 7 - Options to see the trends based on a target API or GP surgery (Options 9 and 10)
#####  8 - Display the total prescription data for the year 2014-2018 in kg (For all API from the Target list )
#####  9 - Display the total prescription data for the year 2014-2018 in kg (For a particular target API)
#####  10 - Display the total prescription data for the year 2014-2018 in kg (For a particular target GP surgery)

Demo - Time series
===
<img src="img/rpres/timeseries_03.gif"  />

 * By selecting the API in the barplot, we able to see the trend of the API over the period
 * Download buttons helps to download the data in .csv files
 
Demo - Targeted API
===
<img src="img/rpres/targeted_01.gif"  />

 * By selecting the API in the dropdown, we able to see the trend of the API over the period

Demo - Timeseries GP
===
<img src="img/rpres/targeted_gp.gif"  />

 * To see the trend of the API over the period with respective to different GP practice in the particular STP catchment area
 * Working on getting the trend of different APIs over the selection of targeted GP practice

Demo - Particular year
===

<img src="img/rpres/particular_year_01.png"  />

#####  11 - Select the STP catchment area
#####  12 - Select the year of interest
#####  13 - Select the targeted GP of interest (for the option 16)
#####  14 - Display the total prescription data for the selected year in kg (For all API from the Target list )
#####  15 - Display the total prescription data per month for the selected year in kg (For all API from the Target list )
#####  16 - Display the total prescription data on targted GP for the selected year in kg (For all API from the Target list )

Demo - Particular year
===

<img src="img/rpres/particular_year_01.gif"  />

  * By selecting the API in the barplot, we able to see the trend of the API over,
    * Period
    * GP Practice - Total
    * Chemical form
    * Medicinal form
    * GP Practice - Monthwise
    
Demo - Monthwise APIs
===

<img src="img/rpres/monthwise_API.gif"  />

* By selecting the year in the dropdown, we able to see the monthwise trend of all APIs for a particular STP

Demo - Monthwise GP
===

<img src="img/rpres/monthwise_gp.gif"  />

* By selecting the GP Practice code in the dropdown, we able to see the monthwise trend of all APIs for a particular GP

Demo - Compare STW
===

<img src="img/rpres/compare_stw_01.png"  />

#####  17 - Select the year of interest
#####  18 - Comparison of the APIs for the selected year - 2D Plot 
#####  19 - Comparison of the APIs for the selected year - 3D Plot

Demo - Compare STW - 2D plot
===

<img src="img/rpres/compare_stw_01.gif"  />


* By selecting the year in the dropdown, we able to see the year wise trend of all APIs for different STPs

Demo - Compare STW - 3D plot
===

<img src="img/rpres/compare_stw_02.gif"  />

* By selecting the year in the dropdown, we able to see the year wise trend of all APIs for different STPs

<!-- Additional Features - Heatmap -->
<!-- === -->

<!-- <img src="img/rpres/add_heatmap_01.png"  /> -->

<!-- #####  20 - Data with total APIs   -->
<!-- #####  21 - Select API  -->
<!-- #####  22 - Select Year -->
<!-- #####  23 - Heatmap Tab -->

<!-- Additional Features - Pie Chart -->
<!-- === -->

<!-- <img src="img/rpres/add_piechart_01.png"  /> -->

<!-- #####  20 - Data with total APIs   -->
<!-- #####  21 - Select API  -->
<!-- #####  22 - Select Year -->
<!-- #####  24 - PieChart Tab -->

Summary
===

 * The tool helps to calculate the consumption data for different APIs over the time period 2014-2018
 * Also helps in the assessment of consumption trends over spatial and temporal regions, which are interesting for a number of applications
 * It has present limitation in calculation the quantity of the APIs prescribed as ‘solution for injection’
 * Hosted in a private server - R Shiny server
 * Initiated the idea of hosting it in the University of Bath server
