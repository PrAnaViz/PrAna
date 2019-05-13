Prescription Data Analysis
===
author: Kishore Jagadeesan
date: 13 May, 2019
width: 960
height: 700
transition: linear
css: prana_summary.css

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
* Features
* Summary

Prescription Data Analysis
===
Tool to calculate the total amount of Active Pharmaceutical Ingredients (APIs) prescribed and to monitor prescribing trends over different spatial and temporal regions.

Background
===

 * To predict the total amount of APIs, released to the environment and to calculate Predicted environmental concentration (PEC), information on the manufacturing or consumption is vital.
 * And for certain groups of APIs, assessment of their consumption trends over spatial and temporal regions helps to understand and correlate with various facts like drug abuse and antimicrobial resistance.
 
Data and Strucutre
===

 * NHS Prescription Dataset
  * Monthly dataset ~ 1.3 GB
  * more than 500 million rows
  

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
  