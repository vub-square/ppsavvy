## EXCITE Shiny application   
*Exploring Complex Interactions using Tree-based models*  

This research tool helps researchers uncover and visualise complex interactions in their data using a combination of ANOVA and decision tree modeling. Designed for ease of use, EXCITE offers a seamless workflow from data import to advanced statistical analysis, with the ability to download reports summarising your results. 

The application is freely accessible at <https://percysavieri.shinyapps.io/EXCITE/>


### User-interface
The web application provides a user-interface (UI) to upload data and fit the regression model under review. It is comprised of the sidebar panel (for managing inputs) and the main panel (for viewing outputs).  

*Key Features
  + Interactive data import and management options.
  + Two- and Three-Way ANOVA modeling with clear visualizations.
  + Decision tree exploration to uncover complex interactions.
  + Dynamic report generation in multiple formats (HTML, PDF, Word).


* The sidebar panel is subdivided into 4 tabs:
  +	Data Input
  +	Define Model
  +	Transform
  +	Reset  

* The main panel consists of 6 tabs:
  +	View Data
  +	Model Summary
  +	Linearity Assumption
  +	Linearity Assumption: C+R Plots
  +	Normality Assumption
  +	Download Report  

Each output tab is associated with R code to perform analyses. These main panels are conditional on the sidebar panel selected and this is achieved through a dynamic UI.

### Code Structure
The 'app.R' file is the main R file; all the others R files are called here.

* There are two main functions defined: ui and server:
  1. The ui part of Shiny, written in HTML and CSS, contains the main ui functions that call the other ui functions and set up the structure of the ui (sidebar panel/main panel). It handles user input, server output and ui display.

  2. The server part of Shiny, written in R, contains functions which handles the input from ui and process the output within a reactive value. These functions return a reactive value output. It processes the ui input to calculate output, communicates via keywords associated to each input and output functions declared in the ui function.  

            
Version 06-Dec-2022
