# Assignment 4: Climate Change

Climate change is a pressing issue around the globe that has differential impacts on individuals based on where they live and their financial status. In this assignment, you will build an interactive Shiny application exploring CO2 emissions data compiled by Our World In Data. You will need to create all of the necessary files for your work (e.g., `app.R`, `ui.R`, `server.R`).

Shiny App: [LINK TO YOUR SHINY APP HERE]

## Data

The data, which you will load from this [GitHub repository](https://github.com/owid/co2-data/) (specifically the [owid-co2-data.csv](https://github.com/owid/co2-data/) file). A few hints on working with it:

- Read the documentation: See the GitHub Repository to view the "Codebook" that explains the meaning of each variable. You'll need to understand what each variable represents, so read carefully!
- Beware of missing values: You don't need to worry about "imputing" these, but when choosing what to visualize, you will want to focus on a particular location and/or year that has sufficient data (note, this varies across the variables)

## Assignment structure

For this assignment, you will create a multi-page interactive Shiny application that allows users to explore data about CO2 emissions, which must include:

- An introductory page (tab) that introduces the topic, describes the data, and provides some summary values from the dataset
- An interactive visualization page (tab) where users can control a visualization using at least two Shiny widgets
- A page (tab) that briefly considers a Value Sensitive Design issue regarding your own Shiny App

## Introduction + Summary + Data

To introduce your small project, you should describe the features that you've chosen to analyze in your Shiny app and why.

Then, in at least 200 words, describe the dataset that you're analyzing, and be sure to answer the following questions: Who collected the data? How was the data collected or generated? Why was the data collected? What are possible limitations or problems with this data? 

Then, you will share at least 5 relevant values of interest. These will likely be calculated using your DPLYR skills, answering questions such as: 

- What is the average value of my variable across all the countries?
- Where is my variable the highest / lowest?
- How much has my variable change over the last N years?

Feel free to calculate and report values that you find relevant. Importantly, you should calculate these values in your app_server.R file, and display them in your user interface using the appropriate method. 

## Interactive Visualization Page

On your second page (tab), you will create an interactive visualization that is controlled by two or more Shiny widgets. A few requirements for this page:

- The chart must be interactive (e.g., display information when you hover over visual marks), so you will need to use a library such as Plotly or Bokeh (or another interactive package of your choice)
- The chart must have clear axis labels, title, and legend (depending on the chart type)
- At least one of the widgets must change what data is displayed in the chart (e.g., selecting a variable to display in the chart). The other widget can control something like the colors used in the chart, or allow you to select something like the year being displayed
- Below the chart, you need to include a description of why you included the chart, and what patterns emerged as you explored it

## Value Sensitive Design Page

On your third page (tab), you will pick one card/prompt from your Envisioning Cards (available here) and answer the prompt with regard to your own Shiny application. For example, you might select the "Widespread Use" card and answer this prompt (it's fine to actually answer this prompt in your assignment — it's a good one!):

> Envision your system in use by a single stakeholder. Now imagine 100 such individuals interacting with the system. Then 1,000 individuals. Then 100,00. What new interactions might emerge from widespread use? Find three synergistic benefits of widespread use and three potential breakdowns.

Once you have completed your work, make sure to host your application on https://www.shinyapps.io/.
