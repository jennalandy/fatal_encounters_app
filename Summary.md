# Final Project Summary
#### Jenna Landy

## 1. Blurb

The [Fatal Encounters](https://fatalencounters.org/) project was founded on the premise that Americans should have the ability to track circumstances where police use deadly force, even though these circumstances are not tracked by any governmental organization. This project, run by D. Brian Burghart, has built a database of all deaths through police interaction in the United States since January 1, 2020.

My Shiny Application will provide a simple, interactive format for exploring this dataset. Fatal Encounters has done a fabulous job gathering this data, keeping it publically available, and creating a simple search engine for specific cases. The purpose of my application is to make the information from the database more accessible to the general public, not just data scientists and statisticians.

## 2. Details for Professors

So far, my Shiny application has three options for interactive visualization:

1. Deaths over time, across different groups: This panel looks at the number of deaths per 100k individuals over time, and has the option to be separated by gender or race.
2. Distributions of different groups: This panel looks at the distribution of racial groups and genders in US Population compared to their distributions in individuals killed.
3. Deaths across states, over time: This panel has a choropleth of the United States, colored by death rates. This is animated over time, moving year by year, and a line graph of total deaths is built as the animation progresses.

Of these, I'm most proud of the animated choropleth/line plot combination.

## 3. Other Skills

I used a lot of skills from the visualizations week of this class, including creating a choropleth and customizing the theme of my ggplots. I also added upon what we learned, by including multiple colors in the title of a plot with the `grid`, animating hover text with `plotly`, and animating plots over time with an animated slider input.

## 4. Permission to share

I give permission to share this project publicly, with my name attached to it. 