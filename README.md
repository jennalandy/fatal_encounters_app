# Data

Most of the data comes from the [Fatal Encounters](https://fatalencounters.org/) project, which was founded on the premise that Americans should have the ability to track circumstances where police use deadly force, even though these circumstances are not tracked by any governmental organization. This project, run by D. Brian Burghart, has built a database of all deaths through police interaction in the United States since January 1, 2020.

Estimated proportions of the US population that are in each racial and gnder group were found on the [Census QuickFacts Page](https://www.census.gov/quickfacts/fact/table/US/IPE120218). Transgender individuals are not counted as such in the United States Census, so this proportion was found separately from the [Wikipedia page on LGBT demographics of the United States](https://en.wikipedia.org/wiki/LGBT_demographics_of_the_United_States#:~:text=A%20different%20survey%20in%202016,adult%20population%20identifying%20as%20LGBT.).

# Note for Professors

I had trouble thinking of a good way to incorporate data anlaysis in this application, so it focuses primarily on exploration. I wanted to include some analysis, even if it was minimal, so these are the small analyses performed and where you can see them in the app:

- A 95% confidence interval for the increase in overall number of deaths per year, as seen under the plot over time in the "plots over time" tab.
- Two sample z tests for proportions were used in the "proportions" tab to compare the proportion of the group in question within individuals killed to those in the US population. The result of these test are reported under the proportions plot.
