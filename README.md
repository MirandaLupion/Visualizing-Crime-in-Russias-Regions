# Crime Data in the Russian Regions From 1990 to 2010

### Summary

This Shiny App displays data about crimes and road accidents in the Russian Federation's federal subjects (administrative units) from 1990 through 2010. Users can visualize the data interactively in a table, scatterplot, and map. I hope that, through this data exploration, users will realize that Russia is as geographically diverse as the United States. The data also helps communicate changes in crime trends from the tumultuous 1990s period through President Vladimir Putin's first two terms and the first half of Dmitry Medvedev's presidency. 

### Background

The application includes data on 82 administrative units of six types. Oblasts and Krais are the most common units and are similar to U.S. states with a governor and regional legislature. Republics (22 total) have greater autonomy and were traditionally associated with a non-Russian titular nationality (i.e. Tatarstan for Tatars, Chechnya for Chechens). The Federal cities of Moscow and Petersburg function as separate regions. Autonomous okrugs and oblasts typically house or housed a subtantial ethnic minority. For a deeper explanation about the various administrative terms used to describe the units, see page 13 of [this report](://riate.cnrs.fr/wp-content/uploads/2015/03/M4D_20121220_TR_russia.pdf). 

Studying Russian crime data provides insight into the Russian Federation's 27 year trajectory. The dissolution of the Soviet Union in 1991 precipitated a decade of instability, as the weak state passed painful economic reforms (known as "shock therapy") and struggled to keep crime in check. The rapid transition from a planned economy to a system based on free markets severely depressed the standard of living in Russia. Unemployment increased from 9.4 percent in 1995 to 13 percent by 1998. As demand for labor plummeted, output declined by 45 percent from 1989 to 1998. Inflation peaked at 84 percent in 1999, eroding Russians' savings. Revenues from tax collection fell by 20 percent, leading the government to cut welfare spending. The economic instability correlated with (and perhaps caused - the jury is still out) a spike in organized crime. By 1995, Russia’s murder rate stood at 30 deaths per 100,000 people, compared to one to two deaths per 1000,000 in Western Europe. Only South Africa and Colombia had higher figures. Analysts attribute the current regime's historically high approval rating with the restoration of order in post-Soviet Russia. As you cycle through the data in this app, see if the numbers corrorborate this narrative. 

For more on organized crime in Russia in the 1990s and Putin-era reforms, see [this piece from The Economist](https://www.economist.com/leaders/2009/01/22/mass-murder-and-the-market,) and [this Stratfor report](https://worldview.stratfor.com/article/organized-crime-russia).

### Shiny

Please note that this app cannot be deployed on my Shiny account; the repeatred rendering of the map using a detailed shapefile is taxing, and my free Shiny account runs out of memory. I offered to purchase more memory to deploy the app, but the Preceptor said that was not necessary, as long as it displayed in the RStudio HTML viewer.

### Data Sources

Inter-university Consortium for Political and Social Research (ICPSR)
ICPSR 35355 Aggregate Data, Regions of Russia (RoR), 1990 - 2010, created by Irina Mirkina
To learn more: https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35355

Diva GIS
Polygon shapefile for Russia's first-level administrative boundaries (RUS_adm1.shp and associated files)
To learn more: http://www.diva-gis.org/
