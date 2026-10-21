Updated every quarter by Angelina Bruno (will need to nominate a new person to do this after December 15).

Run '1. Refresh all data.R'. This script links to 8 other R scripts and 8 folders where the graphs are saved.

Run '2. Australia Chair Tool.qmd' collates and saves Tool in html file.

'0. Exchange rates.R' is run by the chart scripts themselves (no need to open it). It downloads the RBA's daily A$/US$ rates so every chart in dollars is saved twice, in A$ (name.png) and US$ (name_usd.png), for the A$/US$ toggle in the dashboard's top bar. Each value is converted at the average rate for its own period.

Data sources and links are saved in https://csis365.sharepoint.com/:x:/s/Australia/IQDrSA6EMKEQTaaFrVvdDzzkARJpelangI6YcqL9G_NOScE?e=dy4MO5
