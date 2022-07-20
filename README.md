# MLB Starting Pitcher Stats
A script written for a client that takes team level game data and player level data to generate statistics for starting pitchers. Most ofthe clients desired statistics 
were based around a startging pitcher's last two starts. For instance strike outs per batters faces over the last two outings, or pitches thown compared to days off 
between the last two outings.

The only non-base packages used in this script are the [`data.table`](https://github.com/Rdatatable/data.table) and [`stringi`](https://github.com/gagolews/stringi) 
packages. The packages and data are loaded, then the two tables are formatted to make them easier to work with. The formatting process includes: 
- Setting column names to lower case
- Making sure pitcher names are the same case between the two tables so they can be cross referenced
- Removing any duplicate rows
- Adding columns to make life easier... here I added `seasons` column to the player data 

The section of the code reorganizes the data so all the calculations can be done column-wise. R is really good at vectorized operations like this, so this kind of 
approach is always better than for loops. 

