# Problem Set 3
Due 1 Mar, 2025


## Instructions
Generate the requested plots to the best of your ability. Be sure to `knit` the document to `.pdf` format. `echo` all code chunks as an appendix. you must show every step you took and option you specified to generate the plots. Post your completed work in a repository on your github account on or before March 1. 


## Suppressing Black Votes  
Keele et al (2021) evaluate the impact of efforts to disenfranchise African American voters in Louisiana in the 1950s and 1960s. They focus specifically on the impact of the Understanding Clause, which only some parishes administered. 

You need the following materials:

  - KEELE L, CUBBISON W, WHITE I. (2021). Suppressing Black Votes: A Historical Case Study of Voting Restrictions in Louisiana. American Political Science Review. <https://doi.org/10.1017/S0003055421000034>.   

  - `la_turnout_basic.dta` replication data, available in this repository. Hint: this is a parish-by-year dataset while the plot you need to create is a treatment group-by-year dataset. You'll need to transform the data to get there. You may even need to clean the `understandingclause2` variable which equals 1 for a parish that used the Understanding Clause (Treated) and 0 for a parish that did not (Control).  

Your task is to replicate, and then improve on, Figure 2. Be sure to choose an appropriate size for the visuals in your final output. 

  1. Replicate Figure 2 from the article. Match *every* element of the plot. Note that for this figure, they create two separate graphs and paste them together. You will need to do the same with `patchwork`.  

  2. Improve Figure 2. I'm asking for several changes:   
    - Set the vertical axes to range from 0-1.   
    - Remove the gridlines from the plot.   
    - Add a lightly shaded area denoting the use of the Understanding Clause (1954 to 1965 according to Justice Department investigations).  
    - Use faceting to create two subplots instead of two separate plots. Hint: you will need to pivot to make this work.  
    - Improve the visibility of the lines/shapes as you see fit.  
