# Predicting MLB game winners based on past performance ⚾

## Goal
Predicting win/loss at MLB baseball games  
How I will get there:
Using data from Baseballreference, MLB API and other sources (collected using baseballr R package), I will train ML models that look at past team results and pitcher performance to try to determine to the best extent possible the winners from each day's MLB games.
As it is a difficult problem to solve, I'd be very satisfied achieving 75% accuracy on a day-to-day basis. Early models show 57 to 65% accuracy so far, which is not terrible.

## Done  

- [x] Scraping data using baseballr R package  
- [x] Creating initial models using h2o AutoML
- [ ] Generate predictions on-demand from 

## To do

- [ ] Analyze correlations of features with win/loss
- [ ] Combining standings, past results and pitchers data
- [ ] Creating automated script to run every day and collect new data

## Ideas
Assign bigger weights to recent matches?
When splitting train/test datasets, sample a few games from each day? Or hide the last n days from the model?
What's the procedure on refining and finalizing models after showing test data to the model? 
Automatically remove features that don't improve the models' performance 

