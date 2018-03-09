
## Benefactors: - Leeds Cicy Council - 'Users' interested in footfall ... ## Aims:

 - **Essential**: Develop a web tool (interface to a model), capable of allowing people to:   - Allow the user to look back at past events, see what the actual footfall was, and what our model predicted it would be under normal conditions.   - Allow the user to ask for estimates of future footfall (they will need to enter all of the required inputs (e.g. temp, rain, etc.)). - **Would be nice**:
    - See if a Bayesian method preforms better than random forest
    - The model automatically scrapes new data and retrains itself to make better future predictions## TODO

 - [ ] Collect up-to-date version of the input data and make a new input data file

 - [ ] Re-implement the Random Forest model in R

 - [ ] Check that the new model makes similar estimates to the Python one

 - Create an R Shiny app capable of:
  - [ ] Allowing the the user to run the model on historical data to see what the real footfall v.s. prediction was (e.g. a `Retrospective Analysis` tab).
  - [ ] Allowing the user to use the model to make predictions of the future footfall, given their estimates of the weather conditions etc. (e.g. a `Prospective Analysis` tab).

**Others**

 - [ ] Document everything properly for the website
 