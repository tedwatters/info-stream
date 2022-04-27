## Introduction
Project for:
* JHU EN 625.692 Probabilistic Graphical Models
* JHU EN 625.664 Computational Statistics

## Required Software
NetLogo 6.2.2
R

## Implementation Notes
Because of the size and fluid nature of the data files, they are not contained within the repo. To generate them:
1. Run `market.nlogo` in NetLogo.
2. Run `chaingraph.R`

Other notes:
* I wrote this for Ubuntu, and have harded coded some relative file paths. You might need to change the slash direction on file paths if you're having trouble running on Windows
* I've included a `renv.lock` in case you want to recreate the environment using https://rstudio.github.io/renv/articles/renv.html