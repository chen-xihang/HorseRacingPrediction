# Horse Racing Win Probability Project

This project uses historical horse racing data to estimate each horse’s probability of winning a race. It builds simple performance-based ratings (for horses, jockeys, and trainers), fits a logistic regression model, and compares its predictions to the Betfair Starting Price to check whether a value-betting edge exists.

The dataset contains one row per horse per race, with columns such as:

`race_id`, `race_type_simple`, `race_distance`

`horse_id`, `age`, `carried_weight`, `draw`

`obs__is_winner`, `obs__racing_post_rating`, `obs__bsp`

The full workflow is in .rmd file.
