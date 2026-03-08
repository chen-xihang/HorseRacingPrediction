## Overall summary
This project develops a race-level win probability model for horse racing using 
a conditional logit framework that accounts for the competitive structure within
each race. The model incorporates performance ratings for horses, jockeys, and
trainers, alongside race-level features (distance, going, field size, race type),
and tests whether these variables improve prediction after accounting for ltp_5min.
Performance is evaluated out-of-sample using probabilistic metrics (log loss, 
Brier score, calibration) and simulated betting returns with confidence intervals,
with a focus on economically meaningful residual edge.