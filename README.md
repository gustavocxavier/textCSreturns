
# textCSreturns

<!-- badges: start -->
<!-- badges: end -->

The goal of textCSreturns is to predict cross-sectional stock returns using
text data.

The current code predicts the expected investment growth (EIG) of
Hou, Moe, Xue, and Zhang (2021), which is the change in investment-to-assets.

In other words, it is :
  
  [(A_{FY+1} - A_{FY}) / A_{FY}] - [(A_{FY} - A_{FY-1}) / A_{FY-1}] - ,

where A_{FY} is the total assets of the firm in the current fiscal year.

To execute it is necessary to adjust the path of the data files
(DT_AllSample.csv and the rds) available under request.