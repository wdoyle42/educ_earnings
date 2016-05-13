# Replication files

To replicate the tables in the paper and online appendix:

1. Place `Location_R15.csv` from NLSY97 restricted data in the `./data` subdirectory.
2. Run the `geocode.R` script in R.
3. Run the `iv_estimation.do` in Stata.
4. Knit the tables using `tables.Rnw`, which is found in the `./tables` subdirectory.