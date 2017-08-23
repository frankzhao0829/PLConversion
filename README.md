# R code files:
1. plc_getData.r: this file contains codes that fetch data sets from Vertica database and format changes;

2. plc_lead_tree.r: this file contains codes that were used to simulate the daily use of PLC. Account attributes are not incorporated in the model in this file because the data sets contain leads with and without account information;

3. plc_lead_tree_with_account.r: this file was used to test if account attributes increase predictability;

4. plc_lead_tree_with_social.r: this file was used to test if product Twitter information enhances the predictability;

5. myRfunctions_plc.r: auxiliary functions used in the above R files 

RData files:
the data sets used (should be recognizable by names and codes in file plc_getData.r)

(in the files, "current"/"now" means "on April 22, 2014")
