A version of the Model_Main pension simulation code (found here https://github.com/marshallpku/Model_Main) adapted to Vermont actuarial data.  As of July 2 the Vermont data has not yet been added.  Initial data will be for the Vermont State Employees Retirement System (VSERS), with the Vermont Municipal Employee Retirement System (VMERS) and Vermont State Teachers Retirement System (VSTRS) versions being added later.  The goal is to provide stochastic simulations of funded ratio and contribution levels under different structure, economic and asset allocation scenarios.

The "Reproducibility Guide" *should* allow for almost all of the original R files to be run and for the original output to be generated.  Some edits to this and the underlying files were made to allow for the code to run on Linux Mint 18 and the latest versions of R and RStudio.

As of July 2, 2018 the implementation for Vermont data should be considered *draft/work-in-progress/subject to correction or revision* unless otherwise noted and the analysis is not part of any State of Vermont initiative.

The following files have been changed from the existing Model_Main reference code:

Functions.R -> to use "ungroup(df_risk)" in join_all
Model_Master.R -> to save more variables
Model_RunControl_reprod_M1.R
Model_RunControl_reprod_M2.1.R
IO_M1_new/Analysis_Outputs_M1_new.Rmd
IO_M2.1_new/Analysis _Outputs_M2.1_new.Rmd
IO_M2.1_new/Report_M2.1a.R
IO_M2.1_new/Report_M2.1b.R

Other files have also been changed in an effort to be able to replicate output that has been included in the reference code:

IO_M1_new/trade-off.R
IO_M2.1_new/Analysis_Ben_Risk.R
IO_M2.1_new/Analysis_Demo_Loading.R
IO_M2.1_new/Analysis_Demo_all.R
IO_M2.1_new/Analysis_Demo_report.R
IO_M2.1_new/Analysis_M2.1b_MFC2017.R
IO_M2.1_new/Results_Measures.R

Note that in the spreadsheets controlling which scenarios are run, currently *all* scenarios are selected.  So plan on the "reprod" RunControl files to take awhile to execute.

There may still be output legacy output files or .R files included here.  An effort will be made to streamline this repository as much as possible going forward.
