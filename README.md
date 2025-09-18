# Scorecard
LIBNAME MYSAS '/home/u64149066/sasuser.v94/MM711/MYSAS';  
PROC IMPORT 
DATAFILE="/home/u64149066/sasuser.v94/MM711/MYSAS/loans_dataset.xlsx" 
OUT=MYSAS.loan 
DBMS=XLSX  
REPLACE; 
SHEET="CreditRisk"; 
GETNAMES=YES; 
RUN; 
Libname mysas '/home/u64149066/sasuser.v94/MM711/MYSAS'; 
proc sort data=mysas.loan; 
by Default_status;  
run; 
/* Dataset contains exactly 1000 rows, so N=1000 does not reduce the data. 
This step applies stratified randomization only — full data is used. */ 
proc surveyselect data = mysas.loan out = mysas.loan_training 
method = srs N=1000 /*samprate =0.8*/ seed = 12345;  
strata Default_status / alloc=proportional; 
run; 
/* the same % of Bad/Good clients in both data sets*/ 
Title 'LOAN total data'; 
proc freq data=mysas.loan; 
tables Default_status; 
quit; 
Title 'LOAN training data'; 
proc freq data=mysas.loan_training; 
tables Default_status; 
quit; 
PROC MEANS DATA=MYSAS.loan_training_for_model NMISS; 
VAR _ALL_; /* Checks all variables in the dataset */ 
OUTPUT OUT=Missing_Values_Report NMISS=; 
RUN; 
proc means data=mysas.loan_training_for_model n nmiss; 
run; 
PROC PRINT DATA=Missing_Values_Report; 
VAR  Age Net_Income Emp_Years Home_Ownership Default_status Debt_Inc_Ratio and 
Loan_Duration;  
TITLE "Number of Missing Values Per Variable"; 
RUN; 
PROC CONTENTS DATA=MYSAS.loan; 
RUN; 
PROC CONTENTS DATA=MYSAS.loan_training_for_model; 
RUN; 
PROC PRINT DATA=MYSAS.loan_training_for_model (OBS=5); 
RUN; 
PROC MEANS DATA=MYSAS.loan_training_for_model NMISS MAXDEC=0 NOPRINT; 
OUTPUT OUT=Missing_Values_Report NMISS=; 
RUN; 
PROC PRINT DATA=Missing_Values_Report; 
RUN; 
PROC MEANS DATA=MYSAS.loan_training_for_model NMISS NOPRINT; 
OUTPUT OUT=Missing_Values_Report NMISS=; 
RUN; 
PROC TRANSPOSE DATA=Missing_Values_Report OUT=Missing_Transposed; 
RUN; 
PROC PRINT DATA=Missing_Transposed; 
ID _NAME_; 
VAR COL1;   
TITLE "MISSING VALUES"; 
RUN; 
/*  drop the IDClient variable from the training set for modeling */ 
data mysas.loan_training_for_model; 
set mysas.loan_training; 
drop IDClient; /* Exclude the unique identifier */ 
run; 
/* Verification of proportions (using the _training dataset as it still has IDClient for 
comparison if needed) */ 
Title 'LOAN total data - Default Status Distribution'; 
proc freq data=mysas.loan; 
tables Default_status; 
quit; 
Title 'LOAN training data - Default Status Distribution'; 
proc freq data=mysas.loan_training; 
tables Default_status; 
quit; 
ods graphics on; /* ODS Graphics for plots */ 
/*******************************Numerical Data 
Exploration*******************************/ 
/* Univariate distributions for selected numerical variables */ 
proc univariate data=mysas.loan_training_for_model; 
VAR Net_Income; 
CDFPLOT 
Net_Income; 
HISTOGRAM  Net_Income; 
run; 
proc univariate data=mysas.loan_training_for_model; 
VAR Age; 
CDFPLOT 
Age; 
HISTOGRAM  Age; 
run; 
proc univariate data=mysas.loan_training_for_model; 
VAR Loan_Duration; 
CDFPLOT 
Loan_Duration; 
HISTOGRAM  Loan_Duration; 
run; 
proc univariate data=mysas.loan_training_for_model; 
VAR Debt_Inc_Ratio;  
CDFPLOT 
Debt_Inc_Ratio; 
HISTOGRAM  Debt_Inc_Ratio; 
run; 
/* Descriptive statistics for all numerical variables */ 
proc means data=mysas.loan_training_for_model 
N MEAN MEDIAN MODE P1 P99 MAXDEC=4; 
var Age Net_Income Emp_Years Debt_Inc_Ratio Loan_Duration;  
run; 
/* QQ-Plots for normality assessment */ 
proc univariate data=mysas.loan_training_for_model noprint; 
QQPLOT Net_Income /NORMAL(MU=EST SIGMA=EST COLOR=LTGREY); 
run; 
proc univariate data=mysas.loan_training_for_model noprint; 
QQPLOT Age /NORMAL(MU=EST SIGMA=EST COLOR=LTGREY); 
run; 
/*******************************Categorical Data 
Exploration*******************************/ 
/* Frequency tables for categorical variables */ 
proc freq data=mysas.loan_training_for_model; 
tables Home_Ownership Default_status; /* Default_status is also categorical*/ 
quit; 
/* Scatter plot for two numerical variables */ 
proc gplot data=mysas.loan_training_for_model; 
plot Net_Income*Age; /* Example: Relationship between Net_Income and Age */ 
run; 
/**************************************************************************
 ***** 
* * 
* STEP 4: MACROS FOR REUSABLE EDA COMPONENTS                         
* * 
* 
***************************************************************************
 ****/ 
/************* MACRO: HISTOGRAMS WITH CLASS STATEMENT 
***********************/ 
%macro hist(var_x=); 
proc univariate data=mysas.loan_training_for_model; 
class Default_status; /* Stratify histograms by Default_status */ 
var &var_x.; 
histogram &var_x. / nrows=2 odstitle="PROC UNIVARIATE with CLASS statement for 
&var_x."; 
ods select histogram; /* display only the histograms */ 
run; 
%mend; 
%hist(var_x=Net_Income); 
%hist(var_x=Age); 
%hist(var_x=Emp_Years); 
%hist(var_x=Debt_Inc_Ratio);  
%hist(var_x=Loan_Duration); 
 
/************* MACRO: DESCRIPTIVE STATISTICS TABLE 
***********************/ 
 
%Macro DescripStats(VarX=,n=); 
 
PROC UNIVARIATE Noprint DATA=mysas.loan_training_for_model PLOTS; 
     VAR &VarX.; 
     Histogram / Cfill=Blue Outhist = HistOut&n.; 
     OUTPUT OUT=Stat&n. NMISS=NMISS 
NOBS=NOBS PCTLPTS =2.5 97.5 PCTLPRE=P 
     MEAN=MEAN MODE=MODE 
MEDIAN=MEDIAN 
     Q1=Q1 Q3=Q3 P5=P5 P10=P10 P90=P90 P95=P95 
STD=STD 
     MAX=MAX MIN=MIN KURTOSIS=KURTOSIS 
SKEWNESS=SKEWNESS; 
   RUN; 
 
   DATA Stat&n.; 
    FORMAT Name $32. NOBS NMISS KURTOSIS 
SKEWNESS MEAN MODE STD MIN P2_5 P5 P10 Q1 MEDIAN Q3 P90 P95 P97_5 MAX 
BEST12.; 
    SET Stat&n.; 
                 Name="&VarX"; 
                  
            RUN; 
 
 Proc Append base=Summary_STAT data=Stat&n. force; Run; 
%mend DescripStats; 
 
/* Call macro for all numerical variables */ 
%DescripStats(VarX=Age,n=1); 
%DescripStats(VarX=Net_Income,n=2); 
%DescripStats(VarX=Emp_Years,n=3); 
%DescripStats(VarX=Debt_Inc_Ratio,n=4); 
%DescripStats(VarX=Loan_Duration,n=5); 
/************* MACRO: FREQUENCY TABLES FOR CATEGORICAL VARIABLES 
(USING PROC SQL) ***********************/ 
%macro freq_categorical (VarX=,n=); 
/* Use PROC FREQ to get counts and percentages */ 
proc freq data=mysas.loan_training_for_model noprint; 
tables &VarX. / out=FreqRaw&n NOCUM; 
run; 
data FreqTemp&n.; 
set FreqRaw&n; 
length Var $50 Class $100; 
Var = "&VarX.";  
Class = vvaluex("&VarX.");  
   
  N = COUNT; 
  Total = sum(COUNT);  
  percentage = PERCENT/100; 
 
   
  keep Var Class N Total percentage; 
  format Var $50. Class $100. N Best10. Total Best10. percentage percent10.2; 
 run; 
 
  
 proc sql; 
  %if %sysfunc(exist(UNIVAR_CLASS_1)) %then %do; 
   insert into UNIVAR_CLASS_1 
   select Var, Class, N, Total, percentage 
   from FreqTemp&n.; 
  %end; 
  %else %do; 
   create table UNIVAR_CLASS_1 as 
   select Var, Class, N, Total, percentage 
   from FreqTemp&n.; 
  %end; 
 quit; 
 
 /* Clean up temporary datasets */ 
 proc datasets library=work nodetails nolist; 
  delete FreqRaw&n FreqTemp&n; 
 quit; 
  
%mend freq_categorical; 
 
/* Call macro for categorical variables */ 
%freq_categorical(VarX=Home_Ownership,n=1); 
%freq_categorical(VarX=Default_status,n=2); 
Data mySAS.LOAN_training_WOE; 
Set mySAS.LOAN_training; 
/* Age */ 
if age <= 31 then woe_age = -0.199415990153255; 
else if 32 <= age <= 45 then woe_age = 0.12721302409545; 
else if 46 <= age <= 75 then woe_age = 0.19074012725955; 
/* Net Income */ 
if net_income <= 24171 then woe_net_income = -0.238173634719848; 
else if 24234 <= net_income <= 32758 then woe_net_income = 0.266020575670601; 
else if 32786 <= net_income <= 223300 then woe_net_income = 0.543615446588982; 
/* Employment Years */ 
if emp_years <= 3 then woe_emp_years = -0.418480476220235; 
else if 4 <= emp_years <= 7 then woe_emp_years = -0.0224728558520586; 
else if 8 <= emp_years <= 10 then woe_emp_years = 0.312374685042152; 
else if emp_years >= 11 or emp_years = 9999 or missing(emp_years) then woe_emp_years 
= 0.484557696945381; 
/* Debt-to-Income Ratio */ 
if debt_inc_ratio <= 0.326 then woe_debt_inc_ratio = -0.259338543550954; 
else if 0.327 <= debt_inc_ratio <= 0.506 then woe_debt_inc_ratio = 0.12350476778123; 
else if 0.508 <= debt_inc_ratio <= 0.68 then woe_debt_inc_ratio = 0.421213465076303; 
else if 0.681 <= debt_inc_ratio <= 0.842 then woe_debt_inc_ratio = 0.733969175080201; 
/* Loan Duration */ 
if loan_duration <= 3 then woe_loan_duration = 0.27286698666664; 
else if 4 <= loan_duration <= 5 then woe_loan_duration = 0.422601390351152; 
else if loan_duration = 6 then woe_loan_duration = 0.00921665510492405; 
else if 7 <= loan_duration <= 8 then woe_loan_duration = -0.339283201226863; 
else if 9 <= loan_duration <= 10 then woe_loan_duration = -0.0357180826020792; 
/* Home Ownership */ 
if home_ownership = 'Renter' then woe_home_ownership = -0.430245137; 
else if home_ownership = 'Other' then woe_home_ownership = -0.221036069; 
else if home_ownership = 'Owner' then woe_home_ownership = 1.2075378705; 
RUN; 
%Let input_table=mysas.loan_training;  
/*Continuous variables*/ 
%Macro BivariateCont(VarX=,n=); 
/* Handle missing values for the continuous variable by replacing them with 9999. */ 
/* This before clustering to ensure all observations are included. */ 
data &input_table.; 
set &input_table.; 
if &VarX.=. then &VarX.=9999; 
run; 
proc sort data=&input_table.; 
by &VarX.; 
run; 
 
proc fastclus noprint data=&input_table. out=cont_clust_&n. /*converge=0*/ maxclusters=12 
/*MAXITER=200 REPLACE=FULL*/ nomiss; 
      var &VarX.; 
run; 
 
 
data cont_clust_&n.; 
 set cont_clust_&n. (keep=&VarX. Cluster Default_status);  
run; 
 
Proc Sql NOPRINT;create table Report_ContClust_&n. as 
         Select &VarX. as Var, 
         Cluster, 
      Default_status,  
         Count(*)   as Total, 
        sum(Default_status=1)   as 
Total_Bads,   
                 sum(Default_status=0 ) as Total_Goods  
     from  cont_clust_&n. 
; 
Quit; 
 
/* Create the final summary report for the continuous variable's clusters. */ 
Proc Sql NOPRINT;create table SUM_Report_ContFinal_&n. as 
         Select Cluster, 
        Total, 
        Total_Bads, 
        Total_Goods, 
         Count(*)   as N_Class, 
      Min(Var) as Min ,  /* Minimum value of the 
continuous variable in the cluster */ 
        Max(Var) as Max ,  /* Maximum value of 
the continuous variable in the cluster */ 
        sum(Default_status=1)/Total_Bads*100  as 
PCT_B   format=5.2 ,  
                          sum(Default_status=0)/Total_Goods*100  as PCT_G   format=5.2,  
            sum(Default_status=1)   as Bads,   
                 sum(Default_status=0 ) as Goods,  
     
   log((sum(Default_status=0)/Total_Goods)/(sum(Default_status=1)/Total_Bads))  as 
WOE,  
                         (calculated PCT_G - calculated PCT_B)*(calculated Woe)   as IVi, 
       (calculated N_Class)/total*100 as perct_obs 
     from Report_ContClust_&n. 
         Group by Cluster 
         ; 
     Quit; 
 
/* Sort the final report by the minimum value of the cluster for ordered analysis. */ 
proc sort data=SUM_Report_ContFinal_&n. /*out=Report_clust_nd&n.*/ nodupkey; 
by Min; 
quit; 
 
 
%mend BivariateCont; 
 
 
%BivariateCont(VarX=Age,n=1); 
%BivariateCont(VarX=Net_Income,n=2); 
%BivariateCont(VarX=Emp_Years,n=3); 
%BivariateCont(VarX=Debt_Inc_Ratio,n=4); 
%BivariateCont(VarX=Loan_Duration,n=5); 
 
 
%Let input_table=mysas.loan_training; 
 
/*Categorial variables*/ 
%Macro  BivariateCategorical(VarX=,n=); 
 
Proc Sql NOPRINT;create table Report_Bivar_&n. as 
         Select &VarX. as Var, 
         Default_status, 
         Count(* )   as Total, 
        sum(Default_status=1)   as 
Total_Bads,  
                 sum(Default_status=0 ) as Total_Goods 
     from  &input_table.; 
; 
Quit; 
 
Proc Sql NOPRINT;create table SUM_ReportCat_Final_&n. as 
         Select Var, 
        Total, 
        Total_Bads, 
        Total_Goods, 
         Count(* )   as N_Class, 
        sum(Default_status=1)/Total_Bads*100  as 
PCT_B   format=5.2 , 
                          sum(Default_status=0)/Total_Goods*100  as PCT_G   format=5.2, 
            sum(Default_status=1)   as Bads,  
                 sum(Default_status=0 ) as Goods, 
        
log((sum(Default_status=0)/Total_Goods)/(sum(Default_status=1)/Total_Bads))  as WOE, 
                         ((calculated PCT_G/100) - (calculated PCT_B/100))*(calculated Woe)   as 
IVi, 
       (calculated N_Class)/total*100 as perct_obs 
     from   Report_Bivar_&n.  
     group by Var 
     ; 
     Quit; 
 
proc sort data=SUM_ReportCat_Final_&n. /*out=Report_clust_nd&n.*/ nodupkey; 
by WoE; 
quit; 
 
%mend BivariateCategorical; 
%BivariateCategorical(VarX=home_ownership,n=1); 
 
Data mySAS.LOAN_training_WOE; 
  Set mySAS.LOAN_training; 
 
  /* Age */ 
  if age <= 30 then woe_age = -0.199415990153255; 
  else if 31 < age <= 45 then woe_age = 0.12721302409545; 
  else if 46 <= age <= 75 then woe_age = 0.19074012725955; 
 
  /* Net Income */ 
  if net_income <= 24171 then woe_net_income = -0.238173634719848; 
  else if 24234 <= net_income <= 32758 then woe_net_income = 0.266020575670601; 
  else if 32786 <= net_income <= 223300 then woe_net_income = 0.543615446588982; 
 
  /* Employment Years */ 
  if emp_years <= 3 then woe_emp_years = -0.418480476220235; 
  else if 4 <= emp_years <= 7 then woe_emp_years = -0.0224728558520586; 
else if 8 <= emp_years <= 10 then woe_emp_years = 0.312374685042152; 
else if emp_years >= 11 or emp_years = 9999 or missing(emp_years) then woe_emp_years 
= 0.484557696945381; 
/* Debt-to-Income Ratio */ 
if debt_inc_ratio <= 0.326 then woe_debt_inc_ratio = -0.259338543550954; 
else if 0.327 <= debt_inc_ratio <= 0.506 then woe_debt_inc_ratio = 0.12350476778123; 
else if 0.508 <= debt_inc_ratio <= 0.68 then woe_debt_inc_ratio = 0.421213465076303; 
else if 0.681 <= debt_inc_ratio <= 0.842 then woe_debt_inc_ratio = 0.733969175080201; 
/* Loan Duration */ 
if loan_duration <= 3 then woe_loan_duration = 0.27286698666664; 
else if 4 <= loan_duration <= 5 then woe_loan_duration = 0.422601390351152; 
else if loan_duration = 6 then woe_loan_duration = 0.00921665510492405; 
else if 7 <= loan_duration <= 8 then woe_loan_duration = -0.339283201226863; 
else if 9 <= loan_duration <= 10 then woe_loan_duration = -0.0357180826020792; 
/* Home Ownership */ 
if home_ownership = 'Renter' then woe_home_ownership = -0.430245137; 
else if home_ownership = 'Other' then woe_home_ownership = -0.221036069; 
else if home_ownership = 'Owner' then woe_home_ownership = 1.2075378705; 
RUN; 
TITLE; 
TITLE1 "Correlation Analysis"; 
FOOTNOTE; 
FOOTNOTE1 ; 
PROC CORR DATA=mysas.loan_training_WOE 
PLOTS=NONE 
PEARSON 
OUTP=Corr_logit 
VARDEF=DF; 
VAR WoE_home_ownership WoE_age WoE_emp_years WoE_net_income 
WoE_loan_duration WoE_debt_inc_ratio; 
RUN; 
proc surveyselect data=mysas.loan_training_woe 
out=loan_woe_split 
samprate=0.7 
outall 
seed=123; 
run; 
data train test; 
set loan_woe_split; 
if selected=1 then output train;  
else output test; 
run; 
ODS GRAPHICS ON; 
TITLE; 
TITLE1 "Logistic Regression - Train"; 
FOOTNOTE; 
FOOTNOTE1 "scoring models"; 
PROC LOGISTIC DATA=train DESCENDING 
PLOTS(ONLY)=ALL 
OUTMODEL=logit_model;  
MODEL Default_status =  
WoE_home_ownership  
WoE_age  
WoE_emp_years  
WoE_net_income  
WoE_loan_duration  
WoE_debt_inc_ratio 
/  
OUTROC=ROC 
SELECTION=STEPWISE 
SLE=0.1 
SLS=0.1 
INCLUDE=0 
CORRB 
CTABLE 
PPROB=(0.5) 
SCALE=PEARSON 
RSQUARE 
LACKFIT 
LINK=LOGIT 
CLPARM=WALD 
CLODDS=WALD 
ALPHA=0.05; 
ODS OUTPUT ParameterEstimates=Beta; 
ODS OUTPUT Association=STAT_TABLE; 
OUTPUT OUT=Train_Predict  
PREDPROB=(INDIVIDUAL) 
XBETA=xbeta__Target; 
RUN; 
PROC LOGISTIC INMODEL=logit_model; 
SCORE DATA=test OUT=Test_Predict; 
RUN; 
