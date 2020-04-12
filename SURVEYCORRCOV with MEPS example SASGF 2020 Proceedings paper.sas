libname dat /*PERSONALIZE YOUR LOCATION OF MEPS DATA SET H192 HERE*/ "C:\DATA";
/********************************************************************************************
								PREPARE DATASET FOR ANALYSIS
********************************************************************************************/
data a;
	set dat.h192; 

     logDental=log(DVTEXP16+1);  /*  1801    DVTEXP16    Num       8    TOTAL DENTAL CARE EXP 16   */             
     logER    =log(ERTEXP16+1);  /*  1669    ERTEXP16    Num       8    TOTAL ER FACILITY + DR EXP 16   */        
     logHome  =log(HHAEXP16+HHNEXP16+1);  /*  1853    HHAEXP16    Num       8    TOTAL HOME HEALTH AGENCY EXP 16  */       
     logOffice=log(OBVEXP16+1);  /*  1331    OBVEXP16    Num       8    TOTAL OFFICE-BASED EXP 16        */       
     logOutPt =log(OPTEXP16+1);  /*  1467    OPTEXP16    Num       8    TOTAL OUTPATIENT FAC + DR EXP 16  */      
     logRX    =log(RXEXP16+1);   /*  1919    RXEXP16     Num       8    TOTAL RX-EXP 16                   */      
     logInPt  =log(IPTEXP16+1);  /*  1750    IPTEXP16    Num       8    TOT HOSP IP FACILITY + DR EXP 16   */     
     logOthSup=log(OTHEXP16+1);  /*  1903    OTHEXP16    Num       8    TOT OTHER EQUIP/SPLY (EXCL DIAB) EXP 16 */
     logVision=log(VISEXP16+1);  /* */

	 LogTotalExp=log(TOTEXP16+1);

	 if IPTEXP16=0 then ipZero100k=0;
	 	else if IPTEXP16>100000 then ipZero100k=1;
	 	else if IPTEXP16>. then ipZero100k=2;

/*  374    ADCAPE42    Num       4    SAQ 4WKS: FELT CALM/PEACEFUL SF-12V2    
    368    ADCLIM42    Num       4    SAQ: HLTH LIMITS CLIMBING STAIRS SF-12V2
    373    ADPAIN42    Num       4    SAQ 4WKS:PAIN LIMITS NORMAL WORK SF-12V2
    369    ADPALS42    Num       4    SAQ 4WKS:ACCMP LESS B/C PHY PRBS SF-12V2
    367    ADDAYA42    Num       4    SAQ: HLTH LIMITS MOD ACTIVITIES SF-12V2 
    376    ADDOWN42    Num       4    SAQ 4WKS: FELT DOWNHEARTED/DEPR SF-12V2 
    366    ADGENH42    Num       4    SAQ: HEALTH IN GENERAL SF-12V2          
    371    ADMALS42    Num       4    SAQ 4WKS:ACCMP LESS B/C MNT PRBS SF-12V2
    372    ADMWLM42    Num       4    SAQ 4WKS:WORK LIMT B/C MNT PROBS SF-12V2
    375    ADNRGY42    Num       4    SAQ 4WKS: HAD A LOT OF ENERGY SF-12V2   
    370    ADPWLM42    Num       4    SAQ 4WKS:WORK LIMT B/C PHY PROBS SF-12V2
    377    ADSOCA42    Num       4    SAQ 4WKS: HLTH STOPPED SOC ACTIV SF-12V2*/

if ADCAPE42>0 then ADCAPE42=6-ADCAPE42;
	else ADCAPE42=.;

if ADCLIM42<0 then ADCLIM42=.;

if ADDAYA42<0 then ADDAYA42=.;

if ADDOWN42<0 then ADDOWN42=.;

if ADGENH42>0 then ADGENH42=6-ADGENH42;
	else ADGENH42=.;

if ADMALS42<0 then ADMALS42=.;

if ADMWLM42<0 then ADMWLM42=.;

if ADPAIN42>0 then ADPAIN42=6-ADPAIN42;
	else ADPAIN42=.;

if ADPALS42<0 then ADPALS42=.;

if ADPWLM42<0 then ADPWLM42=.;

if ADNRGY42>0 then ADNRGY42=6-ADNRGY42;
	else ADNRGY42=.;

if ADSOCA42<0 then ADSOCA42=.;

/*only age 20-65 for DOMAIN*/

if 20<=AGE16X<=65 then include=1;
	else include=0;

HHEXP16=HHAEXP16+HHNEXP16;

keep dupersID PERWT16F SAQWT16F VARPSU VARSTR logDental logER logHome logOffice logOutPt 
	 logRX logInPt logOthSup logVision LogTotalExp DVTEXP16 ERTEXP16 HHEXP16 OBVEXP16 
	 OPTEXP16 RXEXP16 IPTEXP16 OTHEXP16 VISEXP16 ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42
	 ADMALS42 ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42 include sex AGE16X ipZero100k;
run;


***************************************************************;
*                                                             *;
*~1. File=SurveyCorrCov.sas                                   *;
*                                                             *;
*~2. %macro SurveyCorrCov(data=,id=,varlist=,nvar=,strata=,   *;
*	cluster=,weight=,domain=,subGrp=,outp=,outs=,outsscp=,    *;
*	outcov=,depend=,indep=,outp4reg=,surveyregoptions=,       *;
*	outppval=,outspval=,nomiss=,excelout=,excelfile=,listing=);*;       
*                                                             *;
*~3. This Macro calculates complex survey correlations        *;
*    The program provides the following information:          *;
*    1. Design-based (SURVEYREG) approach to create correlation,*;
*    covariance, and SSCP matrix for output to other PROCS    *;
*    2. Same approach to creating a correlation matrix after  *;
*    ranking observations within domain, for a non-parametric *;
*    approach                                                 *;
*    3. Output similar to PROC CORR using pairwise SURVEYREG  */
*    to create a matrix of correlations and p-values          *;
*    4. Output correlations and p-values to Excel files       *;
*    5. Macro SURVEYCORRCOV contains two macros: SURVEYREG and*;
*       ISBLANK	                                              *;
*                                                             *;
*~3. written by Siew Hoong Wong-Jacobson and David R Nelson   *;
*                                                             *;
*~4. Date: 12/APR/2020                                        *;
*                                                             *;
*~5. Macro Parameters:                                        *;
/*
DATA=(Required) SAS data set name
STRATA=For all SURVEY procs, the STRATA statement names variables that form the strata in a stratified sample design
CLUSTER=For all SURVEY procs, the CLUSTER statement names variables that identify the clusters in a clustered sample design
WEIGHT=For all SURVEY procs, the WEIGHT statement names the variable that contains the sampling weights
DOMAIN=(Required) In SAS SURVEY procs, the DOMAIN statement  names the variables that identify subgroups of interest, or "domains". 
	For this macro, the variable that identifies a subpopulation of interest for calculation of  the correlation, covariance, and/or SSCP.  
	For the entire sample to be included in the macro, create a variable and assign the same value, and use this variable as DOMAIN
SUBGRP=	(Required) The level of the DOMAIN statement to be included for analysis.  For instance, if the subgroup of analysis is defined 
	as include=1, then specify DOMAIN=include, and SUBGRP=1.  Do not use value -999 if NOMISS=1
VARLIST=	(Required unless only output is OUTP4REG) The variables comprising the CORR/COV/SSCP matrix, separated by spaces
EXCELOUT=	if EXCELOUT=1, then create excel output file with a tab for each data set specified in any of the OUT* parameters
EXCELFILE=	Path and filename for Excel file if EXCELOUT=1
SURVEYREGOPTIONS= Specify options as the would appear in the PROC SURVEYREG statement, such as missing value handling and variance 
	estimation methods.  For example, "SURVEYREGOPTIONS=nomcar varmethod=jackknife" specifies treating missing values as not missing 
	completely at random, and the Jackknife as variable estimation method.  If specified, all analyses will use these options
----Parameters for Correlations with p-values	
ID=	(Required for OUTPPVAL and OUTSPVAL) Identification variable, unique for each observation
NVAR=	(Required for OUTPPVAL and OUTSPVAL) Integer representing the number of varlist
OUTPPVAL=	Data set name for output data set with Pearson Product-Moment Correlation and their p-values
OUTSPVAL=	Data set name for output data set with within-domain rank-based correlation and their p-values
NOMISS=	For p-value matrices, similar option as PROC CORR.  NOMISS=1 excludes observations with any VARLIST missing values from the analysis.  
	Otherwise the correlations and p-values are from all available data.  NOTE, for all output data sets without p-values, NOMISS is used
LISTING=	If LISTING=1, then PROC PRINT will display OUTPPVAL and OUTSPVAL, if specified
-----Parameters for Correlations without p-values	
OUTP=	Similar to PROC CORR option; Data set name for output data set with Pearson Product-Moment Correlation, no p-values
OUTS=	Similar to PROC CORR; Data set name for output data set with within-domain rank-based correlation (Spearman’s correlation) , no p-values
OUTCOV=	Data set name for output data set with covariance matrix
OUTSSCP=	Data set name for output data set with sum of squares and cross products matrix
OUTP4REG=	Data set name for output data set with Pearson Product-Moment Correlation, no p-values.  N is adjusted by the design effect for each variable
DEPEND=	(Required for OUTP4REG) Dependent variable name that corresponds to SURVEYREG model statement before the "="
INDEP=	(Required for OUTP4REG) Independent varable names that correspond to SURVEYREG model statement after the "="
*/



%macro SurveyCorrCov(data=,id=,varlist=,nvar=,strata=,cluster=,weight=,domain=,subGrp=,outp=,
					 outs=,outsscp=,outcov=,depend=,indep=,outp4reg=,surveyregoptions=,
					 outppval=,outspval=,nomiss=,excelout=,excelfile=,listing=);

ods graphics off;
ods exclude all;

%macro isBlank(param);   
%sysevalf(%superq(param)=,boolean) 
%mend isBlank;

%if %isBlank(&outs)=0 or %isBlank(&outp)=0 or %isBlank(&outsscp)=0 or %isBlank(&outcov)=0 %then %do;
	DATA ZZ1;
		set &data;

		pseudo_y=_n_;
	run;

	ODS SELECT none;
	PROC SURVEYREG DATA=ZZ1 &surveyregoptions;
		MODEL pseudo_y= &varlist /XPX;
					   strata &strata; 
					   cluster &cluster; 
					   weight &weight; 
					   domain &domain;
					   ODS OUTPUT XPX=ZZ2;
	RUN;

	DATA ZZ2;
		SET ZZ2;

		IF DOMAIN="&domain.=&SubGrp.";

		rename parameter=_NAME_;

		_TYPE_="SSCP";

		drop  DOMAIN;
	run;

	%if %isBlank(&outsscp)=0 %then %do;
		DATA &outsscp;
			SET ZZ2;

			IF _NAME_=:"pseudo_y" then delete;

			drop  pseudo_y;
		run;
	%end;

	%if %isBlank(&outp)=0 %then %do;
		proc princomp data=ZZ2(TYPE=SSCP) outstat=&outp ;
			var &varlist;
		run;
			
		data &outp;
			set &outp;

			if _TYPE_ in ("SCORE", "EIGENVAL") then delete;
		run;
	%end;

	%if %isBlank(&outcov)=0 %then %do;
		proc princomp data=ZZ2(TYPE=SSCP) outstat=&outcov cov ;
			var &varlist;
			
		data &outcov;
			set &outcov;

			if _TYPE_ in ("SCORE", "EIGENVAL") then delete;
		run;
	%end;

	%if %isBlank(&outs)=0 %then %do;
		proc sort data=ZZ1 out=ZZ3;
			by &domain;
		run;

		proc rank  data=ZZ3 out=ZZ3;
			var &varlist;
			by &domain;
		run;

		PROC SURVEYREG DATA=ZZ3;
		MODEL pseudo_y= &varlist /XPX;
					   strata &strata; 
					   cluster &cluster; 
					   weight &weight; 
					   domain &domain;
					   ODS OUTPUT XPX=ZZ2;
		RUN;

		DATA ZZ2;
			SET ZZ2;

			IF DOMAIN="&domain.=&SubGrp.";

			rename parameter=_NAME_;

			_TYPE_="SSCP";

			drop  DOMAIN;
		run;

		proc princomp data=ZZ2(TYPE=SSCP) outstat=&outs ;
			var &varlist ;
		run; 

		data &outs;
			set &outs;

			if _TYPE_ in ("SCORE", "EIGENVAL") then delete;
		run;
	%end;
%end;

%if %isBlank(&outp4reg)=0 %then %do;
	DATA ZZ1;
		set &data;

		pseudo_y=_n_;
	run;

	PROC SURVEYREG DATA=ZZ1;
		MODEL &depend= &indep /XPX deff;
					   strata &strata; 
					   cluster &cluster; 
					   weight &weight; 
					   domain &domain;
					   ODS OUTPUT XPX=ZZ2 DomainSummary=ZZ5 ParameterEstimates=ZZ4 ;
	RUN;

	data ZZ4;
		set ZZ4;

		IF DOMAIN="&domain.=&SubGrp.";

		match=1;

		keep parameter designeffect match;
	run;

	data ZZ5;
		set ZZ5;

		IF DOMAIN="&domain.=&SubGrp." and Label1=:"Number of Observations in Domain";

		match=1;

		keep nValue1 match;
	run;

	data ZZ6;
		merge ZZ5 ZZ4;
		by match;

		N=nValue1/DesignEffect;

		keep parameter n;
	run;

	proc transpose data=ZZ6 out=ZZ6;
		id parameter;
	run;

	data ZZ6;
		set ZZ6;

		rename _NAME_=_TYPE_ ;

		drop intercept;
	run;

	DATA ZZ2;
		SET ZZ2;

		IF DOMAIN="&domain.=&SubGrp.";

		rename parameter=_NAME_;

		_TYPE_="SSCP";

		drop  DOMAIN;
	run;

	proc princomp data=ZZ2(TYPE=SSCP) outstat=&outp4reg ;
		var &depend &indep ;

	run;
		
	data &outp4reg;
		set &outp4reg;

		if _TYPE_ in ("SCORE", "EIGENVAL", "N") then delete;
	run;

	data &outp4reg;
		set &outp4reg ZZ6;
	run;
%end;

%if %isBlank(&outppval)=0 or %isBlank(&outspval)=0 %then %do;

%macro surveyReg(target=, var=, out=);
proc surveyReg data=source;
	strata &strata;
	cluster &cluster;
	weight &weight;
	domain &domain;
	model &target=&var /SOLUTION;
	ods output PARAMETERESTIMATES=est(where=(&domain=&subGrp)) FITSTATISTICS=fit;
run;

data fit;
	set fit;
	if domain="&domain=&subGrp";
run;

proc sql noprint;
	insert into &out
	select distinct lowcase(parameter) as parameter, cValue1 as r_square, 
				    sign(estimate)*sqrt(input(cValue1, 8.)) as r,
		   		    probt as pValue format pvalue6.4, a.domain, "&target" as depVar
	from fit as a,
		 est as b
	where label1 = "R-Square" and	
		  parameter = "&var" 
	;
quit;

proc sort nodupkey data=&out;
	by depVar descending r parameter;
run;
%mEnd surveyReg;

data &data;
	set &data;
	%if &nomiss=1 %then %do;
		if nmiss(of &id &varlist) then &domain=-999;/*corresponding nomiss option in proc corr*/
	%end;
run;

proc sort data=&data out=dset(keep=&varlist);
	by &id;
run;

proc sql noprint;
	select nvar into :nvar
	from DICTIONARY.TABLES 
	where LIBNAME="WORK" and MEMNAME='DSET';
quit;

proc contents data=DSET out=contents;
run;

proc sql;
	select name into:var1-:%sysfunc(compress(var&nvar))
	from contents;
quit;

proc sql noprint;
create table CorrCov_r
(parameter char(15), r_square char(8), r num(8), probt num(8), domain char(15), depVar char(15));

create table CorrCov_rank
(parameter char(15), r_square char(8), r num(8), probt num(8), domain char(15), depVar char(15));
quit;

/*sorted dataset used in PROC RANK to get the subgroup sum of weights*/
proc sort data=&data out=dset_sorted;
	by &domain;
run;

proc sql noprint;
create table diag_matrix as
	select "1.000" as diag length=15, lowcase(name) as parameter length=50
	from contents
	order by parameter;
quit;

proc transpose data=diag_matrix out=d_matrix(drop=_name_);
	var diag;
	by parameter;
	id parameter;
run;

proc sort data=d_matrix;
	by parameter;
run;

%end;

%if %isBlank(&outppval)=0 %then %do;
	data source;
		set &data;
		%do i=1 %to &nvar; 
			%do j=1 %to &nvar;
				%surveyReg(target=%quote(&&var&i), var=%quote(&&var&j),
							   out=corrCov_r); 
			%end;
		%end;

proc sql noprint;
create table r_sorted as	
	select parameter length=15, lowcase(depVar) as depVar length=50, 
		   put(r, 8.4) as r_character length=15
	from CorrCov_r
	order by depvar;
quit;

proc transpose data=r_sorted out=r_matrix(rename=depVar=parameter drop=_name_);
	var r_character;
	by depVar;
	id parameter;
run;

proc sort data=r_matrix;
	by parameter;
run;

proc sql noprint;
create table p_value as
	select parameter length=15, lowcase(depVar) as depVar length=50, 
		case when r = 1 then " "
			 when probt < 0.0002 then "<.0001"
			 else put(probt, pvalue6.4)
		end as pValue length=15
	from CorrCov_r
	order by depVar;
quit;

proc transpose data=p_value out=p_matrix(rename=depVar=parameter drop=_name_);
	var pValue;
	by depVar;
	id parameter;
run;

data p_matrix(rename=parameter1=parameter);
	set p_matrix;
	length parameter1 $50;
		   parameter1=catx('_', parameter, "p");
	drop parameter;
run;

proc sort data=p_matrix;			
	by parameter;
run;

data &outppval;
	merge d_matrix
		  r_matrix
		  p_matrix;
	by parameter;

	if index(parameter, "_p") > 0 then parameter=" ";
run;

%end;

%if %isBlank(&outspval)=0 %then %do;
		proc rank data=dset_sorted out=source;
			by &domain;
			var &varlist;
		run;

		%do i=1 %to &nvar; 
			%do j=1 %to &nvar;
				%surveyReg(target=%quote(&&var&i), var=%quote(&&var&j), out=corrCov_rank); 
			%end;
		%end;

		proc sql noprint;
		create table rank_sorted as	
			select parameter length=15, lowcase(depVar) as depVar length=50, 
		   		   put(r, 8.4) as r_character length=15
			from CorrCov_rank
			order by depvar;
		quit;

		proc transpose data=rank_sorted out=rank_rMatrix(rename=depVar=parameter drop=_name_);
			var r_character;
			by depVar;
			id parameter;
		run;

		proc sort data=rank_rMatrix;
			by parameter;
		run;

		proc sql noprint;
		create table rank_pValue as
			select parameter length=15, lowcase(depVar) as depVar length=50, 
				   case when r = 1 then " "
						when probt < 0.0002 then "<.0001"
				   else put(probt, pvalue6.4)
				   end as pValue length=15
			from CorrCov_rank
			order by depVar;
		quit;

		proc transpose data=rank_pValue out=rank_pMatrix(rename=depVar=parameter drop=_name_);
			var pValue;
			by depVar;
			id parameter;
		run;

		data rank_pMatrix(rename=parameter1=parameter);
			set rank_pMatrix;
			length parameter1 $50;
			parameter1=catx('_', parameter, "p");
			drop parameter;
		run;

		proc sort data=rank_pMatrix;			
			by parameter;
		run;

		data &outspval;
			merge d_matrix
				  rank_rMatrix
				  rank_pMatrix;
			by parameter;

			if index(parameter, "_p") > 0 then parameter=" ";
		run;
	%end;

ods exclude none;

%if &excelout=1 %then %do;
		options missing=' ';
		ods listing close;
		ODS EXCEL file="&excelfile"
		STYLE=printer OPTIONS (Orientation = 'landscape' FitToPage = 'yes' Pages_FitWidth = '1' 
							   Pages_FitHeight = '100' embedded_titles = 'yes' 
							   EMBEDDED_FOOTNOTES = 'yes');
	%if &outppval ne %then 
		%do;
			ODS EXCEL OPTIONS ( sheet_name="Pearson Correlation Matrix");	
			title1 justify=center HEIGHT=12pt "Pearson Correlation Matrix";
			title2 justify=center HEIGHT=10pt "Rank-ordered data is not used"; 
			footnote1 justify=left HEIGHT=10pt " ";
			proc print data=&outppval noobs;
			run;
		%end;

	%if &outspval ne %then 
		%do;
			ODS EXCEL OPTIONS ( sheet_name="Rank-based Correlation Matrix");	
			title1 justify=center HEIGHT=12pt "Rank-based Spearman Correlation Matrix"; 
			title2 justify=center HEIGHT=10pt "Rank-ordered data is used"; 
			footnote1 justify=left HEIGHT=10pt "Please use p-values as informational; Spearman Correlation not in Complex Survey Literature";
			proc print data=&outspval noobs;
			run;
		%end;

	%if &outp ne %then 
		%do;
			ODS EXCEL OPTIONS ( sheet_name="Pearson Correlation Data Set");	
			title1 justify=center HEIGHT=12pt "Pearson Correlation Matrix; TYPE=CORR Data set";
			title2 justify=center HEIGHT=10pt "Rank-ordered data is not used"; 
			footnote1 justify=left HEIGHT=10pt " ";
			proc print data=&outp noobs;
			run;
		%end;

	%if &outs ne %then 
		%do;
			ODS EXCEL OPTIONS ( sheet_name="Rank-based Correlation Data Set");	
			title1 justify=center HEIGHT=12pt "Rank-based Spearman Correlation Matrix TYPE=CORR Data set"; 
			title2 justify=center HEIGHT=10pt "Rank-ordered data is used"; 
			footnote1 justify=left HEIGHT=10pt " ";
			proc print data=&outs noobs;
			run;
		%end;

%if &outsscp ne %then 
		%do;
			ODS EXCEL OPTIONS ( sheet_name="SSCP Data Set");	
			title1 justify=center HEIGHT=12pt "Sum of Squares and Crossproducts; TYPE=SSCP Data set";
			title2 justify=center HEIGHT=10pt "Rank-ordered data is not used"; 
			footnote1 justify=left HEIGHT=10pt " ";
			proc print data=&outsscp noobs;
			run;
		%end;

	%if &outcov ne %then 
		%do;
			ODS EXCEL OPTIONS ( sheet_name="Covariance Data Set");	
			title1 justify=center HEIGHT=12pt "Covariance Matrix; TYPE=COV Data set"; 
			title2 justify=center HEIGHT=10pt "Rank-ordered data is not used"; 
			footnote1 justify=left HEIGHT=10pt " ";
			proc print data=&outcov noobs;
			run;
		%end;
	%if &outp4reg ne %then 
		%do;
			ODS EXCEL OPTIONS ( sheet_name="Correlation Data Set for PROC REG");	
			title1 justify=center HEIGHT=12pt "Pearson Correlation Matrix for PROC REQ; TYPE=CORR Data set";
			title2 justify=center HEIGHT=10pt "Rank-ordered data is not used"; 
			footnote1 justify=left HEIGHT=10pt "Sample Size N's Adjusted for Design Effect For Use in PROC REG";
			footnote2 justify=left HEIGHT=10pt "The Independent Variable with the Largest Design Effect";
			footnote3 justify=left HEIGHT=10pt "Will Have Same Results in PROC SURVEYREG and PROC REG";
			proc print data=&outp4reg noobs;
			run;
		%end;

	ods EXCEL close;
		ods listing;
%end;

ods graphics on;

%if &listing=1 %then %do;
	%if %isBlank(&outppval)=0 %then %do;
		title1 "Complex Survey Based Pearson Product-Moment Correlations";
		proc print data=&outppval;
		run;
	%end;
%if %isBlank(&outspval)=0 %then %do;
		title1 "Complex Survey Based Rank-Based Correlations";
		proc print data=&outspval;
		run;
	%end;
%end;

ods select none;
proc datasets;
   delete ZZ: contents CorrCov_r CorrCov_rank dset_sorted diag_matrix d_matirx fit est
		  source r_sorted r_matrix p_value p_matrix;
run;
ods select all;
quit;
%mEnd SurveyCorrCov;

/******************************************************************************************************************************
TESTING OF MACROS FOR SAS GLOBAL FORUM PAPER:
%SURVEYCORRCOV Macro: Complex Survey Data Correlations for Multivariate Analysis and Model Building 
*******************************************************************************************************************************/


title1 "Results from paper's section THE %SURVEYCORRCOV MACRO";
title2 "Display 1's Four Data Sets Above OUTP4REG";
title3 "Also Excel and Listing Output for Display 2";
%SurveyCorrCov(data=a, id=dupersid, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=outp,
			   outs=outs,outsscp=outsscp,outcov=outcov,depend=,indep=outcov,outp4reg=,surveyregoptions=,
			   outppval=outppval, outspval=outspval, nomiss=, excelout=1, 
			   excelfile=/*PERSONALIZE YOUR LOCATION OF EXCEL OUTPUT HERE SUCH AS*/ C:\OUTPUT\excelfile.xlsx, 
			   listing=,varlist=ADCAPE42 ADCLIM42 ADDAYA42);
run;


title1 "Using the %SURVEYCORRCOV matrix to produce the";
title2 "correlation matrix of nine types of expenses";
title3 "from MEPS 2016 data. Both original and natural";
title4 "log-tranformed variable are included";
%SurveyCorrCov(data=a, id=, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=MEPScorr,
			   outs=,outsscp=,outcov=,depend=,indep=,outp4reg=,surveyregoptions=,
			   outppval=, outspval=, nomiss=, excelout=, excelfile=, listing=,
		 	   varlist=DVTEXP16 ERTEXP16 HHEXP16 OBVEXP16 OPTEXP16 RXEXP16 IPTEXP16
			   OTHEXP16 VISEXP16 logDental logER logHome logOffice logOutPt logRX
			   logInPt logOthSup logVision);

title1 "Using the %SURVEYCORRCOV matrix to produce the";
title2 "correlation matrix of nine types of expenses";
title3 "from MEPS 2016 data. Both original and natural";
title4 "log-tranformed variable are included";
%SurveyCorrCov(data=a, id=, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=MEPScorr,
			   outs=,outsscp=,outcov=,depend=,indep=,outp4reg=,surveyregoptions=,
			   outppval=, outspval=, nomiss=, excelout=, excelfile=, listing=,
		 	   varlist=DVTEXP16 ERTEXP16 HHEXP16 OBVEXP16 OPTEXP16 RXEXP16 IPTEXP16
			   OTHEXP16 VISEXP16 logDental logER logHome logOffice logOutPt logRX
			   logInPt logOthSup logVision);
/*NOTE: SHORTER MACRO CALL BELOW PROVIDES THE SAME RESULT
	%SurveyCorrCov(data=a, strata=VARSTR, cluster=VARPSU, 
		weight=PERWT16F, domain=include, subGrp=1, outp=MEPScorr,
		varlist= DVTEXP16 ERTEXP16 HHEXP16 OBVEXP16 OPTEXP16 
			RXEXP16 IPTEXP16 OTHEXP16 VISEXP16 
			logDental logER logHome logOffice logOutPt logRX
			logInPt logOthSup logVision);*/

title1 "PCA based on the correlation matrix";
proc princomp data=MEPScorr(TYPE=CORR);
	var DVTEXP16 ERTEXP16 HHEXP16 OBVEXP16 OPTEXP16 RXEXP16
		IPTEXP16 OTHEXP16 VISEXP16;
run;

title1 "PCA based on the covariance matrix";
proc princomp data=MEPScorr(TYPE=CORR) COV;
	var DVTEXP16 ERTEXP16 HHEXP16 OBVEXP16 OPTEXP16 RXEXP16
		IPTEXP16 OTHEXP16 VISEXP16;
run;

title1 "PCA based on the cov matrix of natural log-transformed";
proc princomp data=MEPScorr(TYPE=CORR) COV;
	var logDental logER logHome logOffice logOutPt logRX 
		logInPt logOthSup logVision ;
run;

title1 "Using the %SURVEYCORRCOV matrix to produce the";
title2 "correlation matrix of twelve questions from";
title3 "MEPS 2016 data. Ages 20-65";
%SurveyCorrCov(data=a, id=, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=SF12corr,
			   outs=,outsscp=,outcov=,depend=,indep=,outp4reg=,surveyregoptions=,
			   outppval=, outspval=, nomiss=, excelout=, excelfile=, listing=,
		 	   varlist=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 ADMALS42 ADMWLM42
			   ADPAIN42 ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42);
/*NOTE: SHORTER MACRO CALL BELOW PROVIDES THE SAME RESULT
%SurveyCorrCov(data=a,strata=VARSTR, cluster=VARPSU, 
		weight=PERWT16F, domain=include, subGrp=1, outp=SF12corr,
		varlist=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 
			ADMALS42 ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 ADNRGY42
			ADSOCA42);*/	

title1 "EFA based on the corr matrix; no rotation";
proc factor data=SF12corr(TYPE=CORR) method=principal score ;
	var ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 ADMALS42 
		ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42;
run;

title1 "EFA based on the corr matrix; orthogonal rotation";
proc factor data=SF12corr(TYPE=CORR) rotate=varimax score ;
	var ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 ADMALS42 
		ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42;
run;

title1 "EFA based on the corr matrix; non-orthogonal rotation";
proc factor data=SF12corr(TYPE=CORR) rotate=promax score;
	var ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 ADMALS42 
		ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42;
run;

title1 "Using the %SURVEYCORRCOV matrix to produce the";
title2 "correlation matrix of nine types of expenses";
title3 "and twelve SF-12 questions from MEPS 2016 data";
%SurveyCorrCov(data=a, id=, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=CanCorr1,
			   outs=,outsscp=,outcov=,depend=,indep=,outp4reg=,surveyregoptions=,
			   outppval=, outspval=, nomiss=, excelout=, excelfile=, listing=,
		 	   varlist=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 ADMALS42 ADMWLM42
			   ADPAIN42 ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42 logDental logER logHome logOffice
			   logOutPt logRX logInPt logOthSup logVision);
/*NOTE: SHORTER MACRO CALL BELOW PROVIDES THE SAME RESULT
%SurveyCorrCov(data=a, strata=VARSTR, cluster=VARPSU, 
		weight=PERWT16F, domain=include, subGrp=1, outp=CanCorr1,
		varlist=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 
			ADMALS42 ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 
			ADNRGY42 ADSOCA42 logDental logER logHome logOffice
			logOutPt logRX logInPt logOthSup logVision);*/

title1 "Canonical Correlation, No Sample Size Adjustment";
proc cancorr data=CanCorr1(TYPE=CORR);
	var ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 ADMALS42 ADMWLM42 ADPAIN42 ADPALS42  
		ADPWLM42 ADNRGY42 ADSOCA42; 
	with logDental logER logHome logOffice logOutPt logRX logInPt logOthSup logVision;
run;

title1 "Using the %SURVEYCORRCOV matrix to produce the";
title2 "correlation matrix of nine types of expenses";
title3 "twelve SF-12 questions, age from MEPS 2016 data";
%SurveyCorrCov(data=a, id=, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=, outs=,
			   outsscp=,outcov=,depend=ADGENH42,outp4reg=CanCorr2, surveyregoptions=,
			   indep=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADMALS42 ADMWLM42 ADPAIN42 ADPALS42  
			   ADPWLM42 ADNRGY42 ADSOCA42 logDental logER logHome logOffice logOutPt logRX
			   logInPt logOthSup logVision age16X, outppval=, outspval=, nomiss=, excelout=, 
			   excelfile=, listing=, varlist=);
/*NOTE: SHORTER MACRO CALL BELOW PROVIDES THE SAME RESULT
%SurveyCorrCov(data=a, strata=VARSTR, cluster=VARPSU, 
		weight=PERWT16F, domain=include, subGrp=1, 
			outp4reg=CanCorr2,
		indep=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42  
			ADMALS42 ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 
			ADNRGY42 ADSOCA42 logDental logER logHome logOffice
			logOutPt logRX logInPt logOthSup logVision age_16X,
			depend=ADGENH42);*/

title1 "Partial Canonical Correlation, Sample Size Adjustment";
proc cancorr data=CanCorr2(TYPE=CORR);
	var ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADGENH42 ADMALS42 ADMWLM42 ADPAIN42 ADPALS42  
		ADPWLM42 ADNRGY42 ADSOCA42; 
	with logDental logER logHome logOffice logOutPt logRX logInPt logOthSup logVision;
	partial age16X;
run;

title1 "Using the %SURVEYCORRCOV matrix to produce the";
title2 "rank-based correlation matrix of nine types of expenses";
title3 "twelve SF-12 questions, age from MEPS 2016 data";
%SurveyCorrCov(data=a, id=, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=, outs=RankCorr,
			   outsscp=,outcov=,depend=,indep=,outp4reg=,surveyregoptions=,
			   outppval=, outspval=, nomiss=, excelout=, excelfile=, listing=,
		 	   varlist=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADMALS42 ADMWLM42 ADPAIN42 
			   ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42 logDental logER logHome logOffice logOutPt
			   logRX logInPt logOthSup logVision age16X ADGENH42);

/*NOTE: SHORTER MACRO CALL BELOW PROVIDES THE SAME RESULT
%SurveyCorrCov(data=a, strata=VARSTR, cluster=VARPSU, 
		weight=PERWT16F, domain=include, subGrp=1, 
			outs=RankCorr,
		varlist=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42  
			ADMALS42 ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 
			ADNRGY42 ADSOCA42 logDental logER logHome logOffice
			logOutPt logRX logInPt logOthSup logVision age_16X 
			ADGENH42);*/

title1 "Variable Clustering";
proc varclus data=RankCorr(TYPE=CORR);
	var ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADMALS42 ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 
		ADNRGY42 ADSOCA42 logDental logER logHome logOffice logOutPt logRX logInPt logOthSup
		logVision age16X ADGENH42;
run;

title1 "Using the %SURVEYCORRCOV matrix to produce the";
title2 "correlation matrix for dependent variable of";
title3 "log of total medical expenditures in 2016,";
title4 "with independent variables of twelve SF-12";
title5 "and age from MEPS data";
%SurveyCorrCov(data=a, id=, nvar=, strata=VARSTR, cluster=VARPSU, 
			   weight=PERWT16F, domain=include, subGrp=1, outp=, 
			   outs=,outsscp=,outcov=,depend=LogTotalExp, 
			   indep=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADMALS42 ADMWLM42 ADPAIN42 ADPALS42  
			   ADPWLM42 ADNRGY42 ADSOCA42 ADGENH42 age16X,outp4reg=ProcReg,surveyregoptions=,
			   outppval=, outspval=, nomiss=, excelout=, excelfile=, listing=, varlist=);

/*%SurveyCorrCov(data=a, strata=VARSTR, cluster=VARPSU, 
		weight=PERWT16F, domain=include, subGrp=1, 
			outp4reg=ProcReg, depend=LogTotalExp,
		indep=ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42  
			ADMALS42 ADMWLM42 ADPAIN42 ADPALS42 ADPWLM42 
			ADNRGY42 ADSOCA42 ADGENH42 age_16X);*/

ods graphics on;
title1 "Variance Inflation Factor and Ridge Regression";
proc reg data=ProcReg(TYPE=CORR)  outvif outest=b ridge=0 to 2 by .2;
	model LogTotalExp = ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADMALS42 ADMWLM42 ADPAIN42  
						ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42 ADGENH42 age16X /vif ;
run;

proc print data=b;run;

title1 "Model Selection By Mallows' CP Selection";
proc reg data=ProcReg(TYPE=CORR)  ;
	model LogTotalExp = ADCAPE42 ADCLIM42 ADDAYA42 ADDOWN42 ADMALS42 ADMWLM42 ADPAIN42 
		  ADPALS42 ADPWLM42 ADNRGY42 ADSOCA42 ADGENH42 age16X / selection=cp;
run;

title1 "Implementing Mallows' CP Model from PROC REG in SURVEYREG";
PROC SURVEYREG DATA=a;
	MODEL LogTotalExp= ADCLIM42 ADDOWN42 ADMALS42 ADMWLM42 ADPAIN42 ADPWLM42 ADNRGY42 Age16X;
	strata VARSTR; 
	cluster VARPSU; 
	weight PERWT16F; 
	domain include; 
run;