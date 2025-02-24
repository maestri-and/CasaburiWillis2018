**THIS DO FILE PRODUCES RESULTS FOR THE TIME PREFERENCE EXPERIMENT: FIGURE 5; TABLE 5;

*Preamble;
		 # d ;
		 cap log close;
		 set more off;
		 clear;
		 clear mata;
		 clear matrix;
		 set mem 500m;
		 *set maxvar 32000;
		 *set matsize 4000;		
		 pause on;

	*Change directories;		
			cd "D:\Download\contract_farming_kenya\insurance_replication_AER";	
		
		
	*Define locals;
		local a_list l_area  l_yield;
		local hh_basics man age  ;
		local wealth  acres any_cow  share_income_cane savings_1 savings_5;
		local expectations exp_yield_normal exp_yield_good exp_yield_bad ;
		local relationship   good_relation trust_fa trust_manager;
		local x_list `hh_basics' `wealth' `expectations'  `relationship';

	*Call data;
		use data/data_timeexp, clear;

	*********************************;
	*** TABLE 5: TIME EXPERIMENT RESULTS;
	*********************************;
		*Fill controls with missing;
			foreach list in a_list x_list{;
				dis "``list''";
				foreach x of varlist ``list''{;
					gen m_`x'=0;
					replace m_`x'=1 if `x'==.;
					replace `x'=-1 if `x'==.;
					local `list'_with_m ``list'_with_m'  `x' m_`x';
				};
			};	
		*Assign lagged yield to wealth;
			local wealth `wealth' l_yield;
		*Preliminary steps;
			estimates clear;
			local y tookup;
			tab `y';
		
			
		*Field FE (baseline spec for other tables);
			eststo: areg `y' C if tookup!=., a(fid3);
			quietly sum `y' if A==1;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr N;		
			estadd local f_contr N;
		
		*Plot controls;
			eststo: xi:areg `y' C  `a_list_with_m' if tookup!=., a(fid3);
			quietly sum `y' if A==1 ;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr Y;		
			estadd local f_contr N;
	
		*Farmer controls;
			eststo: xi:areg `y' C   `x_list_with_m' if tookup!=., a(fid3);
			quietly sum `y' if A==1 ;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr N;		
			estadd local f_contr Y;
	
		*Plot+farmer controls;
			eststo: xi:areg `y' C  `a_list_with_m' `x_list_with_m' if tookup!=., a(fid3);
			quietly sum `y' if A==1 ;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr Y;		
			estadd local f_contr Y;
		
		*ESttab;			
			esttab using "out/KMD_timeexp_results.tex", 					
			keep(C)				
			replace br se  label star(* 0.10 ** 0.05 *** 0.01) obslast nomtitles  compress longtable 	
			b(%9.3f) se(%9.3f) 
			scalars(/*"field_FE Field FE"*/ "p_contr Plot Controls" "f_contr Farmer Controls" "y_mean_round Mean dep. var. (Receive Choice Now group)") 
			nonotes 
			nogaps				
			title(Intertemporal Preferences Experiment: Treatment Effect on Take-Up \label{tab:exp3-results}) ;				
		
	*********************************;
	*** FIGURE 5: TIME EXPERIMENT GRAPH  **;
	*********************************;	
		*Sample size;
		count;
		local N = r(N);
	
		*Create treatment dummies;
			foreach x in A C{;
				sum tookup if `x' == 1;
				local t_`x'=r(mean);
			};
		*Run regression and save  SE;
			reg tookup C;			
			foreach x in C{;
				local se_`x'=_se[`x'];
				*local u_`x'=`t_`x''+1.95*`se_`x'';
				*local b_`x'=`t_`x''-1.95*`se_`x'';
			};
		*COllapse database at mean by treatment level	;
			collapse (mean) mean=tookup (sd) sd=tookup (count) n=tookup, by(treat);
		*Preparation for graph;	
			generate h = mean;
			replace h=mean+1.96*`se_C' if treatment==1;
			gen l=mean;
			replace l=mean-1.96*`se_C' if treatment==1;
		*Prepare means for scatter;	
			gen mean2=round(mean+.03,.01);
			gen mean3=round(mean,.01);
		*Graph;
			graph twoway (bar mean treat if treat==0 ,barwidth(.8) base(0) color(navy) )			
			(bar mean treat if treat==1, barwidth(.8) mlabel(mean) color(forest_green))
			(rcap h l treat if treat!=0)
			(scatter mean2 treat, mlabel(mean3) mlabsize(medlarge)  mlabcolor(black) msymbol(i)),
			 /*over(treat_num) bargap(50)*/  
				legend(off)		title("Insurance Take-Up (N=`N')")  
				xlabel( 0 "Receive Choice Now" 1 "Receive Choice in One Month", noticks) ylabel(0(.2).8) xtitle("") graphregion(color(white)) bgcolor(white) ;			
		*Graph export;	 
			graph export "out/histogram_timeexp.pdf", replace;			

	
exit;
