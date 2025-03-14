**THIS DO FILE PRODUCES RESULTS FOR THE CASH DROP EXPERIMENT: FIGURE 4, TABLE 4;


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
	
	
	*********************************;
	*** TABLE 4: CASH EXPERIMENT TABLE;
	*********************************;
		*Call data;
			use data/data_cashexp, clear;
			des, fu;
		
		*Fill controls with missing;
		foreach list in a_list {;
			dis "``list''";
			foreach x of varlist ``list''{;
				gen m_`x'=0;
				replace m_`x'=1 if `x'==.;
				replace `x'=-1 if `x'==.;
				local `list'_with_m ``list'_with_m'  `x' m_`x';
			};
		};	
		*Preliminary steps;
		estimates clear;
		local y tookup;

		**Without interaction;
		
		
		*Field FE (baseline spec for other tables);
		eststo: areg `y' B cash if tookup!=., a(fid2);
		quietly sum `y' if A1==1;
		estadd local y_mean_round = string(r(mean), "%9.3f");		
		test B = cash;
		estadd local p_val_round = string(r(p), "%9.3f");													
		estadd local field_FE Y;
		estadd local p_contr N;		
		
		*Plot controls;
		eststo: xi:areg `y' B cash `a_list_with_m' if tookup!=., a(fid2);
		quietly sum `y' if A1==1 ;
		estadd local y_mean_round = string(r(mean), "%9.3f");		
		test B = cash;
		estadd local p_val_round = string(r(p), "%9.3f");											
		estadd local field_FE Y;
		estadd local p_contr Y;		

		**With interaction;
		
		
		*Field FE (baseline spec for other tables);
		eststo: areg `y' B cash B_cash if tookup!=., a(fid2);
		quietly sum `y' if A1==1;
		estadd local y_mean_round = string(r(mean), "%9.3f");		
		test B = cash;
		estadd local p_val_round = string(r(p), "%9.3f");													
		estadd local field_FE Y;
		estadd local p_contr N;		
		
		*Plot controls;
		eststo: xi:areg `y' B cash B_cash `a_list_with_m' if tookup!=., a(fid2);
		quietly sum `y' if A1==1 ;
		estadd local y_mean_round = string(r(mean), "%9.3f");		
		test B = cash;
		estadd local p_val_round = string(r(p), "%9.3f");													
		estadd local field_FE Y;
		estadd local p_contr Y;		
	
		
		*ESttab;			
		esttab using "out/KMD_cashexp_main_results.tex", 					
		keep(B cash B_cash)				
		order(B cash B_cash)
		replace br se  label star(* 0.10 ** 0.05 *** 0.01) obslast nomtitles  compress longtable 	
		b(%9.3f) se(%9.3f) 
		scalars(/*"field_FE Field FE"*/ "p_contr Plot Controls" "y_mean_round Mean dep. var. (Pay Upfront group)" "p_val_round P-value: Pay at Harvest = Cash") 
		nonotes 
		nogaps				
		title(Cash Drop Experiment: Treatment Effects on Take-Up \label{tab:liquidity-results}) ;	
	
				
	*********************************;
	*** FIGURE 4: TAKE-UP GRAPH FOR CASH EXPERIMENT  **;
	*********************************;
	
		count;
		local N = r(N);
		
		drop cash;
		
			*Create treatment means;
				foreach x in A B{;
					foreach c in 1 2{;
						sum tookup if treat=="`x'`c'";
						local t_`x'`c'=r(mean);
					};
				};
				
				
			*Run regression and save  SE;
				reg tookup A2 B1 B2;			
				foreach x in A2 B1 B2 {;
					local se_`x'=_se[`x'];
					*local u_`x'=`t_`x''+1.95*`se_`x'';
					*local b_`x'=`t_`x''-1.95*`se_`x'';
				};
			*COllapse database at mean by treatment level	;
				collapse (mean) mean=tookup (sd) sd=tookup (count) n=tookup, by(treat);
			*Preparation for graph;	
				gen treat_num=1;
				replace treat_num=2 if treatment=="A2";
				replace treat_num=3 if treatment=="B1";
				replace treat_num=4 if treatment=="B2";				
				generate h = mean;
				replace h=mean+1.96*`se_A2' if treatment=="A2";
				replace h=mean+1.96*`se_B1' if treatment=="B1";
				replace h=mean+1.96*`se_B2' if treatment=="B2";				
				gen l=mean;
				replace l=mean-1.96*`se_A2' if treatment=="A2";
				replace l=mean-1.96*`se_B1' if treatment=="B1";
				replace l=mean-1.96*`se_B2' if treatment=="B2";				
				label define treat_num 1 "Pay Upfront" 2"Pay Upfront + Cash" 3"Pay At Harvest" 4"Pay At Harvest + Cash";
				label value treat_num treat_num;	
			*Prepare means for scatter;	
				gen mean2=round(mean+.03,.01);
				gen mean3=round(mean,.01);

			*Graph;
				graph twoway (bar mean treat_num if treat_num==1 ,barwidth(.8) color(eltblue) )			
				(bar mean treat_num if treat_num==2, barwidth(.8) mlabel(mean) color(navy) )
				(bar mean treat_num if treat_num==3, barwidth(.8) mlabel(mean) color(orange))
				(bar mean treat_num if treat_num==4, barwidth(.8) mlabel(mean) color(maroon))				
				(rcap h l treat_num if treat_num!=1)
				(scatter mean2 treat_num, mlabel(mean3) mlabsize(medlarge)  mlabcolor(black) msymbol(i)),
				 /*over(treat_num) bargap(50)*/  
					legend(off)		title("Insurance Take-Up (N=`N')")  
					xlabel(1 "Pay Upfront" 2 `""Pay Upfront" "+ Cash""' 3 "Pay At Harvest" 4 `""Pay At Harvest" "+ Cash""', labsize(small) noticks) 
				 xtitle("") yscale(range(0 1)) graphregion(color(white)) bgcolor(white);			
			*Graph export;	 
				graph export "out/histogram_cashexp.pdf", replace;
		
		
exit;

