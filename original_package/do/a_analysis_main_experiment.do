**THIS DO FILE PRODUCES RESULTS FOR THE MAIN EXPERIMENT: FIGURES 3,6,7; TABLES 1,2,3,6;

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

	*********************************;
	***  TABLE 1: BALANCE TABLE ***;
	*********************************;
		*Call data;
			use data/data_mainexp, clear;
			
		*Set empty "matrix";
			preserve;
				clear;
				set obs 100;
				*J=variable title column+# mean&sd columns+# p-values columns+obs N column;
				local J=10;
				forvalues i=1/`J'{;
					gen v`i'="";
				};
				tempfile sum_stats;
				sa `sum_stats';
			restore;
	
		*Fill rows for each baseline covariate;	
			local j=1;			
			foreach var of varlist `a_list' `x_list'  {;							
				local k=`j'+1;
				local var_label: variable label `var';				
				*Mean and sd by treatment group;
				foreach group in A1 A2 B {;
					cap sum `var' if `group'==1 & tookup!=.;
					local mean_`group'=r(mean);		
					local mean_`group'= substr("`mean_`group''", 1, 4);
					dis "`mean_`group''";	
					capture sum `var' if `group'==1 & tookup!=.;	
					local sd_`group'=r(sd);
					local sd_`group' =substr("`sd_`group''", 1,4);
					local sd_`group' ="`sd_`group''";
					dis "sd_`group'";
				};					
				*p-values (from regressions with field FE);				
					capture areg `var' A1  if B==0 & tookup!=., a(fid1);
					local p_5 = round((2 * ttail(e(df_r), abs(_b[A1]/_se[A1]))),.001);
					local p_5=substr("`p_5'",1,4);
					capture areg `var' A1 if A2==0 & tookup!=., a(fid1);
					local p_6 = round((2 * ttail(e(df_r), abs(_b[A1]/_se[A1]))),.001);
					local p_6=substr("`p_6'",1,4);				
					capture areg `var' A2 if A1==0 & tookup!=., a(fid1);
					local p_7= round((2 * ttail(e(df_r), abs(_b[A2]/_se[A2]))),.001);
					local p_7=substr("`p_7'",1,4);
					capture areg `var' A if tookup!=., a(fid1);
					local p_8= round((2 * ttail(e(df_r), abs(_b[A]/_se[A]))),.001);
					local p_8=substr("`p_8'",1,4);
				
				*Number of non-missing observations for that variable;
					count if `var'!=. & tookup!=.;
					return list;
					local N_nm=r(N);
					
				*Fill table row for that column;
					preserve;
						use `sum_stats', clear;
						replace v1="`var_label'" if _n==`j';							
						replace v2="`mean_A1'" if _n==`j';
						replace v2="(`sd_A1')" if _n==`k';
						replace v3="`mean_A2'" if _n==`j';
						replace v3="(`sd_A2')" if _n==`k';
						replace v4="`mean_B'" if _n==`j';
						replace v4="(`sd_B')" if _n==`k';						
						forvalues i=5/8{;
							replace v`i'="`p_`i''" if _n==`j' ;
							replace v`i'="`p_`i''*" if _n==`j' & `p_`i''<.1;
							replace v`i'="`p_`i''**" if _n==`j' & `p_`i''<.05;
							replace v`i'="`p_`i''***" if _n==`j' & `p_`i''<.01;
						};
						replace v9="`N_nm'" if _n==`j';
						sa `sum_stats', replace;
					restore;
					local j=`j'+2;
			};		
			
		*Export summary stat table to latex;				
			use `sum_stats', clear;
			des, fu;	
			drop if v2=="";
			listtex using "out/KMD_balancing_FE_1.tex", rstyle(tabular) replace
							headlines( 
							"&Upfront&Upfront-30\%&Harvest&P-value&P-value&P-value&P-value&\\" 
										"&[U1]&[U2]&[H]&[U1-U2]&[U1-H]&[U2-H]&[U-H]&N\\" "\hline" "&&&&&&\\")
						footlines("\bottomrule" );							
		
		*Add field-level variables (no variation across plots within field);
			local relationship `relationship' deliv_harvest_share_plots_fh deliv_harvest_share_plots_sh;


		*********************************;
		*** TABLE 2: MAIN RESULTS**;
		*********************************;
			*Call data;
				use data/data_mainexp, clear;
			
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

			*Preliminary steps;
			estimates clear;
			local y tookup;
			label var A2 "Pay Upfront with 30\% Discount";
			label var B "Pay At Harvest";
		
		
			*Field FE (baseline spec for other tables);
			eststo: areg `y' A2 B if tookup!=., a(fid1);
			quietly sum `y' if A1==1;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr N;		
			estadd local f_contr N;
			
			*A1+A2 vs. B;
			eststo: areg `y'  B if tookup!=., a(fid1);
			quietly sum `y' if A1==1 | A2==1;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr N;		
			estadd local f_contr N;		
		
			*Plot controls;
			eststo: xi:areg `y' A2 B   `a_list_with_m' if tookup!=., a(fid1);
			quietly sum `y' if A1==1 ;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr Y;		
			estadd local f_contr N;
		
			*Farmer controls;
			eststo: xi:areg `y' A2 B   `x_list_with_m' if tookup!=., a(fid1);
			quietly sum `y' if A1==1 ;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr N;		
			estadd local f_contr Y;
		
			*Plot+farmer controls;
			eststo: xi:areg `y' A2 B   `a_list_with_m' `x_list_with_m' if tookup!=., a(fid1);
			quietly sum `y' if A1==1 ;
			estadd local y_mean_round = string(r(mean), "%9.3f");										
			estadd local field_FE Y;
			estadd local p_contr Y;		
			estadd local f_contr Y;
			
			*ESttab;			
			esttab using "out/KMD_main_results.tex", 					
			keep(A2 B)				
			order(A2 B)
			replace br se  label star(* 0.10 ** 0.05 *** 0.01) obslast nomtitles  compress longtable 	
			b(%9.3f) se(%9.3f) 
			scalars(/*"field_FE Field FE"*/ "p_contr Plot Controls" "f_contr Farmer Controls" 
			"y_mean_round Mean dep. var. (Pay Upfront group)") 
			nonotes 
			nogaps				
			title(Main Experiment: Treatment Effects on Take-Up \label{tab:main-results}) ;	


		*********************************;
		*** FIGURE 3: TAKE-UP GRAPH  **;
		*********************************;
			tempfile data;
			sa `data';

			*Sample size;
			count;
			local N = r(N);
		
			*Create treatment dummies;
				foreach x in A1 A2 B{;
					sum tookup if treat=="`x'";
					local t_`x'=r(mean);
				};
			*Run regression and save  SE;
				reg tookup A2 B;			
				foreach x in A2 B{;
					local se_`x'=_se[`x'];
					*local u_`x'=`t_`x''+1.95*`se_`x'';
					*local b_`x'=`t_`x''-1.95*`se_`x'';
				};
			*COllapse database at mean by treatment level	;
				collapse (mean) mean=tookup (sd) sd=tookup (count) n=tookup, by(treat);
			*Preparation for graph;	
				gen treat_num=1;
				replace treat_num=2 if treatment=="A2";
				replace treat_num=3 if treatment=="B";			
				generate h = mean;
				replace h=mean+1.96*`se_A2' if treatment=="A2";
				replace h=mean+1.96*`se_B' if treatment=="B";
				gen l=mean;
				replace l=mean-1.96*`se_A2' if treatment=="A2";
				replace l=mean-1.96*`se_B' if treatment=="B";			
				label define treat_num 1 "ExAnte" 2"ExAnte+Discount" 3"ExPost";
				label value treat_num treat_num;	
			*Prepare means for scatter;	
				gen mean2=round(mean+.02,.01);
				gen mean3=round(mean,.01);
			*Graph;
				graph twoway (bar mean treat_num if treat_num==1 ,barwidth(.8) color(eltblue) )			
				(bar mean treat_num if treat_num==2, barwidth(.8) mlabel(mean) color(navy) )
				(bar mean treat_num if treat_num==3, barwidth(.8) mlabel(mean) color(orange))
				(rcap h l treat_num if treat_num!=1)
				(scatter mean2 treat_num, mlabel(mean3) mlabsize(medlarge)  mlabcolor(black) msymbol(i)),
				 /*over(treat_num) bargap(50)*/  
					legend(off)		title("Insurance Take-Up (N=`N')")   
					xlabel( 1 "Pay Upfront" 2 `""Pay Upfront" "with 30% Discount""' 3 "Pay At Harvest", noticks) 
				 xtitle("") ytitle("") graphregion(color(white)) bgcolor(white) ;			
			*Graph export;	 
				graph export "out/histogram_main.pdf", replace;
			
			use `data', clear;
	
	
		
		*********************************;
		*** TABLES 3, 6, : HETEROGENEITY ANALYSIS BY WEALTH, TRUST+HARVEST RATE;
		*********************************;		
		*Create interactions;
			*Assign lagged yield and lagged area to local wealth;				
			local wealth acres any_cow l_yield l_area share_income_cane savings_1 savings_5;
			
			local lists wealth relationship ;
			foreach var of varlist `wealth' `relationship'{;
				gen B_`var'=B*`var' if `var'!=-1;		
				label var B_`var' "... *B";				
			};
		
		*Standardize variables;
			foreach s in acres l_yield l_area {;
				*Re-setting missing to missing;
				replace `s' = . if `s' == -1;
				
				*Standardize;
				egen z2_`s' = std(`s');
				replace `s' = z2_`s';
				replace B_`s' = B*`s';
			};

		*Regressions;
			estimates clear;
			gen het=.;
			label var het "X";
			gen B_het=.;
			label var B_het "X *Pay At Harvest";
			label var B "Pay At Harvest";
			foreach list in `lists'{;
				local list1=subinstr("`list'","_"," ",.);
				if "`list'"=="wealth"{; local t1 Wealth and Liquidity Constraints Proxies; };
				if "`list'"=="relationship"{; local t1 Proxies for Expectations of Default; };				
				local m1; 
				estimates clear;
				local X ;
				foreach var of varlist ``list''{;					
					replace het=`var';
					replace het=. if B_`var'==.;
					replace B_het=B_`var';
					replace B_het=. if B_`var'==.;

					*Reg with field FE;
					local mt : var label `var';
					eststo, title(`mt'): areg tookup het B_het B , a(fid1);
					quietly sum `y' if A1==1 |A2==1 ;
					estadd local y_mean_round = string(r(mean), "%9.3f");										
					test het+B_het=0;
					estadd local p_mean_round = string(r(p), "%9.3f");					
					quietly sum het if e(sample);
					estadd local x_mean_round = string(r(mean), "%9.3f");										
					quietly sum het if e(sample);
					estadd local x_st_round = string(r(sd), "%9.3f");
				};
				
				*Esttab;
				esttab using "out/KMD_het_`list'.tex", 		
				keep ( B_het  het B)	
				order (B_het het  B )
				replace br se  label star(* 0.10 ** 0.05 *** 0.01) obslast mtitles  compress longtable 	
				b(%9.3f) se(%9.3f) 
				scalars(/*"p_mean_round p-value $\gamma+\delta$"*/ "y_mean_round Mean dep. var. (Pay Upfront group)"
					"x_mean_round Mean heterogeneity var. (X)" "x_st_round S.D. heterogeneity var. (X)") 
				nonotes
				nogaps				
				title(\mbox{Main Experiment: Heterogeneous Treatment Effect by `t1'} \label{tab:heterogeneity-`list1'}) ;			
			};		

		*********************************;
		*** FIGURE 7 (PANEL A&B): ANALYSIS OF HARVEST vs. TREATMENT STATUS;
		*********************************;
			use data/data_mainexp, clear;
			tempfile data;
			sa `data';
			local j=0;
			foreach y in    deliv_harvest_plot deliv_tons  {;
				local j=`j'+1;
				*Call data;
					use  `data', clear;
				*Graph titles;
					if "`y'"=="deliv_harvest_plot"{; local g_title "Panel A: Share of Harvested Plots"; };
					if "`y'"=="deliv_tons"{; local g_title "Panel B: Average Tons of Cane per Harvested Plot"; };
					
				*Prepare: A1 vs. A2 vs. B;
					sum `y' if A1==1;
					local m_A1=round(r(mean),.01);
					*areg `y'  A2 B, a(fid1);		
					reg `y'  A2 B;		
					foreach x in B A2{;
						local m_`x'=round(`m_A1'+_b[`x'],.01);
						local ci_u_`x'=round(`m_A1'+_b[`x']+1.95*_se[`x'],.01);
						local ci_l_`x'=round(`m_A1'+_b[`x']-1.95*_se[`x'],.01);
						local p_`x'= round((2 * ttail(e(df_r), abs(_b[`x']/_se[`x']))),.001);
						local d_`x';
						macro list;
						dis "`p_`x''";
						if `p_`x''<.01{;
							local d_`x'="***";
						};
						else if `p_`x''<.05{;
							local d_`x'="*";
						};
						else if `p_`x''<.10{;
							local d_`x'="*";
						};	
					};
				*Graphs for A1 vs. A2 vs. B;	
					*Prelim;
						clear;
						set obs 3;
						gen mean=.;
						replace mean=`m_A1' if _n==1;
						replace mean=`m_A2' if _n==2;
						replace mean=`m_B' if _n==3;
						gen mean2=round(mean*1.03,.01);
						gen mean3=round(mean,.01);

						gen t=_n;
						quietly sum mean;
						local y_max=r(max)*1.1;
						local m_A1_l=`m_A1'*1.05;
						local m_A2_l=`m_A2'*1.05;
						local m_B_l=`m_B'*1.05;
						foreach z in l u{;
							gen `z'=.;
							replace `z'=`ci_`z'_A2' if t==2;
							replace `z'=`ci_`z'_B' if t==3;
						};
					*Histogram;
						graph twoway (bar mean t if t==1 ,barwidth(.8) color(eltblue) )			
							(bar mean t if t==2, barwidth(.8) mlabel(mean) color(navy) )
							(bar mean t if t==3, barwidth(.8) mlabel(mean) color(orange))
							(rcap u l t if t!=1)
							(scatter mean2 t, mlabel(mean3) mlabsize(medlarge)  mlabcolor(black) msymbol(i)),
							 /*over(treat_num) bargap(50)*/  legend(off)	title(`g_title')   
							xlabel( 1 "Pay Upfront" 2 `""Pay Upfront" "with 30% Discount""' 3 "Pay at Harvest", noticks) 
							 xtitle("") ytitle("") yscale(range(0/`y_max')) ylabel(#4) graphregion(color(white)) bgcolor(white) ;
						graph save "out/g`j'.gph", replace;				
						
			};
			*One figure, two panels;
				graph combine "out/g1.gph" "out/g2.gph", col(1) xsize(10) ysize(12) graphregion(color(white)) /*bgcolor(white)*/;
				graph export "out/histogram_harvesting.eps", replace;							
				erase "out/g1.gph";
				erase "out/g2.gph";
		
		*********************************;
		*** FIGURE 6 : PAST VS. CONTEMPORARY SIDE-SELLING;
		*********************************;
			# d cr	
	
			*Call database;
				use data/data_mainexp, clear

			*Keep one obs per sublocation;
				duplicates drop subloc_code, force
				count
				
			*Graph	
				twoway (histogram deliv_harvest_share_plots_s , start(0) width(0.075) lcolor(white) frequency ) ///
					   (histogram deliv_harvest_share_plots_sh , start(0) width(0.075) ///
					   fcolor(none) lcolor(black) lwidth(medthick) frequency ) , title("Histogram of harvest rates by sublocation") ///
					   legend(order(1 "Main Experiment" 2 "Historical lower bound (2011-14)" )) xtitle("Proportion of farmers in sublocation who harvest with company") graphregion(color(white)) bgcolor(white)
				graph export "out/histogram_sideselling_sublocation.eps"	  , replace
	
				
exit
