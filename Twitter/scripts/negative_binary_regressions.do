global foundation "carevirtue carevice fairnessvirtue fairnessvice authorityvirtue authorityvice loyaltyvirtue loyaltyvice purityvirtue purityvice"

/// Negative binary regressions for retweets
foreach var in $foundation {
	display "Foundation: `var'. Party: Republicans."
	zinb retweets `var' followers if partyd == 0, inflate(followers) vce( cluster author ) zip nolog
	estimates store `var'_R, nocopy
	
	display "Foundation: `var'. Party: Democrats."
	zinb retweets `var' followers if partyd == 1, inflate(followers) vce( cluster author ) zip nolog
	estimates store `var'_D, nocopy
	
	display "Foundation: `var' interaction."
	zinb retweets followers partyd `var' c.`var'#i.partyd, inflate(followers) vce( cluster author ) zip nolog
}

/// Negative binary regressions for likes
foreach var in $foundation {
	display "Foundation: `var'. Party: Republicans."
	zinb favorites `var' followers if partyd == 0, inflate(followers) vce( cluster author ) zip nolog
	estimates store `var'_R, nocopy
	
	display "Foundation: `var'. Party: Democrats."
	zinb favorites `var' followers if partyd == 1, inflate(followers) vce( cluster author ) zip nolog
	estimates store `var'_D, nocopy
	
	display "Foundation: `var' interaction."
	zinb favorites followers partyd `var' c.`var'#i.partyd, inflate(followers) vce( cluster author ) zip nolog
}

/// Margins for retweets separately for Ds and Rs, write to CSV
foreach var in $foundation {
	estimates restore `var'_D 
	estpost margins, at(`var'=(.05(.05).35) followers=30547) vsquish
	esttab using margins-D.csv, cells("b ci_l ci_u") noobs append
	
	estimates restore `var'_R 
	estpost margins, at(`var'=(.05(.05).35) followers=30547) vsquish
	esttab using margins-R.csv, cells("b ci_l ci_u") noobs append
}
