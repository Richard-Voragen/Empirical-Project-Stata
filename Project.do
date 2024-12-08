import delimited pppub24.csv, clear
keep a_age a_hga pearnval pefntvty pemntvty prcitshp a_lfsr a_untype a_wkslk prdtrace
// age
// educational attainment (<39 = no high school, 39 = highschool, 41-42 = associates, 43 = bachelors, 44 = masters, 45-46 = MD/PHD)
// total earnings
// Country father was born in (57 is US)
// country mother was born in (57 is US)
// citizenship status (1 = native, 2 = born in PR or outlying area, 3 = born abroad with US parents, 4 = foreign, gained citizenship, 5 = not citizen)
// labor status (0 = child, 1 = working, 2 = with job, not at work, 3 = unemployed but looking, 4 = unemployed on layoff)
// unemployed type (1-2 = job loser/layoff, 3 = left job, 4 = Re-entrant, 5= New-entrant)
// Duration of unemployment (weeks)
// Race (1= white, 2= black, 3= amer indian, 4= asian, 5= pacific island, 06 = White-Black, 07 = White-AI, 08 = White-Asian, 09 = White-HP, 10 = Black-AI, 11 = Black-Asian, 12 = Black-HP, 13 = AI-Asian, 14 = AI-HP, 15 = Asian-HP, 16 = White-Black-AI, 17 = White-Black-Asian, 18 = White-Black-HP, 19 = White-AI-Asian, 20 = White-AI-HP, 21 = White-Asian-HP, 22 = Black-AI-Asian, 23 = White-Black-AI-Asian, 24 = White-AI-Asian-HP, 25 = Other 3 race comb., 26 = Other 4 or 5 race comb.)

drop if pearnval <= 10000
drop if prdtrace >= 6
drop if a_age > 65

gen citizenship_status = cond(prcitshp <= 2, cond(pefntvty != 57, cond(pemntvty != 57, 1, 2), 2), cond(prcitshp == 3, 2, 0))
gen ordered_race_data = cond(prdtrace==3, 1, cond(prdtrace==5, 2, cond(prdtrace==2, 3, cond(prdtrace==1, 4, cond(prdtrace==4, 5, 0)))))

gen race_data_named = cond(prdtrace==5, "Pacific Island", cond(prdtrace==3, "Black", cond(prdtrace==2, "American Indian", cond(prdtrace==1, "White", cond(prdtrace==4, "Asian", "Other")))))
gen citizenship_status_named = cond(citizenship_status == 0, "Non-citizen", cond(citizenship_status == 1, "First Generation American", "Second+ Generation American"))

graph bar (median) pearnval, over(race_data_named)
graph export raceToIncome.png, replace
graph bar (median) pearnval, over(citizenship_status_named)
graph export citizenshipToIncome.png, replace

// Create a dummy variable for education
gen college = a_hga >= 41
gen age2 = a_age * a_age

// Separate the races into different dummy variables
gen black = prdtrace==3
gen white = prdtrace==1
gen asian = prdtrace==4
gen american_indian = prdtrace==2
gen pacific_island = prdtrace==5

reg pearnval black asian american_indian pacific_island
estimates store table1, title(Table 1)
estout table1 using table1.doc, cells("Coefficient Std_Err t_value p_value") replace

// Separate the citizenship status
gen non_citizen = citizenship_status == 0
gen first_gen_citizen = citizenship_status == 1
gen secondPlus_gen_citizen = citizenship_status == 2

// Use regression models to analyze the data to predict income
// compared to a white, 1st gen citizen
reg pearnval black asian american_indian pacific_island college a_age age2
reg pearnval black asian american_indian pacific_island non_citizen secondPlus_gen_citizen college a_age age2
estimates store table2, title(Table 2)
esttab table2 using table2.doc, cells("Coefficient Std_Err t_value p_value") replace
predict income_estimate


graph bar income_estimate if (college==0), over(citizenship_status_named) over(race_data_named) asyvar blabel(total, format(%9.0f) size(medsmall)) bargap(10) xsize(12) ytitle("Predicted Income") plotregion(fcolor(white)) graphregion(fcolor(white)) legend(col(1)) title("Predicted Income By Race and Citizenship for Non-College Educated Americans")
graph export incomePredictionsUneducatedFigure2.png, replace
graph bar income_estimate if college, over(citizenship_status_named) over(race_data_named) asyvar blabel(total, format(%9.0f) size(medsmall)) bargap(10) xsize(12) ytitle("Predicted Income") plotregion(fcolor(white)) graphregion(fcolor(white)) legend(col(1)) title("Predicted Income By Race and Citizenship for College Educated Americans")
graph export incomePredictionsEducatedFigure3.png, replace

**# Bookmark #2

// Now we will measure the probabilities of earning above the median income for each group
sum pearnval, detail
gen above_median_income = pearnval > 54800

gen above_median_income_percentage = above_median_income*100
graph bar above_median_income_percentage, over(citizenship_status_named) over(race_data_named) asyvar blabel(total, format(%9.1f) size(medsmall)) bargap(10) xsize(12) ytitle("Percentage above median income (%)") plotregion(fcolor(white)) graphregion(fcolor(white)) legend(col(1)) title("Odds of being above Median Income By Race and Citizenship")
graph export wealthyPredictions.png, replace
drop above_median_income_percentage

// Use probit compared to a first_gen_citizen
probit above_median_income non_citizen secondPlus_gen_citizen
predict above_pred_firstgen
sum above_pred_firstgen if first_gen_citizen
gen relative_pred_gen = (above_pred_firstgen - 0.4449717) * 100
graph bar relative_pred_gen, over(citizenship_status_named) asyvar blabel(total, format(%9.1f) size(medsmall)) bargap(10) xsize(12) ytitle("Relative percentage compared to first_gen_citizen (%)") plotregion(fcolor(white)) graphregion(fcolor(white)) legend(col(1)) title("Relative Percentages based on Citizenship (Compared to a First Generation American)")
graph export relativeToGen.png, replace
drop above_pred_firstgen
drop relative_pred_gen

// Use probit compared to a white American
probit above_median_income black asian american_indian pacific_island
predict above_pred_white
sum above_pred_white if white
gen relative_pred_race = (above_pred_white - 0.5054932) * 100
graph bar relative_pred_race, over(race_data_named) asyvar blabel(total, format(%9.1f) size(medsmall)) bargap(10) xsize(12) ytitle("Relative percentage compared to white (%)") plotregion(fcolor(white)) graphregion(fcolor(white)) legend(col(1)) title("Relative Percentages based on Race (Compared to a White American)")
graph export relativeToRace.png, replace
drop above_pred_white
drop relative_pred_race

// Use probit compared to a white first_gen_citizen
probit above_median_income black asian american_indian pacific_island non_citizen secondPlus_gen_citizen
estimates store table3, title(Table 3)
esttab table3 using table3.doc, cells("Coefficient Std_Err t_value p_value") replace
predict above_pred_first_gen_white

sum above_pred_first_gen_white if white & first_gen_citizen
gen relative_pred_gen_race = (above_pred_first_gen_white - 0.4111186) * 100
graph bar relative_pred_gen_race, over(citizenship_status_named)over(race_data_named) asyvar blabel(total, format(%9.1f) size(medsmall)) bargap(10) xsize(12) ytitle("Relative percentage to white first_gen_citizen (%)") plotregion(fcolor(white)) graphregion(fcolor(white)) legend(col(1)) title("% Chance of Having Over Median Income (Compared to a White First Generation American)")
graph export relativeToGenRaceFigure1.png, replace
drop above_pred_first_gen_white
drop relative_pred_gen_race
