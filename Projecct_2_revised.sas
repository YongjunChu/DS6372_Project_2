***upload the file (kobe);
proc import 
	    datafile='/home/ychu2/sasuser.v94/MSDS6372/project2Data_new.csv'
	    dbms=csv
    out=kobe1;
    guessingrows=26000;
run;

/*find all the variables having NA or missing data*/

/* create a format to group missing and nonmissing */
/*proc format;
 value $missfmt ' '='Missing' other='Not Missing';
 value  missfmt  . ='Missing' other='Not Missing';
run;*/

proc format;
 value $missfmt 'NA'='Missing' other='Not Missing';
 value  missfmt . ='Missing' other='Not Missing';
run;
 
proc freq data=kobe1; 
format _CHAR_ $missfmt.; /* apply format for the duration of this PROC */;
tables _CHAR_ / missing missprint nocum nopercent;
format _NUMERIC_ missfmt.;
tables _NUMERIC_ / missing missprint nocum nopercent;
run;

*no missing values were found for each column;

*Address the need for any potential transformations;
*Didn't find the need to do so;

*Address and identify outliers;
*We have found a couple of observations;
 
*Address and identify any multicollinearity;
*SAS Proc Logistic already took care of this;

*the "action_type" is overlapping with combined_shot_type, so only use action_type in the modeling;

***initial logistic model fitting with stepwise (team_id and team_name are removed);
ods graphics on;
proc logistic data=kobe1 DESCENDING;
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = shot_distance playoffs shot_distance*playoffs shot_distance*opponent action_type combined_shot_type matchup opponent season shot_type
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         avgnoisedb game_date game_event_id game_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining shot_id / selection=stepwise
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable; 
*   output out=kobeout p=probpreb;
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;

*the accuracy is 68.3% and HL test p-value is 0.3644;


***initial logistic model fitting with forward (team_id and team_name are removed);
ods graphics on;
proc logistic data=kobe1 DESCENDING;
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = shot_distance playoffs shot_distance*playoffs shot_distance*opponent action_type combined_shot_type matchup opponent season shot_type
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         avgnoisedb game_date game_event_id game_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining shot_id / selection=forward
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable; 
*   output out=kobeout p=probpreb;
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;

*the results from Forward is the same as that from stepwise;

***refined logistic model fitting by requiring to include shot_distance, playoffs and their interaction term (team_id and team_name are removed );
ods graphics on;
proc logistic data=kobe1 DESCENDING plots(MAXPOINTS=NONE only label)= (leverage dpc) outest=betas;
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = shot_distance playoffs shot_distance*playoffs shot_distance*opponent action_type combined_shot_type matchup opponent season shot_type
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         avgnoisedb game_date game_event_id game_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining shot_id / selection=stepwise
                  slentry=0.1
                  slstay=0.1
                  INCLUDE=3
                  details
                  lackfit ctable; 
*   output out=kobeout p=probpreb;
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;

*the identified significant variables are still 11, just like the above results/ the HL test p-value is 0.2093 and no quasi-complete separation occured;

proc print data=betas;
   title2 'Parameter Estimates and Covariance Matrix';
run;

***using 11 significant variables and shot_distance, playoffs and their interaction term to find estimates/stastics for each variable;
ods graphics on;
proc logistic data=kobe1 DESCENDING plots(MAXPOINTS=NONE only label)= (leverage dpc);
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = shot_distance playoffs shot_distance*playoffs action_type season 
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         game_event_id minutes_remaining
         period seconds_remaining / 
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable cl;
*   output out=kobeout p=probpreb;
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;


***using 11 significant variables and shot_distance, playoffs and their interaction term to get probability plots;
ods graphics on;
proc logistic data=kobe1 DESCENDING ;
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = shot_distance playoffs shot_distance*playoffs action_type season 
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         game_event_id minutes_remaining
         period seconds_remaining / 
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable cl;
                * effectplot slicefit; 
                * effectplot interaction(x=playoffs sliceby=shot_distance) / noobs link;
                  effectplot fit ( plotby=playoffs x=shot_distance);
                * effectplot interaction(x=shot_distance plotby=playoffs) / noobs link;
                  effectplot slicefit(sliceby=playoffs x=shot_distance)
*   output out=kobeout p=probpreb;
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;





***using 11 significant variables only for modeling and get the AUC, sensivity, LOSS for whole input dataset itself;
ods graphics on;
proc logistic data=kobe1 DESCENDING plots(MAXPOINTS=NONE only label)= (leverage dpc);
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = action_type season 
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         game_event_id minutes_remaining
         period seconds_remaining / 
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable cl; 
    output out=itselfout predprobs=(X I);
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;

*find the predicted outcomes for kobe1 dataset;
data itselfout1;
  set itselfout;
  keep shot_id shot_made_flag _into_ XP_1 XP_0;

run;

*export the database itselfout1 and then download it to local computer-itself folder for R analysis;

*get ROC curve for this case with 11 significant variables;
ods graphics on;
proc logistic data=kobe1 DESCENDING plots(only)=roc(  id=obs );
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = action_type season 
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         game_event_id minutes_remaining
         period seconds_remaining / 
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable cl; 
*   output out=itselfout predprobs=(X I);
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;



***predicting the test dataset pred from Dr.T;
proc import 
	    datafile='/home/ychu2/sasuser.v94/MSDS6372/project2Pred_new.csv'
	    dbms=csv
    out=pred;
    guessingrows=26000;
run;

*convert the shot_made_type to numeric in pred1 dataset;
data pred1;
set pred;
   shot_made_flag_1 = 10;
   drop shot_made_flag;
   rename shot_made_flag_1=shot_made_flag;
run;

data pred1;
set pred1;
   shot_made_flag = ".";
run;

*change the action_type in kobe1 to $36 so that it matches the length from pred1 dataset;
data kobe2;
  length action_type $36;
  format action_type $36.;
  set kobe1;
 
run;

*combine kobe2 and pred1;
data full;
set kobe2 pred1;
run;

*using 11 significant variables in the logistic model to predict the shot_made_type in pred1 dataset;
ods graphics on;
proc logistic data=full DESCENDING plots(MAXPOINTS=NONE only label)= (leverage dpc);
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = action_type season 
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         game_event_id minutes_remaining
         period seconds_remaining / 
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable; 
    output out=predout predprobs=(X I);
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;

*find the predicted outcomes for pred1 dataset;
data predout1;
set predout;
keep shot_id shot_made_flag _into_ IP_1 IP_0;
where shot_made_flag =.;
run;

data predout2;
set predout1;
pred=_into_;
drop _into_;
run;

*export the database predout2 and then download it to local computer-prediction folder;


***generate train and test datasets;
*create different datasets;
data Have;             /* the data to partition  */
   set kobe1;  /* for example, use Heart data */
run;
 
/* If propTrain + propValid = 1, then no observation is assigned to testing */
%let propTrain = 0.80;         /* proportion of trainging data */
%let propValid = 0.00;         /* proportion of validation data */
%let propTest = %sysevalf(1 - &propTrain - &propValid); /* remaining are used for testing */

*either use the following; 
/* Randomly assign each observation to a role; _ROLE_ is indicator variable */
data RandOut;
   array p[2] _temporary_ (&propTrain, &propValid);
   array labels[3] $ _temporary_ ("Train", "Validate", "Test");
   set Have;
   call streaminit(123);         /* set random number seed */
   /* RAND("table") returns 1, 2, or 3 with specified probabilities */
   _k = rand("Table", of p[*]); 
   _ROLE_ = labels[_k];          /* use _ROLE_ = _k if you prefer numerical categories */
   drop _k;
run;
 
proc freq data=RandOut order=freq;
   tables _ROLE_ / nocum;
run;

*or use these codes;
/* create a separate data set for each role */
data Train Validate Test;
array p[2] _temporary_ (&propTrain, &propValid);
set Have;
call streaminit(123);         /* set random number seed */
/* RAND("table") returns 1, 2, or 3 with specified probabilities */
_k = rand("Table", of p[*]);
if      _k = 1 then output Train;
else if _k = 2 then output Validate;
else                output Test;
drop _k;
run;

***check train dataset to find potential quasi-complete separation issues;
proc means data=train noprint nway;
      class action_type;
      var shot_made_flag;
      output out=kobe_freq mean=prop;
run;


***use train data to find the significant variables;
ods graphics on;
proc logistic data=train DESCENDING;
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = shot_distance playoffs shot_distance*playoffs shot_distance*opponent action_type combined_shot_type matchup opponent season shot_type
         shot_zone_area shot_zone_basic shot_zone_range arena_temp attendance
         avgnoisedb game_date game_event_id game_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining shot_id / selection=stepwise
                  slentry=0.1
                  slstay=0.1
                  details
                  lackfit ctable; 
*   output out=kobeout p=probpreb;
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;

*the HL test p-value of 0.5149, all steps were converged;

***testing by including the test dataset generated above;
*create a test dataset test1 without values for shot_made_flag;
data test1;
set test;
shot_made_flag = ".";
run;

*combine test_1 and train;
data combined;
set train test1;
run;

*using only significant varibles obtained from running train set to predict shot_made_type in test1 datset;
ods graphics on;
proc logistic data=combined DESCENDING plots(MAXPOINTS=NONE only label)= (leverage dpc) outest=betas;
   class action_type combined_shot_type matchup opponent season shot_type shot_zone_area
         shot_zone_basic shot_zone_range;

   model shot_made_flag (event='1') = action_type season
         shot_zone_area shot_zone_range arena_temp attendance
         game_event_id seconds_remaining shot_zone_basic minutes_remaining period /
                  details
                  lackfit ctable; 
                *  oddsratio shot_distance;
                *  oddsratio playoffs;                                  
    output out=kobeout predprobs=(X I);
*   oddsratio Heat / at(Soak=1 2 3 4);
run;
ods graphics off;

***find the predicted outcomes for test dataset;
data kobeout1;
set kobeout;
*Predicted = probpreb;
keep shot_id _from_ _into_ IP_1 IP_0;
where shot_made_flag =.;
run;

*export the database kobeout1 and then download it to local computer and find the accuracy in R;
*export the database test and then download it to local computer for comparison;

***using LDA for classification;
proc discrim data=train pool=yes crossvalidate testdata=test;
class shot_made_flag;
var shot_distance arena_temp attendance 
         avgnoisedb game_date game_event_id game_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining shot_id;
run;

*only using continuous variables which are quantitative variables and may follow normal distribution for modeling;
proc discrim data=train pool=test crossvalidate testdata=test  testout=tout testoutd=toutd   ;
class shot_made_flag;
var shot_distance arena_temp attendance
         avgnoisedb game_event_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining;     
run;

*export the database tout and then download it to local computer and find the accuracy in R;


*using non-normal method for modeling;
proc discrim data=train method=npar kernel=normal
             r=.5 pool=yes crossvalidate testdata=test   testout=tout1;
class shot_made_flag;
var shot_distance arena_temp attendance 
         avgnoisedb playoffs game_date game_event_id game_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining;  
run;

proc discrim data=train method=npar kernel=normal
             r=.5 pool=yes crossvalidate testdata=test   testout=tout1;
class shot_made_flag;
var shot_distance arena_temp attendance
         avgnoisedb game_event_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining;
run;



*get the parameters for the LDA function;
proc discrim data=train ;
class shot_made_flag;
var shot_distance arena_temp attendance
         avgnoisedb game_event_id lat loc_x loc_y lon minutes_remaining
         period seconds_remaining;    
run;
