$title  least cost coastal adaptation for SLR
$eolcom #
$onempty

$if not set usesubset $set usesubset yes
$if not set subset $set subset test
$if not set SLR $set SLR off
$if not set mode $set mode fixed
$if not set rcp $set rcp rcp85
$if not set pctile $set pctile 50
$if not set sens $set sens ref
$if not set land $set land fund
$if %sens%=='gtapland' $set land gtap
$if %SLR%=='off' $set rcp rcp0

* Only return positive values
$macro pos(x) max(0,x)
* Compute annual national growth rate between periods
$macro gr(x) (x(t,country)/(x(t-1,country)+epsilon)-1)
* Map country to segment
$macro segment(x)  sum(xsc(seg,country), x)


* Time periods
set
t  		time periods of model horizon in 10 yr time steps (1=2010) /0*20/
at(t)		adaptation planning periods (default is 40-50 years) /0,1,5,10,15,19/
;
alias(t,tt);

* Map time periods t to adaptation periods at
* Adjust this section if the planning horizon changes
set xtat(t,at) / 0:0
1:1, 2:1, 3:1, 4:1 # i.e., t-periods 1 to 4 map to at-period 1
5:5, 6:5, 7:5, 8:5, 9:5
10:10, 11:10, 12:10,13:10, 14:10
15:15, 16:15, 17:15, 18:15
19:19, 20:19/;


parameter
dr			discount rate /0.04/
dfact(t)		discount factor
tstept			length of timestep between period t /10/
tstep(at) 		length of adaptation timestep;
$if %sens%=='dr3' dr=0.03;
$if %sens%=='dr5' dr=0.05;
dfact(t)$(t.val>0) = 1/(1+dr)**(tstept*(t.val-1)); dfact('0')=1;
tstep(at) = tstept*sum(xtat(t,at),1);
$set mapt 5 # report results for mapping - must be in set 'at'


* Segment characteristics
set
seg			DIVA segment id
segi(seg)		selected i DIVA segment
greenland(seg)		subset of segments that are Greenland (not Denmark)
island(seg)		subset of segments that are islands
highlat(seg)		subset of segments that are over 70 deg latitude
country			country ISO 3 letter code
xsc(seg,country)	segment to country mapping
fund			FUND region
xrg(country,fund)	country to FUND region mapping
attribute		segment attributes
;
alias(seg,l);

* all slr measurements are for the start of time period t
* area in sqkm, pop in people, popdens in ppl/sqkm, US $2010
parameters
data(seg,attribute)	output segment characteristics of interest
ypcc(t,country)		GDP per capita ($2010 per capita) - country basis
ypc(t,seg)		GDP per capita ($2010 per capita) - segment basis
ypc_scale(seg)		adjust naional ypc average to coastal segments
pop(t,country)		MERGE Population (Million people)
ym(t,country)		MERGE GDP (Billion $US 2010)
gdp(t)			global GDP (Billion $US 2010) (MERGE baseline)
pc0			reference cost of protection ($2010 M per km per vert m squared) /6.02/
cci(country)		construction cost index from World Bank 2011
dvbm			FUND value of OECD dryland per Darwin et al 1995 converted from $1995 4M ($2010M per sqkm) /5.376/
wvbm			annual value of wetland services (Brander et al 2006) converted from $1995 0.28M ($2010M per sqkm per year) /0.376/
gtapland(country)	land value in 2007 from GTAP ($2010M per sq km)
fundland(country) 	land value in 1995 from FUND ($2010M per sq km)
countryarea(country)	country area from GIM (sq km)
coastland_scale(t,seg)
refpopdens(country)	reference population density per country
appr(t,country)		land values appreciate over time based on a regression per Yohe ref Abraham and Hendershott
popdens(t,seg)		population density that grows over time (ppl per sqkm)
interior(t,seg)		total endowment value of land per GTAP or FUND ($2010M per sqkm)
coastland(t,seg)	total endowment value of land adjusted for coast ($2010M per sqkm)
landval(t,seg)		specify total endowment value to use for inundation costs (specificy interior or coastal social vs private)
landrent(t,seg)		specify annual rental value of land ($2010M per sqkm per year)
wetlandservice(t,seg)	annual value of wetland services (Brander et al 2006) adjusted for income and density per FUND ($2010M per sqkm per year)
wmaxrate		maximum rate of wetland accretion (m per yr) per Kirwan et al 2010 /0.01/
capital(t,seg)		total endowment value of capital stock ($2010M per sqkm)
pc(seg)			cost of protection per segment ($M per km per vert m squared)
mobcapfrac		Fraction of capital stock that is mobile /0.25/
democost		Cost to demolish immobile capital as fraction of asset /0.05/
pcfixedfrac		Fraction of protection cost that is fixed (not variable in height) /0.3/
capmovefactor		cost to relocate mobile capital as a fraction of asset value /0.1/
movefactor		cost to relocate people as a factor of annual income (Tol 3x RMendelsohn 0.5x) /1/
rho(t,country)          resilience factor (logistic function relating ypc to US)
floodmortality		floods take 1 in 100 people exposed (per Jonkman Vrijling 2008 Table4) resiliency can reduce this /0.01/
vsl(t,seg)		value of a statistical life ($2010M)
epsilon			arbitrary small unit to avoid division by zero errors /1e-9/
;

$if %sens%=='wetmax5mm' wmaxrate=0.005;

* load data from other sources
$gdxin data/CIAMdata
$if %usesubset%=='no' $load seg, attribute, data, country, xsc
$if %usesubset%=='yes' $load seg=segsub%subset%, attribute, data=datasub%subset%, country, xsc
$load ypcc=ypc, pop, ym, cci, gtapland, fund, xrg, countryarea, refpopdens, greenland, island, highlat


* SLR parameters
* all slr measurements are for the start of time period t
set
rcp_pt			RCP scenarios by percentile
;
parameters
gsl, slr(t)		global mean sea level reached by the start of period t per Kopp et al 2014 (in m)
lsl, lslr(t,seg)	local sea level reached by the start of period t per Kopp et al 2014 (in m)
lslrPlan(at,seg)	local sea level that adaptation is PLANNED for in period at (m)
localrate(t,seg)	rate of local sea level rise at segment seg (m per yr)
;
$load rcp_pt, gsl, lsl
* Note LSLR can be negative
lslr(t,seg)= lsl(t,seg,'%rcp%_p%pctile%');
slr(t)= gsl(t,'%rcp%_p%pctile%');
* Set level of LSLR (in m) that adaptation will be planned for
lslrPlan(at,seg)=lslr(at+1,seg); lslrPlan(at,seg)$(ord(at)=card(at))=sum(t$(ord(t)=card(t)),lslr(t,seg));

* LSRL rate in m/yr (used for wetlands) negative rates discarded, tstept is yrs to the next period
localrate(t,seg)=pos(lsl(t+1,seg,'%rcp%_p%pctile%')-lsl(t,seg,'%rcp%_p%pctile%'))/tstept;
localrate(t,seg)$(ord(t)=card(t))=localrate(t-1,seg);

* Population density (ppl per sqkm) grows over time based on country growth rate
popdens('1',seg) = data(seg,'popdens'); loop(t$(t.val>1), popdens(t,seg) = popdens(t-1,seg)*segment(1+gr(pop)));

* Income per capita can differ between seg and country based on population density
ypc_scale(seg) = max(0.9,(popdens('1',seg)/250)**0.05);
ypc(t,seg) = segment(ypcc(t,country))*ypc_scale(seg);
gdp(t)=sum(country, ym(t,country));
* Correct income for Greenland per World Bank $2010USD
ypc(t,greenland) = 22642*1.01**t.val;

* All land, capital values below in $M/sqkm

* Land values reflect annual rents
* GTAP rents order of mag lower than FUND
* Yohe cites Cline92 dryland=$4000/acre=$1M/sqkm, wetland=$10000/acre=$2.5M/sqkm
* FUND OECD land value used to compute national land values - assume linear in income density relative to USA
fundland(country) = min(dvbm,max(0.005,dvbm*ypcc('1',country)*refpopdens(country)/(ypcc('1','USA')*refpopdens('USA')))); #$M/sqkm per FUND

* National land values appreciate over time based on a regression per Yohe ref Abraham and Hendershott
appr('1',country) = 1; loop(t$(t.val>1), appr(t,country) = appr(t-1,country)*exp(0.565*gr(ypcc) + 0.313*gr(pop)); );


* Use national data on baseland value and ypc, and segment characteristics of popdens
interior(t,seg) = segment(appr(t,country)*%land%land(country));
* assume that Greenland is like Canadian land
interior(t,greenland) = appr(t,'CAN')*%land%land('CAN');

* stylized assumption that populated coastal land more valuable, as much as 2.4x when = 2000ppl/sqkm (1 at 25ppl/sqkm, 0.75 at 10ppl/sqkm)
coastland_scale(t,seg) = max(0.5,log(1+popdens(t,seg))/log(25));
coastland(t,seg) = interior(t,seg) * coastland_scale(t,seg);

landval(t,seg) = min(coastland(t,seg),interior(t,seg)); # This ensures unpopulated segments are not protected simply because of high interior land values

* Use a special case for the private landowner
$if %sens%=='private' landval(t,seg) = coastland(t,seg);

* Sensitivity case that land is twice as valuable
$if %sens%=='land2x' landval(t,seg) = 2*landval(t,seg);
$if %sens%=='worst' landval(t,seg) = 2*landval(t,seg);

* Convert all land values to annual rent - CIAM damages computed as lost rents based on dr flow, 4% per Sohngen)
landrent(t,seg) = landval(t,seg)*0.04;

* Current national value of wetland services as an annual flow per Brander et al 2006
wetlandservice('1',seg) = wvbm*segment((ypcc('1',country)/ypcc('1','USA'))**1.16*(refpopdens(country)/27.59)**0.47);
loop(t$(t.val>1), wetlandservice(t,seg) = segment(appr(t,country))*wetlandservice('1',seg););

* Estimate capital stock from a capital output ratio of 3
* MERGE K-GDP ratio is 2.4-3, R. Mendelsohn uses 5x
* $ per capita * ppl/sqkm * 1e-6 = $M/sqkm
parameter kgdp capital output ratio /3/;
$if %sens%=='kgdp5' kgdp=5;
capital(t,seg) = kgdp * ypc(t,seg)*popdens(t,seg)*1e-6;

* VSL is 200x ypc in US per FUND per Cline 1992, US assumes $9.1M which is 216x ypc; income elasticity formulation per Viscusi and Aldy 2003
vsl(t,seg)= 1e-6 * segment(216*ypcc(t,'USA')*(ypcc(t,country)/ypcc(t,'USA'))**0.5);
vsl(t,greenland)= 1e-6 * 216*ypcc(t,'USA')*(ypc(t,greenland)/ypcc(t,'USA'))**0.5;

* resilience factor between 0 and 1, increasing in ypc - rho=1 means no storm costs
* assume logistic function where 50% resilience is reached at half of current US ypc
rho(t,country) = ypcc(t,country)/(ypcc(t,country)+ypcc('1','USA'));



$if %sens%=='pc2x' pc0=2*pc0;
$if %sens%=='worst'  pc0=2*pc0;
* pc0=6.02 based on Hillen et al 2010 review of coastal adaptation costs (CLIMSAVE.xls)
* Linham et al. (2010) 1 km of seawall is in the range of $0.4 to 27.5 M, FUND has $4.8M for US but much cheaper for other regions (Calculated from $95.3B over 19,924 km coastline)
* Yohe $4000/ft high ($750/ft low) is $13.1M/km (low $2.46M)
* Local protection costs depend on country ($M per km per vert m squared)
pc(seg) =  pc0 * segment(cci(country));
* Cost of construction on an island will be higher and this is missing from CCI table
pc(island) =  2*pc(island);

* Coastal slope accounting
* Area parameters give sqkm per 1m of vertical elevation
* This defines a function to return the linear piecewise interpolation of inundation area (accounts for coastal curvature) and smoothes the kink at 1m elevation by averaging coast between 0.5 and 1.5 m elevation
$macro coastarea(y,t,seg) (data(seg,'area1')*max(0,min(0.5,y(t,seg)-0))+(data(seg,'area1')+data(seg,'area2'))/2*max(0,min(1,y(t,seg)-0.5))+data(seg,'area2')*max(0,min(0.5,y(t,seg)-1.5))+data(seg,'area3')*max(0,min(1,y(t,seg)-2))+data(seg,'area4')*max(0,min(1,y(t,seg)-3))+data(seg,'area5')*max(0,min(1,y(t,seg)-4))+data(seg,'area6')*max(0,min(1,y(t,seg)-5))+data(seg,'area7')*max(0,min(1,y(t,seg)-6))+data(seg,'area8')*max(0,min(1,y(t,seg)-7))+data(seg,'area9')*max(0,min(1,y(t,seg)-8))+data(seg,'area10')*max(0,min(1,y(t,seg)-9))+data(seg,'area11')*max(0,min(1,y(t,seg)-10))+data(seg,'area12')*max(0,min(1,y(t,seg)-11))+data(seg,'area13')*max(0,min(1,y(t,seg)-12))+data(seg,'area14')*max(0,min(1,y(t,seg)-13))+data(seg,'area15')*max(0,y(t,seg)-14))
$macro coastareaA(y,t,seg,cases) (data(seg,'area1')*max(0,min(0.5,y(seg,t,cases)-0))+(data(seg,'area1')+data(seg,'area2'))/2*max(0,min(1,y(seg,t,cases)-0.5))+ data(seg,'area2')*max(0,min(0.5,y(seg,t,cases)-1.5))+data(seg,'area3')*max(0,min(1,y(seg,t,cases)-2))+data(seg,'area4')*max(0,min(1,y(seg,t,cases)-3))+data(seg,'area5')*max(0,min(1,y(seg,t,cases)-4))+data(seg,'area6')*max(0,min(1,y(seg,t,cases)-5))+data(seg,'area7')*max(0,min(1,y(seg,t,cases)-6))+data(seg,'area8')*max(0,min(1,y(seg,t,cases)-7))+data(seg,'area9')*max(0,min(1,y(seg,t,cases)-8))+data(seg,'area10')*max(0,min(1,y(seg,t,cases)-9))+data(seg,'area11')*max(0,min(1,y(seg,t,cases)-10))+data(seg,'area12')*max(0,min(1,y(seg,t,cases)-11))+data(seg,'area13')*max(0,min(1,y(seg,t,cases)-12))+data(seg,'area14')*max(0,min(1,y(seg,t,cases)-13))+data(seg,'area15')*max(0,y(seg,t,cases)-14))

* Make sure t=0 can be evaluated
landrent('0',seg)=landrent('1',seg);
capital('0',seg)=capital('1',seg);
popdens('0',seg)=popdens('1',seg);
ypc('0',seg)=ypc('1',seg);
rho('0',country)=rho('1',country);
vsl('0',seg)=vsl('1',seg);
wetlandservice('0',seg)=wetlandservice('1',seg);


***************************** Adaptation Decisions *****************************
* This solves for the least cost fixed strategy with perfect foresight
$if not set depr $set depr 1
$if %sens%=='nodepr' $set depr 0.35
$if %sens%=='worst' $set depr 0.35

set
rlist			/retreat1, retreat10, retreat100, retreat1000, retreat10000/
plist			/protect10, protect100, protect1000, protect10000/
cases			/noAdaptation, fullRetreat, fullProtect, set.rlist, set.plist, optimal%mode%/
retreatGrid(cases)	/set.rlist/
protectGrid(cases)	/set.plist/
adaptGrid(cases)	/set.retreatGrid, set.protectGrid/
solved(cases)		/noAdaptation, set.adaptGrid/
results(cases)		/noAdaptation, fullRetreat, fullProtect, optimal%mode%/
costtype		/inundation, relocation, protection, storms, stormCapital, stormPopulation, wetland, total/
opttype			/optimal%mode%/
;

* These parameters are the decision variables (evaluated brute force)
parameter
H(seg,t,cases)			Height of current sea wall only (m) not including any retreat
R(seg,t,cases)			Retreat perimeter (height m)
SIGMA(seg,t,cases)		Expected value of effective exposure area (sq km) for an overtopping surge
Costs(cases,seg,t,costtype)	Cost by type ($M)
;


* Run no adaptation (abandon) case, where retreat perimeter equals SLR at start of time t
H(seg,t,'noAdaptation')=0;
R(seg,t,'noAdaptation')=lslr(t,seg);

* Run retreat cases - retreat once for the SLR planning horizon
* In very long time horizons this is unduly pessimistic because retreat can always be incremental driven by SLR
H(seg,at,retreatGrid)    = 0;
R(seg,at,'retreat1')     = lslrPlan(at,seg);
R(seg,at,'retreat10')    = lslrPlan(at,seg)+data(seg,'s10');
R(seg,at,'retreat100')   = lslrPlan(at,seg)+data(seg,'s100');
R(seg,at,'retreat1000')  = lslrPlan(at,seg)+data(seg,'s1000');
R(seg,at,'retreat10000') = lslrPlan(at,seg)+data(seg,'smax');


* Run protect case - protection must be built for the SLR planning horizon
R(seg,at,protectGrid)    = 0;
H(seg,at,'protect10')    = lslrPlan(at,seg)+data(seg,'s10')/2; # correct for how quickly sigma drops off
H(seg,at,'protect100')   = lslrPlan(at,seg)+data(seg,'s100');
H(seg,at,'protect1000')  = lslrPlan(at,seg)+data(seg,'s1000');
H(seg,at,'protect10000') = lslrPlan(at,seg)+data(seg,'smax');


* Do not adapt to a negative height (in case of sea level fall)
H(seg,t,cases)=pos(H(seg,t,cases)); R(seg,t,cases)=pos(R(seg,t,cases));


* Credit reference coastal measures in place from initial state (as computed by CIAM) for the noAdaptation case - can always do better by adapting
$if %SLR%=='on'  set A /R, H/; parameter refA(seg,A);
$if %SLR%=='on'  $gdxin output/noSLRreference/noSLRref%subset%
$if %SLR%=='on'  $load refA
$if %SLR%=='on'  R(seg,t,'noAdaptation')=max(refA(seg,'H'),max(refA(seg,'R'),R(seg,t,'noAdaptation')));

***************** Compute costs, all in $M for all costs over the timestep of period, Assume that 25% of capital is mobile, 75% immobile **************************
* Inundation cost - lose the land rent of all inundated (or vacated) land plus immobile capital on incremental area (between lslr_t and at-1) that has not been depreciated over 30 yrs (e.g., noAdaptation)
* incremental retreat incurs relocation costs (price per person is based on each relocated person's ypc and cost to move mobile capital (25%) is 25% asset, cost to demolish immobile capital (75%) is 5% asset)


***************************** No Adaptation Impacts *****************************
* costs incurred over the course of period t, for SLR from t to t+1, and must account for how much retreat has been done already
loop(t,
Costs('noAdaptation',seg,t,'inundation') = tstept*landrent(t,seg)*pos([coastarea(lslr,t+1,seg)]) + (pos([coastarea(lslr,t+1,seg)]) - pos([coastarea(lslr,t,seg)]))*(1-mobcapfrac)*capital(t,seg);
Costs('noAdaptation',seg,t,'relocation') = (pos([coastarea(lslr,t+1,seg)]) - pos([coastarea(lslr,t,seg)])) * [5*movefactor*ypc(t,seg)*1e-6*popdens(t,seg) + capmovefactor*mobcapfrac*capital(t,seg) + democost*(1-mobcapfrac)*capital(t,seg)];
); # end loop over t
Costs('noAdaptation',seg,t,costtype)$(ord(t)=card(t)) = Costs('noAdaptation',seg,t-1,costtype);
* SIGMA gives EV of effective exposure area (sq km) for an overtopping surge (after accounting for flood depth damage function but before resilience (rho) of capital and people)
SIGMA(seg,t,'noAdaptation')=data(seg,'rsig0')/( 1+data(seg,'rsigA')*exp(data(seg,'rsigB')*pos(R(seg,t,'noAdaptation')-lslr(t,seg))) );


* Note that no adaptation can have lower inundation costs than retreat because the affected area is very small in each period
* Ad cases: All costs up front incurred over the course of period at, then spread over at (eg, [tstept/tstep(at)]) and assigned to period t (eg, sum(xtat(t,at),)
loop(at,
***************************** Retreat Costs *****************************
* Inundation cost - lose the land rent of all inundated (or vacated) land plus immobile capital on incremental area (between lslr_t and at-1) that has not been depreciated over 30 yrs (e.g., noAdaptation)
* No inundation if protecting
Costs(retreatGrid,seg,t,'inundation')$xtat(t,at) = [tstept/tstep(at)]*sum(xtat(t,at), tstep(at)*landrent(at,seg)*[coastareaA(R,at,seg,retreatGrid)] + pos([coastareaA(R,at,seg,retreatGrid)] - [coastareaA(R,at-1,seg,retreatGrid)])*(1-%depr%)*(1-mobcapfrac)*capital(at,seg));

* incremental retreat incurs relocation costs (price per person is based on each relocated person's ypc and cost to move mobile capital (25%) is 25% asset, cost to demolish immobile capital (75%) is 5% asset)
Costs(retreatGrid,seg,t,'relocation')$xtat(t,at) = [tstept/tstep(at)]*sum(xtat(t,at), pos([coastareaA(R,at,seg,retreatGrid)] - [coastareaA(R,at-1,seg,retreatGrid)]) * [movefactor*ypc(at,seg)*1e-6*popdens(at,seg) + capmovefactor*mobcapfrac*capital(at,seg) + democost*(1-mobcapfrac)*capital(at,seg)]);


***************************** Protection Costs *****************************
* Protection costs could eventually adjust for concrete supply or endogenous learning  e.g., ((H(seg,at,solved)+q0)/q0)**(log(1-lr)/log(2));
Costs(protectGrid,seg,t,'protection')$xtat(t,at) =
* total costs over the planning period are spread evenly across time periods
	[tstept/tstep(at)] * sum(xtat(t,at),
* fixed cost of construction occurred each time, variable cost is quadratic in height, charged for increment since last installation, maintenance of 2% per Hillen et al 2010
	data(seg,'length')*pc(seg)*[pcfixedfrac + (1-pcfixedfrac)*(H(seg,at,protectGrid)**2-H(seg,at-1,protectGrid)**2) + 0.02*tstep(at)*H(seg,at,protectGrid)] +
* lose half of land rents, assume 60deg slope = 3/tan60
	data(seg,'length')*1.7*H(seg,at,protectGrid)*landrent(at,seg)/2*tstep(at)
	);

***************************** SIGMA *****************************
* SIGMA gives EV of effective exposure area (sq km) for an overtopping surge (after accounting for flood depth damage function but before resilience (rho) of capital and people)
SIGMA(seg,t,retreatGrid)$xtat(t,at)=data(seg,'rsig0')/( 1+data(seg,'rsigA')*exp(data(seg,'rsigB')*pos(R(seg,at,retreatGrid)-lslr(t,seg))) );
SIGMA(seg,t,protectGrid)$xtat(t,at)=(data(seg,'psig0')+data(seg,'psig0coef')*pos(lslr(t,seg)))/( 1+data(seg,'psigA')*exp(data(seg,'psigB')*pos(H(seg,at,protectGrid)-lslr(t,seg))) );


*****************************
); # end loop over at

* Sensitivity case - what if relocation costs are twice as costly
$if %sens%=='retreat2x' Costs(retreatGrid,seg,t,'relocation')=2*Costs(retreatGrid,seg,t,'relocation');
$if %sens%=='worst' Costs(retreatGrid,seg,t,'relocation')=2*Costs(retreatGrid,seg,t,'relocation');

***************************** Wetland Impacts *****************************
* Assume that if protection is chosen then wetlands are lost, regardless of the SLR scenario
Costs(protectGrid,seg,t,'wetland') = tstept*data(seg,'wetland')*wetlandservice(t,seg); # sqkm * $M/sqkm

* If no protection, then some fraction of inundated wetlands can accrete if local rate of slr less than max
* Fractional loss of wetlands due to rate
$macro wetlandloss(t,seg) min(1, (localrate(t,seg)/wmaxrate)**2)
* Exposed wetland area limited to current rise
$macro exposedwetlandarea(y,t,seg) min(coastarea(lslr,t,seg),data(seg,'wetland'))

Costs(retreatGrid,seg,t,'wetland') =    tstept*wetlandservice(t,seg)*wetlandloss(t,seg)*exposedwetlandarea(y,t,seg);
Costs('noAdaptation',seg,t,'wetland') = tstept*wetlandservice(t,seg)*wetlandloss(t,seg)*exposedwetlandarea(y,t,seg);



***************************** Storm Impacts *****************************

* Think of storm cost as the fair cost of insurance -- tstep times the annual expected damage
* Capital losses in expected flooded area (10 years at resilience * $M/sqkm per year * expected sqkm
Costs(solved,seg,t,'stormCapital') = tstept*(1-segment(rho(t,country)))*capital(t,seg)*SIGMA(seg,t,solved);
* Population losses in expected flooded area (10 years at resilience * ppl/sqkm * $M/ppl per year * 0.01 * expected sqkm
Costs(solved,seg,t,'stormPopulation') = tstept*(1-segment(rho(t,country)))*popdens(t,seg)*vsl(t,seg)*floodmortality*SIGMA(seg,t,solved);

Costs(solved,seg,t,'storms') = Costs(solved,seg,t,'stormCapital') + Costs(solved,seg,t,'stormPopulation');



***************************** Total Impacts *****************************
Costs(solved,seg,t,'total')=Costs(solved,seg,t,'inundation')+Costs(solved,seg,t,'relocation')+Costs(solved,seg,t,'protection')+Costs(solved,seg,t,'storms')+Costs(solved,seg,t,'wetland');




$label min
parameter minvalue, NPV(solved), minR(retreatGrid), minP(protectGrid);

loop(seg,
minvalue=0;
* Don't count t=0 because those represent the transition to the current state of adaptation - will address in a separate paper
NPV(solved) = sum(t$(t.val>0), dfact(t)*Costs(solved,seg,t,'total'));
* skip full protect and retreat cases for no SLR reference
$if %SLR%=='off' $goto leastcost

* find the least cost protect case
minvalue=smin(sameas(protectGrid,solved), NPV(solved));
loop(sameas(protectGrid,solved)$(NPV(solved)=minvalue),
Costs('fullProtect',seg,t,costtype) = Costs(solved,seg,t,costtype);
H(seg,at,'fullProtect') = H(seg,at,solved);
);

* find the least cost retreat case
minvalue=smin(sameas(retreatGrid,solved), NPV(solved));
loop(sameas(retreatGrid,solved)$(NPV(solved)=minvalue),
Costs('fullRetreat',seg,t,costtype) = Costs(solved,seg,t,costtype);
R(seg,at,'fullRetreat') = R(seg,at,solved);
);

$label leastcost
* find the least cost adaptation case (either retreat or protect or no adaptation)
minvalue=smin(solved, NPV(solved));

loop(solved$(NPV(solved)=minvalue),
Costs('optimal%mode%',seg,t,costtype) = Costs(solved,seg,t,costtype);
R(seg,at,'optimal%mode%') = R(seg,at,solved);
H(seg,at,'optimal%mode%') = H(seg,at,solved);
);


); #end loop over segment for ex-post comparisons

* put all costs into $Billions from $M and divide by timestep to show annual costs
Costs(cases,seg,t,costtype) = 1e-3*Costs(cases,seg,t,costtype)/tstept;

**********************************************************

* Save ref baseline for all sens cases $if %SLR%=='off'
$if %SLR%=='on'  $goto report
parameter refA(seg,*);
refA(seg,'R')=R(seg,'0','optimalfixed');
refA(seg,'H')=H(seg,'0','optimalfixed');
display refA;
execute_unload 'output/noSLRreference/noSLRref%subset%.gdx' Costs, refA;
$exit

