

/*pulling in data*/
/*this is the already aggregated/cleaned data from R*/
proc import datafile= 'C:\Users\Evan\Desktop\Evan MSA\Fall 2\TIme Series II\adj_well_data.csv'
	out = g3549final
	dbms = csv;
run;

/*creating fourier variables*/
data wells;
	set g3549final;
	pi=constant("pi");
	s1=sin(2*pi*1*_n_/24);
	c1=cos(2*pi*1*_n_/24);
	s2=sin(2*pi*2*_n_/24);
	c2=cos(2*pi*2*_n_/24);
	s3=sin(2*pi*3*_n_/24);
	c3=cos(2*pi*3*_n_/24);
	s4=sin(2*pi*4*_n_/24);
	c4=cos(2*pi*4*_n_/24);
	s5=sin(2*pi*5*_n_/24);
	c5=cos(2*pi*5*_n_/24);
	s11=sin(2*pi*1*_n_/8766);
	c11=cos(2*pi*1*_n_/8766);
	s21=sin(2*pi*2*_n_/8766);
	c21=cos(2*pi*2*_n_/8766);
	s31=sin(2*pi*3*_n_/8766);
	c31=cos(2*pi*3*_n_/8766);
	s41=sin(2*pi*4*_n_/8766);
	c41=cos(2*pi*4*_n_/8766);
	s51=sin(2*pi*5*_n_/8766);
	c51=cos(2*pi*5*_n_/8766);
	s111=sin(2*pi*1*_n_/4383);
	c111=cos(2*pi*1*_n_/4383);
	s211=sin(2*pi*2*_n_/4383);
	c211=cos(2*pi*2*_n_/4383);
	s311=sin(2*pi*3*_n_/4383);
	c311=cos(2*pi*3*_n_/4383);
	s411=sin(2*pi*4*_n_/4383);
	c411=cos(2*pi*4*_n_/8766);
	s511=sin(2*pi*5*_n_/8766);
	c511=cos(2*pi*5*_n_/8766);
run;

/*all the annual/daily fourier functions*/
%let sines = s1 s2 s3 s4 s5 c1 c2 c3 c4 c5 s11 s21 s31 s41 s51 c11 c21 c31 c41 c51;
/*all the semiannual fourier functions*/
%let semiannual = s111 c111 s211 c211 s311 c311 s411 c411 s511 c511;


proc sort data=wells;
	by datetime;
run;

/*taking just values since 2014*/
proc sql;
	create table wellsshort as
	select *
	from wells
	where year(datepart(datetime)) ge 2014;
quit;

/*using fourier functions to model seasonality*/
/*scan is helping to identify the appropriate p/q terms*/
proc arima data=wellsshort;
    identify var=height  crosscorr=(&sines) scan;
    *estimate p=1 q=1 method=ml input = (&sines);
    *forecast out = wells_seasonality back 168;
run;
quit;


/*taking seasonal differences to model seasonality*/
proc arima data=wellsshort;
	identify var=height(4383) nlag=168 scan;
	estimate p=5 q=0 method=ml;
	forecast back=168;
run;
quit;


/*no seasonality, just ARMA terms*/
proc arima data=wellsshort plot(unpack);
	identify var=height(1) minic
	p=(0:25) q=(0:25);
	*estimate method=ml;
	*forecast back=168;
run;
quit;

proc arima data=wellsshort;
	identify var=height(1,1) nlag=1000;
	estimate p=5 q=0;
	forecast back=168;
run;
quit;

proc arima data=wellsshort plot=all;
    *identify var=height(6) nlag=40 stationarity=(adf=5);
    *identify var=height(6) nlag=30 minic scan esacf P=(0:60) Q=(0:60);
    identify var=height(4383) nlag=25;
    estimate p=7 q=13 method=ML;
    forecast lead=168 back=168 out=wellmodel;
run;
quit;

proc arima data=wellsshort plot=all;
  identify var=logHeight(1) nlag=30;
  estimate p=44 q=0 method=ML;
  forecast lead=168 back=168 out=wellmodel;
run;
quit;

data wellmodel;
	set wellmodel;
	pe=residual/height;
run;

proc sql;
	select mean(pe)
	from wellmodel;
quit;

/*can run arima on residuals*/
proc arima data=wells_seasonality;
	identify var=Residual;
run;
quit;

