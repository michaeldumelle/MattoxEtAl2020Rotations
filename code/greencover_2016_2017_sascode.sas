filename roty1 'greencover_2016_data.csv';
data roty1;
	infile roty1 firstobs = 2 dlm = ',';
	input block trt mo $ response month vgrp;
run;

proc print data = roty1;
run;

ods graphics on;
proc mixed data = roty1 maxiter=100;
	class block trt month vgrp;
	model response = trt|month / residual outp = predresidy1 ddfm = SAT;
	repeated month / subject = block*trt group = trt type = arh(1) rcorr;
	*uncomment the next line if you want the pairwise difference table;
	lsmeans month*trt/ diff;
run;
ods graphics off;




filename roty2 'greencover_2016_data.csv';
data roty2;
	infile roty2 firstobs = 2 dlm = ',';
	input block trt mo $ response month vgrp;
run;

proc print data = roty2;
run;

ods graphics on;
proc mixed data = roty2 maxiter = 100;
	class block trt month vgrp;
	model response = trt|month / residual outp = predresidy1 ddfm = SAT;
	repeated month / subject = block*trt group = trt type = arh(1) rcorr;
	*uncomment the next line if you want the pairwise difference table;
	lsmeans month*trt/ diff;
run;
ods graphics off;





ods graphics on;
proc mixed data = roty2;
	class block trt month;
	model response = trt|month / residual outp = predresidy1 ddfm = SAT;
	repeated month / subject = block*trt group = trt type = arh(1) rcorr;
	*uncomment the next line if you want the pairwise difference table;
	lsmeans month*trt/ diff;
run;
ods graphics off;
