R = R CMD BATCH --no-timing --quiet --vanilla

%.Rout : %.R
	${R} $<

all: *.Rout
