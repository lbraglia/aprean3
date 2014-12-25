R = R CMD BATCH --no-timing --quiet --vanilla

%.Rout : %.R
	${R} $<

all: clean do

do: *.Rout

clean:
	cd ../ && rm -rf data/*
	cd ../ && rm -rf R/ds*
