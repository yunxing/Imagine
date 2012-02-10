.SUFFIXES: .erl .beam

.erl.beam:
	  erlc -DPos=local -W $<

ERL = erl -pa '~/imagine/'

MODS = imagine room roomManager gmail

all: compile
	${ERL} -s imagine start

compile: ${MODS:%=%.beam}

clean: 
	rm -rf *.beam erl_crash.dump