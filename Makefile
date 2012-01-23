.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -pa '~/imagine/'

MODS = imagine

all: compile
	${ERL} -s imagine start

compile: ${MODS:%=%.beam}

clean: 
	rm -rf *.beam erl_crash.dump