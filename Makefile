.SUFFIXES: .erl .beam

.erl.beam:
	  erlc -DPos=local -W $<

ERL = erl -pa '~/imagine/'

MODS = imagine room roomManager gmail imagine_sup imagine_app utility_server web_server

all: compile

compile: ${MODS:%=%.beam}

clean: 
	rm -rf *.beam erl_crash.dump