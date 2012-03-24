
CC      = erlc
BEAMDIR = $(shell pwd)/ebin
EFLAGS  = -I $(shell pwd)/src

ifeq ($(MAKECMDGOALS),test)
	EFLAGS += -Ddebug
endif
export CC BEAMDIR EFLAGS

all:
	$(MAKE) -C src/

test: all
	$(MAKE) -C test/

runtests: test
	for i in ebin/*_tests.beam; do \
		j=$${i#ebin/}; j=$${j%_tests.beam}; \
		erl -pa ebin/ -I src/sips/ -eval "eunit:test($$j,[verbose])." -s erlang halt; \
	done

clean:
	rm -f $(BEAMDIR)/*.beam

