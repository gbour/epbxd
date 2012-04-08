
CC      = erlc
BEAMDIR = $(shell pwd)/ebin
EFLAGS  = -I $(shell pwd)/src
EFLAGS += -I $(shell pwd)/../resources/jsonerl/src
EFLAGS += -I $(shell pwd)/../resources/cowboy/include
EFLAGS += -I /usr/lib/erlang/lib

ifeq ($(MAKECMDGOALS),test)
	EFLAGS += -Ddebug -DTEST +debug_info
endif
export CC BEAMDIR EFLAGS

all:
	mkdir -p ebin/modules
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

