
CC      = erlc
BEAMDIR = $(shell pwd)/ebin
EFLAGS  = -I $(shell pwd)/src
EFLAGS += -I $(shell pwd)/deps/cowboy/include
EFLAGS += -I $(shell pwd)/deps/jsonerl/src
EFLAGS += -I /usr/lib/erlang/lib

ifeq ($(MAKECMDGOALS),test)
	EFLAGS += -Ddebug -DTEST +debug_info
endif
export CC BEAMDIR EFLAGS

all: deps
	mkdir -p ebin/modules
	@$(MAKE) -C src/

deps:
	@$(MAKE) -C deps/

test: all
	@$(MAKE) -C test/

run:
	cd ebin/ && erl -sname epbxd -pa ../deps/cowboy/ebin/ -pa applications/ -eval "application:start(epbxd)"

runtestl: test
	for i in ebin/*_tests.beam; do \
		j=$${i#ebin/}; j=$${j%_tests.beam}; \
		erl -pa ebin/ -I src/sips/ -eval "eunit:test($$j,[verbose])." -s erlang halt; \
	done

clean:
	rm -f $(BEAMDIR)/*.beam

distclean: clean
	@$(MAKE) -C deps/ clean

.PHONY: deps

