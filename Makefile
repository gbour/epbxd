
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
	mkdir -p ebin/modules ebin/applications
	@$(MAKE) -C src/

deps:
	@$(MAKE) -C deps/

test: all
	@$(MAKE) -C test/

ebin/epbxd.app: src/epbxd.app
	@sed -e 's/.*{config,.*/\t\t{config, "epbxd.cfg"}/' $< > $@

ebin/epbxd.cfg: etc/epbxd.cfg
	@sed -e 's/{logfile.*/{logfile, "epbxd.log"}./' \
		   -e 's/{endpoints.*/{endpoints, [\n\t[{name, "100"}],\n\t[{name, "101"}],\n\t[{name, "102"}]/' \
			 $< > $@

run: ebin/epbxd.app ebin/epbxd.cfg
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

