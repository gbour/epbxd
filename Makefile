
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

all: deps src

deps:
	@$(MAKE) -C deps/

src:
	mkdir -p ebin/modules ebin/applications
	@$(MAKE) -C src/

test: all tsrc

tsrc:
	@$(MAKE) -C test/

ebin/epbxd.app: src/epbxd.app
	@sed -e 's/.*{config,.*/\t\t{config, "epbxd.cfg"}/' $< > $@

ebin/epbxd.cfg: etc/epbxd.cfg
	@sed -e 's/^\(.*mod_log_erlang.*filename,\)[^}]\+\(}.*\)$$/\1"epbxd.log"\2/' \
		 -e 's/^\(.*mod_authent_file.*filename,\)[^}]\+\(}.*\)$$/\1"users.auth"\2/' \
	     -e 's/{endpoints.*/{endpoints, [\n\t[{name, "100"}],\n\t[{name, "101"}],\n\t[{name, "102"}]/' \
	     $< > $@

run: ebin/epbxd.app ebin/epbxd.cfg
	cd ebin/ && erl -sname epbxd -pa ../deps/cowboy/ebin/ -pa applications/ -eval "application:start(epbxd)"

runtest:
	for i in `find ebin -iname "*_tests.beam"`; do \
		j=$$(basename $$i); j=$${j%_tests.beam}; \
		erl -pa ebin/ ebin/modules deps/meck/ebin -I src/sips/ -eval "eunit:test($$j,[verbose])." -s erlang halt; \
	done

clean:
	rm -f `find $(BEAMDIR) -name "*.beam"`

distclean: clean
	@$(MAKE) -C deps/ clean

.PHONY: deps src tsrc

