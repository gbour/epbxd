
PATH    := $(PATH):$(shell pwd)/rebar
export PATH

all: rebar cowboy jsonerl poolboy meck

rebar:
	if [ ! -d rebar ]; then\
		git clone https://github.com/basho/rebar.git;\
	fi
	cd rebar && git pull --rebase
	@$(MAKE) -C rebar/

cowboy:
	if [ ! -d cowboy ]; then\
		git clone https://github.com/extend/cowboy.git;\
	fi
	cd cowboy && git pull --rebase
	@$(MAKE) -C cowboy/

jsonerl:
	if [ ! -d $@ ]; then\
		git clone https://github.com/lambder/jsonerl.git;\
	fi
	cd $@ && git pull --rebase
	@$(MAKE) -C $@/

poolboy:
	if [ ! -d $@ ]; then\
		git clone https://github.com/gbour/poolboy;\
	fi
	cd $@ && git pull --rebase
	@$(MAKE) -C $@/

eretrie:
	if [ ! -d $@ ]; then\
		git clone https://github.com/gbour/eretrie;\
	fi
	cd $@ && git pull --rebase
	@$(MAKE) -C $@/

meck:
	if [ ! -d $@ ]; then\
		git clone https://github.com/eproxus/meck;\
	fi
	cd $@ && git pull --rebase
	@$(MAKE) -C $@/

clean:
	find ./* -prune -type d -exec rm -Rf '{}' \;

.PHONY: rebar cowboy jsonerl poolboy eretrie meck
