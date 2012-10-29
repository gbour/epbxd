
PATH    := $(PATH):$(shell pwd)/rebar
export PATH

all: rebar cowboy jsonerl poolboy meck rtplib

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


rabbitmq-umbrella:
	if [ ! -d $@ ]; then\
		git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git rabbitmq-umbrella;\
	fi
	cd $@ && git pull --rebase

erlando: rabbitmq-umbrella
	if [ ! -d $</$@ ]; then\
		git clone https://github.com/rabbitmq/erlando $</$@;\
	fi
	cd rabbitmq-umbrella/$@ && git pull --rebase
	@$(MAKE) -C rabbitmq-umbrella/$@
	

rtplib: erlando
	if [ ! -d $@ ]; then\
		git clone https://github.com/lemenkov/rtplib;\
	fi
	cd $@ && git pull --rebase
	if [ ! -d $@/ebin ]; then\
		mkdir $@/ebin;\
	fi
	cp rabbitmq-umbrella/erlando/ebin/* $@/ebin/
	cd $@ && ./autogen.sh && ./configure --without-audiocodec-ilbc
	@$(MAKE) -C $@/

erlrtpproxy:
	if [ ! -d $@ ]; then\
		git clone https://github.com/lemenkov/erlrtpproxy;\
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

.PHONY: rebar cowboy jsonerl poolboy eretrie meck rtplib
