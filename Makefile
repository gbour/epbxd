
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

clean:
	rm -f $(BEAMDIR)/*.beam

