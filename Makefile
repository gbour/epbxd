
CC      = erlc
BEAMDIR = $(shell pwd)/ebin
EFLAGS  = -I $(shell pwd)/src
export CC BEAMDIR EFLAGS

all:
	$(MAKE) -C src/

clean:
	rm -f $(BEAMDIR)/*.beam

