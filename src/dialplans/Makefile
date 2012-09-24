
SRC     = $(wildcard *.erl)
BEAMS   = $(patsubst %.erl,$(BEAMDIR)/%.beam,$(SRC))
SUBDIRS =

all: subdirs $(BEAMS)

subdirs:
	@for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir; \
	done

$(BEAMDIR)/%.beam: %.erl
	#@-mkdir $(BEAMDIR)/dialplans
	@$(CC) $(EFLAGS) -o $(BEAMDIR)/dialplans $<

.PHONY: all subdirs
