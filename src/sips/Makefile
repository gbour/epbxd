
SRC     = $(wildcard *.erl)
BEAMS   = $(patsubst %.erl,$(BEAMDIR)/%.beam,$(SRC))
SUBDIRS = mods

all: subdirs $(BEAMS)

subdirs:
	@for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir; \
	done

$(BEAMDIR)/%.beam: %.erl
	@$(CC) $(EFLAGS) -o $(BEAMDIR) $<

.PHONY: all subdirs
