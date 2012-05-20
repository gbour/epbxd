
SRC     = $(wildcard *.erl)
BEAMS   = $(patsubst %.erl,$(BEAMDIR)/%.beam,$(SRC))
SUBDIRS = logging

all: subdirs $(BEAMS)

subdirs:
	@for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir; \
	done

$(BEAMDIR)/%.beam: %.erl
	@$(CC) $(EFLAGS) -o $(BEAMDIR)/modules $<

.PHONY: all subdirs
