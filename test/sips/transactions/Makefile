
SRC     = $(wildcard *.erl)
BEAMS   = $(patsubst %.erl,$(BEAMDIR)/%.beam,$(SRC))
SUBDIRS = 


all: subdirs $(BEAMS)

subdirs:
	for dir in $(SUBDIRS); do \
		$(MAKE) -e -C $$dir; \
	done

$(BEAMDIR)/%.beam: %.erl
	$(CC) $(EFLAGS) -o $(BEAMDIR) $<

