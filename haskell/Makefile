.PHONY: all clean

TARGETS = 14003

all: $(TARGETS)

%: %.hs
	ghc -o $@ $<

clean: 
	rm -f *.o *.hi $(TARGETS)