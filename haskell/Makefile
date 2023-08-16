.PHONY: all clean

TARGETS = 2618

all: $(TARGETS)

%: %.hs
	ghc -o $@ $<

clean: 
	rm -f *.o *.hi $(TARGETS)