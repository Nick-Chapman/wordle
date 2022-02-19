
top: legal.entropy

legal.entropy: Makefile src/*.hs
	stack run | sort -k 2 > $@
