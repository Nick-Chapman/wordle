
top: answers.entropy

answers.entropy: Makefile src/*.hs
	stack run | sort -k 2 > $@
