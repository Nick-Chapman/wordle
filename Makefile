
top: answers.entropy

answers.entropy: Makefile src/*.hs
	stack run -- gen entropy | sort -k 2 > $@ # 7 seconds

legal.entropy: Makefile src/*.hs
	stack run -- gen entropy all | sort -k 2 > $@ # 35 seconds
