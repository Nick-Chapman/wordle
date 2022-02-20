
top: answers.entropy

extra.sorted: answers.sorted legal.sorted
	cat $^ | sort | uniq -c | grep 1 | cut -c9- > $@

answers.entropy: Makefile src/*.hs
	stack run -- gen entropy | sort -k 2 > $@ # 7 seconds

legal.entropy: Makefile src/*.hs
	stack run -- gen entropy all | sort -k 2 > $@ # 35 seconds
