
top: bot1.trace

extra.sorted: answers.sorted legal.sorted
	cat $^ | sort | uniq -c | grep 1 | cut -c9- > $@

answers.entropy: Makefile src/*.hs
	stack run -- gen entropy | sort -k 2 > $@ # 7 seconds

legal.entropy: Makefile src/*.hs
	stack run -- gen entropy all | sort -k 2 > $@ # 35 seconds

bot1.trace: src/*.hs
	stack run -- tab bot1 > bot1.trace # 3s

bot2.trace: src/*.hs
	stack run -- tab bot2 > bot2.trace # 18s... #26s (when got slower?)

bot3.trace: src/*.hs
	stack run -- tab bot3 > bot3.trace # 5m 44s

bot4.trace: src/*.hs
	stack run -- tab bot4 > bot4.trace # 5m 57s

bot5.trace: src/*.hs
	stack run -- tab bot5 > bot5.trace
