flp19-log: flp19-log.pl
	swipl -o flp19-log -g main -c flp19-log.pl

.PHONY: clean test

clean:
	rm -f flp19-log

test:
	make
	bash test.sh
