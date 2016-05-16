check:
	rm -f log.txt
	prove -f --exec './t/do-test -log log.txt'

clean:
	rm -f log.txt

.PHONY: check clean
