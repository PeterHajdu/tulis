tulis: *.idr
	idris -p contrib -p iterm Main.idr -o tulis

clean:
	rm -f tulis *.ibc *.core

.phony: clean
