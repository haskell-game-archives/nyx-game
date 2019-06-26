.PHONY: setup

setup:
	stack setup

.PHONY: build

build:
	stack build --ghc-options='-Werror'

.PHONY: dev

dev:
	stack build --fast --file-watch

.PHONY: exec

exec:
	stack build && stack exec app

.PHONY: run

run:
	stack build && stack exec app -- +RTS -sstderr

.PHONY: profile

profile:
	stack build --profile && stack exec app -- +RTS -sstderr -p -hc && stack exec -- hp2ps -c app.hp && evince app.ps

.PHONY: clean

clean:
	stack clean

