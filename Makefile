.PHONY: ps all test ci

all: test

ps:
	@spago build

test: 
	@spago -x test.dhall test

ci: ps test
