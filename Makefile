REBAR = rebar3
NODE_NAME = peer2peer

.PHONY: compile rel run

deploy: build run

build: compile rel

compile:
	$(REBAR) compile

rel:
	$(REBAR) release

run:
	_build/default/rel/$(NODE_NAME)/bin/$(NODE_NAME) console #foreground

stop:
	_build/default/rel/$(NODE_NAME)/bin/$(NODE_NAME) stop

clean:
	rm -rf _build & rm -rf rebar.lock & rm -rf rebar3.crashdump

ct:
	$(REBAR) ct
