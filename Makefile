### INCLUDES
include app.mk

###-----------------------------------------------------------------------------
### APPLICATION LAYOUT
###-----------------------------------------------------------------------------
APPSRC = $(patsubst src/%.app.src,%.app.src,$(wildcard src/*.app.src))
APP = $(APPSRC:.app.src=.app)
APPNAME = $(basename $(APP))
ERLS = $(patsubst src/%.erl,%.erl,$(wildcard src/*.erl))

.PHONY: all clean doc test
.SUFFIXES: .erl .hrl .app.src .app

###-----------------------------------------------------------------------------
### TARGETS
###-----------------------------------------------------------------------------
all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

fmt:
	$(REBAR) fmt

cover:
	@$(REBAR) cover

dialyze:
	@$(REBAR) dialyzer

test: fmt
	@$(REBAR) ct --spec test/conf/test.spec --cover --readable true

test-verbose: fmt
	@$(REBAR) ct --spec test/conf/test.spec --cover --readable true --verbose