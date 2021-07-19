VSN = 1.1.0

### Special characters
comma := ,
empty :=
space := $(empty) $(empty)

### Erlang compiler
ERL = erl
ERLC = erlc

### Flags
DFLAGS = -I .. --src --verbose -c
STUB_EFLAGS = -v -pz ../../vo/ebin/ -I ../.. -I .. -I ./stubs/ -W1  -o stubs

### Default apps
CD = cd
CP = cp -vf
ECHO = echo
ERLDOC = ndoc
MKDIR = mkdir
MV = mv -vf
RM = rm -vf
SED = sed
REBAR = rebar3
