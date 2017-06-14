PROJECT = peer2peer
PROJECT_DESCRIPTION = Peer-to-peer chat
PROJECT_VERSION = 0.1.0
LOCAL_DEPS = kernel stdlib crypto asn1 public_key ssl

## ###################################################################
## For development only
## ###################################################################

LOCAL_DEPS += runtime_tools observer wx inets sasl tools debugger

## ###################################################################
## 3rd-party deps
## ###################################################################

DEPS = cowboy
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.1.2

DEPS += jsx
dep_jsx = git https://github.com/talentdeficit/jsx v2.8.2

DEPS += gproc
dep_gproc = git https://github.com/esl/gproc.git 0.2.16


include erlang.mk
