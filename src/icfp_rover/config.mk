## Template configuration

## Erlang Configuration
ERLC ?= erlc
EFLAGS ?= -Wall
ERL_CONFIG ?= erl-config

# Driver Configuration
PLATFORM ?= gnu
CC ?= cc
LD ?= cc
DRV = example.so
DRV_INSTALLDIR ?= ../priv/lib
INCLUDES ?= -I/usr/local/include
LIBS ?= -L/usr/local/lib

## Revision Macro TAG
REV = 1.0

## Standard system Configuration
SED ?= sed
RM ?= rm
