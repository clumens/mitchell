# $Id: Includes.mk,v 1.2 2005/08/19 01:14:31 chris Exp $

# Set these to your liking on the command line, or accept the defaults.
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
LOCALEDIR ?= $(PREFIX)/locale

# Flags for optimization - not set if debugging
OPTFLAGS = -O2
