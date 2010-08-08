#!/bin/bash
erl -sname test@localhost -pa `pwd`/ebin/ deps/*/ebin/ -run reloader start