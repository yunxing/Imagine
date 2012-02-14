#!/bin/bash

erl -name master@localhost -boot start_sasl -config elog -s web_server start -detached
