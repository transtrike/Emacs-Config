#!/bin/bash
/usr/bin/emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "config.org")'
