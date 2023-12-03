#!/bin/bash

find . -type f | entr sh -c 'make clean; make'
