#!/bin/sh

rsync -avzhP -e ssh rmk7xy@interactive.hpc.virginia.edu:~/ARPvotes/ .
