#!/usr/bin/env python3

import os
import sys
import subprocess as sb

BIN_EXEC = os.path.abspath("../duschinsky")

def run_duschinsky():
   
   sb.run([BIN_EXEC])

if __name__ == "__main__":
   run_duschinsky()