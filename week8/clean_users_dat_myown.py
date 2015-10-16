#!/usr/bin/python -tt

import re, sys

def clean_users():
  """
  Given standard input of the example data:
  userid::gender::age::occupation::zipcode
  1::F::1::10::48067
  reformat to tab separated and correct zipcodes that have extra
  4 digits (-XXXX) to only output beginning 5 digits.
  """ 
  for line in sys.stdin:
    line = line.strip()
    userid,gender,age,occupation,zipcode = line.split('::')
    print "%s\t%s\t%s\t%s\t%s" % (userid,gender,age,occupation,zipcode[0:5])

def main():
  clean_users()

if __name__ == '__main__':
  main()

