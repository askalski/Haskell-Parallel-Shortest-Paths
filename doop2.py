#!/usr/bin/python

import os;

hsrun = "./Main "
csrun = "./a.out "
hssuffix = " +RTS -s -N16"


def main():
	print "compiling HS"
	os.system("ghc -O2 -threaded -rtsopts --make Main.hs")
	for x in range(20,200,20):
		for i in range(1,2):
			name = str(x) + str(i) + ".txt"
			print "firing " + str(x) + " @ " + str(i)
			os.system("./graphgen " + str(x) + " " + str(i) + " > " + name)
			print "calculating in C"
			os.system(csrun + "< " + name + " > c" + name)
			print "done"
			print "calculating in Haskell"
			os.system(hsrun + hssuffix + "< " + name + " > h" + name)			
			print "done"
			print("testing " + name)
			os.system("diff h" + name + " c" + name)
			os.system("rm *"+name)
	
main()
