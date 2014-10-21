#!/usr/bin/python

import os;

hsrun = "runhaskell Main.hs "
csrun = "./a.out "

def main():
	for x in range(5,25):
		for i in range(1,5):
			name = str(x) + str(i) + ".txt"
			os.system("./graphgen " + str(x) + " " + str(i) + " > " + name)
			os.system(hsrun + "< " + name + " > h" + name)
			os.system(csrun + "< " + name + " > c" + name)
			print("testing " + name)
			os.system("diff h" + name + " c" + name)
			os.system("rm *"+name)
	
main()
