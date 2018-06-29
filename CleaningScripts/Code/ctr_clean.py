import csv
from collections import defaultdict
import os 
import sys #dealing with multiple versions of Python and some minor differences between them
import zipfile 
if sys.version_info[0] < 3:
	from Tkinter import *
	import tkFileDialog

else:
	from tkinter import *
	from tkinter import filedialog

def clean(path, outpath = None):
	data = defaultdict(list)
	with open(path) as f:
	    reader = csv.DictReader(f) # read rows into a dictionary format
	    for row in reader: # read a row as {column1: value1, column2: value2,...}
	    	for (k,v) in row.items(): # go over each column name and value 
	    		data[k.strip()].append(v.strip()) # append the value into the appropriate list
	                                 # based on column name k
	headers = ["CTRID","dateOfTransaction","cashDirection","cashAmount","typeOfFinancialInstitution","fullNameOfFinancialInstitution","nameOfBranchOfficeAgency"]
	data["CTRID"] = list(map(lambda x: '"' + x + '"', data["CTRID"]))
	data["fullNameOfFinancialInstitution"] = list(map(lambda x: '"' + x + '"', data["fullNameOfFinancialInstitution"]))
	dirs = path.split("/")
	name = dirs[-1]
	name = "UPDATED_" + name
	if outpath is None:
		outpath = "/".join(dirs[:len(dirs)-1]) + "/" + name
	else:
		outpath += "/" + name
	with open(outpath, 'w') as fout:
		fout.write(",".join(headers) + "\n")
		for i in range(len(data['CTRID'])):
			line = ''
			for val in headers:
				line += data[val][i]
				line += ","
			line += "\n"
			fout.write(line)
	fout.close()

def run():
	Tk().withdraw() #dont need a console open
	if sys.version_info[0] < 3:
		zippath = tkFileDialog.askopenfile() #open up the file system and get the name
	else:
		zippath = filedialog.askopenfile()
	clean(zippath.name)

run()