import csv
from collections import defaultdict
import os 
import sys #dealing with multiple versions of Python and some minor differences between them
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
	headers = ["CTRID","relationshipToTransaction",\
	"lastNameOrNameOfEntity","firstName","middleName",\
	"gender","occupationOrTypeOfBusiness","address",\
	"addressCity","addressState","zipCode","addressCountry", \
	"dateOfBirth","contactPhoneNumber","emailAddress","idType",\
	"idNumber","idCountry","idIssuingAuthority","accountNumbers",\
	"cashDirection","cashAmount"]
	data['accountNumbers'] = list(map(lambda x: " " if x == '' else " / ".join(str(x).split(",")), data['accountNumbers']))
	data['CTRID'] = list(map(lambda x: '"' + str(x) + "0"*(9-len(str(x))) + '"', data['CTRID']))
	data['lastNameOrNameOfEntity'] = list(map(lambda x : 'GIBBONS' if x.strip().upper() == 'GIBSONS' else x.strip().upper(), data['lastNameOrNameOfEntity']))
	for i in range(len(data['lastNameOrNameOfEntity'])):
		if data['lastNameOrNameOfEntity'][i].upper() == "WHIPPS":
			if data['firstName'][i].upper() == "SURANGEL":
				if data['dateOfBirth'][i] == '2/21/1939':
					data['lastNameOrNameOfEntity'][i] = "WHIPPS SR"
		if data['lastNameOrNameOfEntity'][i].upper() in ['BILLY TAKAMINE', 'HARRY BESEBES']:
			names = data['lastNameOrNameOfEntity'][i].upper().split()
			data['lastNameOrNameOfEntity'][i] = names[1].upper()
			data['firstName'][i] = names[0].upper()
	data['contactPhoneNumber'] = list(map((lambda x: str(x).strip()), data['contactPhoneNumber']))
	data['contactPhoneNumber'] = list(map(lambda x: "" if len(str(x)) == 0 else ('"' + str(x)[:3] + "-" + str(x)[3:6] + "-" + str(x)[6:] + '"') if len(str(x)) == 10 else ('"' + "680-" + str(x)[:3] + "-" + str(x)[3:6] + '"' if len(str(x)) == 7 else '"' + str(x) + '"'), data['contactPhoneNumber']))
	data['cashAmount'] = list(map(lambda x: str(x.replace(",","")), data['cashAmount']))
	data['cashAmount'] = list(map(lambda x: str(x.replace("$","")), data['cashAmount']))
	authority_map = dict()
	authority_map['PW'] = 'Palau'
	authority_map['HI'] = 'Hawaii'
	authority_map['DE'] = 'Delaware'
	authority_map['GU'] = 'Guam'
	data['idIssuingAuthority'] = list(map(lambda x: "" if len(str(x).strip()) == 0 else (str(x).strip().upper() if str(x).strip().upper() not in authority_map else authority_map[str(x).strip().upper()]), data['idIssuingAuthority']))
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

