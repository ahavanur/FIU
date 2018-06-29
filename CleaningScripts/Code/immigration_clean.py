import xlrd
import datetime 
import os
import os 
import sys #dealing with multiple versions of Python and some minor differences between them
import zipfile 
if sys.version_info[0] < 3:
	from Tkinter import *
	import tkFileDialog

else:
	from tkinter import *
	from tkinter import filedialog

authority_map = dict()
authority_map['PW'] = 'Palau'
authority_map['HI'] = 'Hawaii'
authority_map['MP'] = 'Saipan'
authority_map['DE'] = 'Delaware'
authority_map['GU'] = 'Guam'
authority_map['CN'] = 'China'
authority_map['US'] = 'United States'
authority_map['SG'] = 'Singapore'
authority_map['KR'] = 'Korea'
authority_map['PH'] = 'Philippines'
authority_map['BD'] = 'Bangladesh'
authority_map['HK'] = 'Hong Kong'
authority_map['USA'] = 'United States'
authority_map['TWN'] = 'Taiwan'

def clean(values):
	values = list(map(lambda x: str(x).strip(), values))
	values = list(map(lambda x: authority_map[x.upper().strip()] if x.upper().strip() in authority_map else x, values))
	values = list(map(lambda x: '"' + x.upper() + '"', values))
	return values


def fixdate(date,wb):
	if date != '':
		temp = xlrd.xldate.xldate_as_datetime(date, wb.datemode)
		newdate = datetime.datetime.strptime(str(temp.date()), "%Y-%m-%d").strftime("%m/%d/%Y")
		return newdate 
	return ''

def main(path,caseID):
	wb = xlrd.open_workbook(path)
	sh = wb.sheet_by_index(0)
	basics = extractBasicInfo(sh,wb)
	permits = extractPermitInfo(sh,wb)
	movements = extractMovements(sh,wb)
	headers = ["CASEID", "First and Middle Name","Last Name","Citizenship","Document Type",
	"Document #","Nationality","Date of Birth","Gender","Employer/Sponsor", "Permit Type", 
	"Permit #", "Permit Employer", "Permit Status", "Permit Issue Date", "Permit Expiry Date", "Permit Issue By", "Carrier",
	"Voyage","Permit # For Travel","Employer During Travel","Travel Date","Travel Status","Travel Purpose","Address",
	"Expiry Date on Travel","User","# of Extend"]
	dirs = path.split("/")
	name = dirs[-1]
	name = "UPDATED_" + name
	name = name.replace(".xls", ".csv")
	outpath = "/".join(dirs[:len(dirs)-1]) + "/" + name
	with open(outpath, 'w') as fout:
		fout.write(",".join(headers) + "\n")
		for move in movements:
			row = basics + permits + move
			row = caseID+","+",".join(row)+"\n"
			fout.write(row)
	fout.close()

def extractBasicInfo(sh,wb):
	values = []
	for i in range(9,18):
		values.append(sh.cell_value(rowx=i, colx=7))
	values[6] = fixdate(values[6], wb)
	return clean(values)

def extractPermitInfo(sh,wb):
	permitinfo = map(lambda x: x.value, sh.row(25))
	non_empty = list(filter(lambda x: x != '', permitinfo))
	if non_empty == []:
		values = ['' for i in range(7)]
	else:
		values = non_empty
		values[4] = fixdate(values[4], wb)
		values[5] = fixdate(values[5], wb)
	return clean(values)

def extractMovements(sh,wb):
	movements = []
	for rx in range(0,sh.nrows):
		a = sh.row(rx)
		if str(a[1].value).strip() == 'Movements':
			m_idx = rx
	extract_idx = [1, 2, 4, 8, 10, 11, 16, 18, 20, 21, 25]
	for i in range(m_idx+3, sh.nrows):
		row = map(lambda x: x.value, sh.row(i))
		extracted = []
		for idx in extract_idx:
			extracted.append(row[idx])
		extracted[4] = fixdate(extracted[4], wb)
		extracted[8] = fixdate(extracted[8], wb)
		movements.append(clean(extracted))
	return movements

def run(caseID):
	Tk().withdraw() #dont need a console open
	if sys.version_info[0] < 3:
		zippath = tkFileDialog.askopenfile() #open up the file system and get the name
	else:
		zippath = filedialog.askopenfile()
	try:
		main(zippath.name, caseID)
	except:
		pass

caseID = None
def show_entry_fields():
	global caseID
	caseID = e1.get()
	master.destroy()

master = Tk()
master.title("Immigration Data Cleaning Tool")

Label(master, text="CASEID").grid(row=0)

e1 = Entry(master)

e1.grid(row=0, column=1)

Button(master, text='Submit Case ID', command=show_entry_fields).grid(row=3, column=1, sticky=W, pady=4)
mainloop( )
run(caseID)

#can you have multiple permits?
#USA-FSM-RMI/ values 
#Any of this uneeded