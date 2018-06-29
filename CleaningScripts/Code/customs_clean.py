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

def clean(values):
	values = list(map(lambda x: str(x).strip(), values))
	values = list(map(lambda x: authority_map[x.upper().strip()] if x.upper().strip() in authority_map else x, values))
	values = list(map(lambda x: '"' + x.upper() + '"', values))
	return values

#print wb
def part1dataextract(sh,wb): #consistent across all forms theoretically
	name = sh.cell_value(rowx=10,colx=1).split(",")
	firstname = str(" ".join(name[1:])).strip()
	lastname = str(name[0]).strip()
	dob = xlrd.xldate.xldate_as_datetime(sh.cell_value(rowx=10, colx=9), wb.datemode)
	dob = datetime.datetime.strptime(str(dob.date()), "%Y-%m-%d").strftime("%m/%d/%Y")
	perm_address = str(sh.cell_value(rowx=12,colx=1))
	citizenship = str(sh.cell_value(rowx=12,colx=9))
	gender = str(sh.cell_value(rowx=14, colx=1))
	palau_address = str(sh.cell_value(rowx=14,colx=9))
	pass_info = sh.cell_value(rowx=16,colx=1).split()
	passport_no = str(pass_info[0]).strip()
	passport_country = str(pass_info[1]).strip()
	palau_visa = str(sh.cell_value(rowx=16,colx=9)).strip()
	work_permit = str(sh.cell_value(rowx=18, colx=1))
	values =  [firstname, lastname, dob, perm_address, citizenship, gender, palau_address, passport_no, passport_country, palau_visa,work_permit]
	return clean(values) 

def part2dataextract(sh,wb): #again... theoretically consistent
	shipment_method = sh.cell_value(rowx=21,colx=1)
	entry_type = sh.cell_value(rowx=21,colx=9)
	departed_from = sh.cell_value(rowx=23, colx=1).replace("{}", '')
	departed_to = sh.cell_value(rowx=23,colx=9).replace("{}", '') #questionable name
	values = [shipment_method, entry_type, departed_from, departed_to]
	return clean(values)

def part4dataextract(sh,wb):
	p4 = None
	p5 = None 
	for rx in range(0,sh.nrows):
		a = sh.row(rx)
		for i in xrange(sh.ncols):
			try:
				if str(a[i].value).encode('utf-8').strip() == 'PART 4. CURRENCY AND MONETARY INSTRUMENT INFORMATION':
					p4 = rx
				if str(a[i].value).encode('utf-8').strip() == 'PART 5. GENERAL - TO BE COMPLETE BY ALL TRAVELERS, SHIPPERS AND RECIPIENTS':
					p5 = rx
			except UnicodeEncodeError:
				print a[i].value.encode('utf-8').strip()
		if p4 and p5:
			break
	p4results = []
	for i in range(p4+2,p5):
		currency_type = sh.cell_value(rowx=i,colx=1)
		currency_country = sh.cell_value(rowx=i,colx=5)
		currency_amount = str(sh.cell_value(rowx=i,colx=13))
		cents = currency_amount.split(".")
		if len(cents) == 1 or len(cents[1]) < 2:
			if "." not in currency_amount:
				currency_amount += "."
			while len(currency_amount.split(".")[1]) < 2:
				currency_amount += "0"
		values = [currency_type,currency_country,currency_amount]
		p4results.append(clean(values))
	return p4results, p5

def part5dataextact(sh,p5,wb): 
	#all data is relative to where p5 begins
	on_behalf = sh.cell_value(rowx=p5+1, colx=1)
	on_behalf_answer = str(on_behalf.split("?")[1])
	on_behalf_name = sh.cell_value(rowx=p5+3, colx=9).split(" ")
	on_behalf_name_last = on_behalf_name[-1]
	on_behalf_name_first = " ".join(on_behalf_name[0:len(on_behalf_name)-1])
	on_behalf_address = sh.cell_value(rowx=p5+4, colx=9)
	on_behalf_profession = sh.cell_value(rowx=p5+5, colx=9)
	date_of_report = xlrd.xldate.xldate_as_datetime(sh.cell_value(rowx=p5+8, colx=9),  wb.datemode)
	date_of_report = datetime.datetime.strptime(str(date_of_report.date()), "%Y-%m-%d").strftime("%m/%d/%Y")
	values = [on_behalf_answer, on_behalf_name_last, on_behalf_name_first, on_behalf_address, on_behalf_profession,date_of_report]
	return clean(values)

def customsdataextract(sh,wb):
	#ask boboy about which parts are relevant
	cidx = None 
	for rx in range(0,sh.nrows):
		a = sh.row(rx)
		for i in xrange(sh.ncols):
			try:
				if str(a[i].value).encode('utf-8').strip() == 'CUSTOMS USE ONLY':
					cidx = rx
			except UnicodeEncodeError:
				print a[i].value.encode('utf-8').strip()
		if cidx:
			break
	customs_date = xlrd.xldate.xldate_as_datetime(sh.cell_value(rowx=cidx+2, colx=1), wb.datemode)
	customs_date = datetime.datetime.strptime(str(customs_date.date()), "%Y-%m-%d").strftime("%m/%d/%Y")
	flight = str(sh.cell_value(rowx=cidx+2, colx=4)).upper()
	count_verified = sh.cell_value(rowx=cidx+2, colx=7)
	declared = sh.cell_value(rowx=cidx+2, colx=11)
	penalties = sh.cell_value(rowx=cidx+2,colx=14)
	values = [customs_date, flight, count_verified, declared, penalties]
	return clean(values)


def main(path,caseID):
	wb = xlrd.open_workbook(path)
	#print wb
	sh = wb.sheet_by_index(0)
	p1result = part1dataextract(sh,wb)
	p2result = part2dataextract(sh,wb)
	p4results, p5idx = part4dataextract(sh,wb)
	p5result = part5dataextact(sh,p5idx,wb)
	customsresult = customsdataextract(sh,wb)
	headers = ['CASEID', 'First Name', 'Last Name', 'Date Of Birth', 'Permanent Address', 
	'Citizenship', 'Gender', 'Address In Palau', 'Passport Number', 'Passport Country', 
	'Palau Visa Number', 'Work Permit Number', 'Method Of Shipment', 'Shipment Entry Type', 
	'Shipment Point Of Departure', 'Shipment Point Of Arrival', 'Currency Type', 'Currency Country', 
	'Currency Amount', 'On Behalf Of Another', 'Beneficiary Last Name', 'Beneficiary First And Middle Name', 
	'Beneficiary Address', 'Beneficiary Profession', 'Date Of Report', 
	'Date Of Customs Report', 'Flight', 'Count Verified', 'Currency Declared', 'Penalty Applied']
	dirs = path.split("/")
	name = dirs[-1]
	name = "UPDATED_" + name
	name = name.replace(".xls", ".csv")
	outpath = "/".join(dirs[:len(dirs)-1]) + "/" + name
	with open(outpath, 'w') as fout:
		fout.write(",".join(headers) + "\n")
		for p4r in p4results:
			row = p1result + p2result + p4r + p5result + customsresult
			row = caseID+","+",".join(row)+"\n"
			fout.write(row)
	fout.close()

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
master.title("Customs Data Cleaning Tool")

Label(master, text="CASEID").grid(row=0)

e1 = Entry(master)

e1.grid(row=0, column=1)

Button(master, text='Submit Case ID', command=show_entry_fields).grid(row=3, column=1, sticky=W, pady=4)
mainloop( )
run(caseID)