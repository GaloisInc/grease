#TODO write a description for this script
#@author 
#@category _NEW_
#@keybinding 
#@menupath 
#@toolbar 
#@runtime Jython


#TODO Add User Code Here




from ghidra.program.flatapi import FlatProgramAPI
from ghidra.program.model.symbol import RefType
import csv


imgBase = currentProgram.getImageBase()
freshBase = askInt("Image base","new base")


def addrToInt(addr):
	return "0x{0:x}".format(addr.subtract(imgBase) + freshBase)

saveTo = askFile("Save callgraph", "save")
listing = currentProgram.getListing()
fm = currentProgram.getFunctionManager()
funcs = fm.getFunctions(True) # True means 'forward'
rows = []
for func in funcs: 
    addrSet = func.getBody()
    insns = listing.getInstructions(addrSet, True) # true means 'forward'
    for insn in insns:
	for ref in insn.getReferencesFrom():
		if ref.getReferenceType().isCall():
			toAddr = ref.getToAddress()
			fromAddr = ref.getFromAddress()
			if toAddr is not None:
				print(addrToInt(toAddr))
				rows.append([addrToInt(func.getEntryPoint()), addrToInt(fromAddr), addrToInt(toAddr)])

with open(saveTo.getPath(),"w") as f:
	wrt = csv.writer(f, delimiter='\t')
	for row in rows:
		wrt.writerow(row)

