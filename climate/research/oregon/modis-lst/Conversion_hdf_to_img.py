#This is a simple code to convert hdf file into img format

listfiles1=glob.glob('*.hdf')

infile=listfiles1[1]
outfile=infile[:-4]+'.img' #this select the name of file without its extension


