import os, fnmatch, zipfile, datetime

outpath = '//I/NCEAS/topo/incoming/srtmv41'
inpath = '//I/NCEAS/SRTM_90m_ASCII_v4.1'
logname = outpath + '/unzip.log'
logfile = open(logname, 'a')
logfile.write('Start unziptiles.py at ' + str(datetime.datetime.today()) + '\n')

nprocessed = 0
nerrors = 0

try:
    
    for filename in os.listdir(inpath):
        fullname = inpath + '/' + filename
        if fnmatch.fnmatch(fullname, '*.zip'):
            print fullname
            try:
                zip = zipfile.ZipFile(fullname, 'r')
                for zipi in zip.infolist():
                    name = zipi.filename
                    if name != 'readme.txt':
                        outfilename = outpath + '/' + name
                        print outfilename
                        if os.path.exists(outfilename):
                            print 'done'
                        else:
                            print 'Process', outfilename
                            outfile = open(outfilename, 'w')
                            outfile.write(zip.read(name))
                            outfile.close()
                            nprocessed = nprocessed + 1
                zip.close()
            except Exception, e:
                print str(e)
                logfile.write(str(e) + '\n')
                nerrors = nerrors + 1
except Exception, e:
    print "Exception:", e.str()
    logfile.write(str(e) + '\n')

logfile.write('Processed ' + str(nprocessed) + '\n')
logfile.write('Errors: ' + str(nerrors) + '\n')
logfile.write('End unziptiles.py at ' + str(datetime.datetime.today()) + '\n')
logfile.close()
