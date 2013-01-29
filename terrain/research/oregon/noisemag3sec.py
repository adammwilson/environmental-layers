#!/usr/bin/env python

############################################################################
# MODULE:   r.noisemag3sec for GRASS
# AUTHOR(S):    Original algorithm and AML implementation by John
#               Gallant. Translation to GRASS/Python by Jim Regetz.
# REFERENCES:
#   Gallant, John. 2001. Adaptive smoothing for noisy DEMs.
#       Geomorphometry 2011.
#       http://geomorphometry.org/system/files/Gallant2011geomorphometry.pdf
#
#############################################################################

#%module
#%  description: Calculates a measure of surface noisiness
#%end
#%option
#%  key: input
#%  type: string
#%  gisprompt: old,cell,raster
#%  description: Raster input map
#%  required : yes
#%end
#%option
#%  key: output
#%  type: string
#%  gisprompt: new,cell,raster
#%  description: Noise standard deviation output raster map
#%  required : yes
#%end
#%option
#%  key: region
#%  type: string
#%  gisprompt: old,cell,raster
#%  description: Map used to set extent; default is to use current region
#%  required : no
#%end

# NOTES
# - make GRASS be quiet with this:
#     gs.os.environ['GRASS_VERBOSE'] = '0'

import atexit
import sys
import math
import grass.script as gs
import tempfile

# create set to store names of temporary maps to be deleted upon exit
tmp_rast = set()

def cleanup():
    gs.message("Removing temporary files...", flag='i')
    for rast in tmp_rast:
        gs.run_command("g.remove", rast=rast, quiet=True)

def coarsen_region(factor=3):
    gs.run_command('g.region',
        rows=gs.region()['rows']/factor,
        cols=gs.region()['cols']/factor)

def refine_region(factor=3):
    gs.run_command('g.region',
        rows=gs.region()['rows']*factor,
        cols=gs.region()['cols']*factor)

def calculate_noise(input, output):

    # create temporary annulus weights file
    weightfile = tempfile.NamedTemporaryFile()
    weightfile.write('0 0 1 0 0\n'
                   + '0 1 1 1 0\n'
                   + '1 1 0 1 1\n'
                   + '0 1 1 1 0\n'
                   + '0 0 1 0 0\n')
    weightfile.flush()

    #todo: do we want flag -a (don't align output with input)?
    gs.run_command('r.neighbors', input=input, output='noisemean',
        method='average', weight=weightfile.name, size=5, quiet=True)
    tmp_rast.add('noisemean')

    gs.mapcalc('noisediffmn = %s - noisemean' % input)
    tmp_rast.add('noisediffmn')

    gs.run_command('r.neighbors', input='noisediffmn',
        output='noiseraw', method='stddev', flags='c', size=5,
        quiet=True)
    tmp_rast.add('noiseraw')

    # [jg] 'calculate mean difference from mean'
    #noisemndf = focalmean(noisediffmn, circle, 2)
    gs.run_command('r.neighbors', input='noisediffmn',
        output='noisemndf', method='average', flags='c', size=5,
        quiet=True)
    tmp_rast.add('noisemndf')
    # [jg] 'soft absolute value of that'
    #noisemndf_abs = sqrt(1 + sqr(noisemndf)) - 1
    gs.mapcalc('noisemndf_abs = sqrt(1 + pow(noisemndf,2)) - 1')
    tmp_rast.add('noisemndf_abs')

    # [jg] 'smooth, this is the terrain signal'
    #noisemndf_mdn = focalmedian(noisemndf_abs, circle, 3)
    gs.run_command('r.neighbors', input='noisemndf_abs',
        output='noisemndf_mdn', method='median', flags='c', size=7,
        quiet=True)
    tmp_rast.add('noisemndf_mdn')

    # [jg] 'reduce noiseraw by the terrain signal'
    #noiseraw_rdc = noiseraw / (1 + 2 * mndiffmn2bfmd)
    # jr: looks like a typo in the 2nd input grid name above...
    gs.mapcalc('noiseraw_rdc = noiseraw / (1 + 2 * noisemndf_mdn)')
    tmp_rast.add('noiseraw_rdc')

    #setcell minof
    #noisemag_2_2 = focalmedian(aggregate(noiseraw_rdc, 2, median), circle, 2)
    coarsen_region(factor=2)
    gs.run_command('r.resamp.stats', method='median',
        input='noiseraw_rdc', output='tmp', quiet=True)
    tmp_rast.add('tmp')
    #todo: check if we're doing this next one at the right resolution...
    gs.run_command('r.neighbors', input='tmp',
        output='noisemag_2_2', method='median', flags='c', size=5,
        quiet=True)
    tmp_rast.add('noisemag_2_2')

    #setcell %DEMgrid%
    #noisemag = resample(noisemag_2_2, #, bilinear)
    refine_region(factor=2)
    gs.run_command('r.resamp.interp', method='bilinear',
        input='noisemag_2_2', output=output, quiet=True)

    cleanup()
    return None


def main():

    # process command options
    input = options['input']
    if not gs.find_file(input)['file']:
        gs.fatal(_("Raster map <%s> not found") % input)

    output = options['output']
    if gs.find_file(output)['file'] and not gs.overwrite():
        gs.fatal(_("Output map <%s> already exists") % output)

    # set aside region for internal use
    gs.use_temp_region()

    # subset input if desired
    region = options.get('region')
    if region:
        if not gs.find_file(region)['file']:
            gs.fatal(_("Raster map <%s> not found") % region)
        gs.message("Setting region to %s" % region, flag='i')
        gs.run_command('g.region', rast=region, align=input)
    else:
        gs.message("Using existing GRASS region", flag='i')
    gs.debug('='*50)
    gs.debug('\n'.join(gs.parse_command('g.region', 'p').keys()))
    gs.debug('='*50)

    calculate_noise(input, output)

    # restore original region
    gs.del_temp_region()

    return None


if __name__ == '__main__':
    options, flags = gs.parser()
    atexit.register(cleanup)
    main()
