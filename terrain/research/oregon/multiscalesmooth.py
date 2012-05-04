#!/usr/bin/env python

############################################################################
# MODULE:   r.multiscalesmooth for GRASS
# AUTHOR(S):    Original algorithm and AML implementation by John
#               Gallant. Translation to GRASS/Python by Jim Regetz.
# REFERENCES:
#   Gallant, John. 2001. Adaptive smoothing for noisy DEMs.
#       Geomorphometry 2011.
#       http://geomorphometry.org/system/files/Gallant2011geomorphometry.pdf
#
#############################################################################

#%module
#%  description: Multiscale raster smoother
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
#%  description: Smoothed output raster map
#%  required : yes
#%end
#%option
#%  key: sd
#%  type: string
#%  description: Noise standard deviation; can be a raster or a constant
#%  required : yes
#%end
#%option
#%  key: alpha
#%  type: double
#%  answer: 0.05
#%  description: Alpha used for chi-square test [0-1]
#%  required : no
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

# create set to store names of temporary maps to be deleted upon exit
tmp_rast = set()

def cleanup():
    gs.message("Removing temporary files...", flag='i')
    for rast in tmp_rast:
        gs.run_command("g.remove", rast=rast, quiet=True)

def coarsen_region(factor=3):
    gs.run_command('g.region',
        rows=gs.region()['rows']/3,
        cols=gs.region()['cols']/3)

def refine_region(factor=3):
    gs.run_command('g.region',
        rows=gs.region()['rows']*3,
        cols=gs.region()['cols']*3)

def multiscalesmooth(input, smooth, sd, alpha=0.05):

    chisqa = (2.807 - 0.6422 * math.log10(alpha) - 3.410 *
        math.pow(alpha, 0.3411))
    chisqb = (-5.871 - 3.675 * math.log10(alpha) + 4.690 *
        math.pow(alpha, 0.3377))

    gs.message("Preparing initial grids", flag='i')

    # create copy of initial grid of values using current region
    gs.mapcalc('z0 = ${input}', input=input, quiet=True)
    tmp_rast.add('z0')

    # set number of aggregation levels and neighborhood size
    NUM_LEVELS = 4
    NUM_CELLS = 3

    # expand region to accommodate integer number of cells at coarsest
    # level, by adding roungly equal number of cells on either side
    # (with one extra on top/right if an odd number of cells is needed)
    max_size = NUM_CELLS**NUM_LEVELS
    region = gs.region()
    extra = region['cols'] % max_size
    if (0 < extra):
        addx = (max_size-extra)/2.0
    else:
        addx = 0.0
    extra = region['rows'] % max_size
    if (0 < extra):
        addy = (max_size-extra)/2.0
    else:
        addy = 0.0
    gs.run_command('g.region', flags='a',
        w = region['w'] - math.floor(addx) * region['ewres'],
        e = region['e'] + math.ceil(addx) * region['ewres'],
        s = region['s'] - math.floor(addy) * region['nsres'],
        n = region['n'] + math.ceil(addy) * region['nsres'])
    gs.debug('\n'.join(gs.parse_command('g.region', 'up').keys()))

    # create initial grid of variances; sd can be a raster or a constant
    gs.mapcalc('v0 = if(isnull(z0), null(), ${sd}^2)', sd=sd, quiet=True)
    tmp_rast.add('v0')
    # set initial "group variance" to individual msmt variance (noise)
    gs.run_command('g.copy', rast='v0,v.g')
    tmp_rast.add('v.g')
    # weights for aggregation, based on total variance
    gs.mapcalc('w = if(isnull(v0), 0, 1/v0)', quiet=True)
    tmp_rast.add('w')
    # squared weights
    gs.mapcalc('wsq = w^2', quiet=True)
    tmp_rast.add('wsq')
    # effective number of measurements
    gs.mapcalc('n = if(isnull(z0), 0, 1)', quiet=True)
    tmp_rast.add('n')

    # aggregate to broader scales
    for j in range(NUM_LEVELS):
        i = j + 1
        gs.message('Aggregating from %d to %d' % (j, i), flag='i')
        gs.debug(_('Region dimensions: %d x %d' % (gs.region()['rows'],
            gs.region()['cols'])))

        # rename previous (finer scale) weights grid
        gs.run_command('g.rename', rast='w,w.finer', overwrite=True,
            quiet=True)
        tmp_rast.add('w.finer')

        # calc neighborhood weights, num cells, effective num cells
        coarsen_region()
        gs.run_command('r.resamp.stats', method='sum', input='w.finer',
            output='w', quiet=True)
        gs.run_command('r.resamp.stats', method='sum', input='wsq',
            output='wsq', overwrite=True, quiet=True)
        gs.run_command('r.resamp.stats', method='sum', input='n',
            output='n', overwrite=True, quiet=True)

        # calc variance-weighted neighborhood mean
        refine_region()
        gs.mapcalc('tmp = w.finer * z%d / w' % j, quiet=True)
        tmp_rast.add('tmp')
        coarsen_region()
        gs.run_command('r.resamp.stats', method='sum', input='tmp',
            output='z%d' % i, quiet=True)
        tmp_rast.add('z%d' % i)

        # calc between-cell variance, taken over neighborhood
        refine_region()
        gs.mapcalc('tmp = w.finer * (z%d - z%d)^2 / w' % (j, i),
            quiet=True)
        tmp_rast.add('tmp')
        coarsen_region()
        gs.run_command('r.resamp.stats', method='sum', input='tmp',
            output='v.bg', overwrite=True, quiet=True)
        tmp_rast.add('v.bg')

        # calc wtd avg of within-cell variance, taken over neighborhood
        if (i==1):
            gs.mapcalc('v.wg = 0', quiet=True)
            tmp_rast.add('v.wg')
        else:
            refine_region()
            gs.mapcalc('tmp = w.finer * v.g / w', quiet=True)
            tmp_rast.add('tmp')
            coarsen_region()
            gs.run_command('r.resamp.stats', method='sum', input='tmp',
                output='v.wg', overwrite=True, quiet=True)

        # calc total group variance
        # ~= total variance of cell vals in the underlying neighborhood
        gs.mapcalc('v.g = v.bg + v.wg', quiet=True)
        tmp_rast.add('v.g')

        # calc chisq critical values (where df = n.eff - 1)
        gs.mapcalc('chisq = 1 + ${chisqa}/sqrt(${df}) + ${chisqb}/${df}',
            chisqa=chisqa, chisqb=chisqb, df='(w^2/wsq - 1)', quiet=True)
        tmp_rast.add('chisq')
        # set coarsened cell variances: if group variance is small
        # relative to noise variance (mean of finer scale variances),
        # use variance of neighborhood mean instead of group variance
        gs.mapcalc('v%d = if(v.g/chisq < ${mv}, ${vm}, v.g)' % i,
            vm = '(1.0/w)', mv = '(n/w)', quiet=True)
        tmp_rast.add('v%d' % i)

    # get arbitrarily large value to fill null variance cells
    bigvar = gs.raster_info('v0')['max'] * 10

    # prep for refinement phase
    gs.run_command('g.rename', rast='z%d,%s' % (NUM_LEVELS, smooth),
        quiet=True)
    tmp_rast.remove('z%d' % NUM_LEVELS)
    gs.run_command('g.rename', rast='v%d,v.smooth' % NUM_LEVELS,
        quiet=True)
    tmp_rast.add('v.smooth')
    tmp_rast.remove('v%d' % NUM_LEVELS)

    # refine, smooth, and combine each layer in turn
    for j in reversed(range(NUM_LEVELS)):

        i = j + 1
        gs.message('Refining from %d to %d' % (i, j), flag='i')

        # create smoothed higher resolution versions of z and v
        # using weight matrix equivalent to ArcGIS circle with radius 2
        refine_region()
        gs.run_command('r.neighbors', flags='ac', input=smooth,
            output='zs', method='average', size=5, overwrite=True,
            quiet=True)
        tmp_rast.add('zs')
        gs.run_command('r.neighbors', flags='ac', input='v.smooth',
            output='vs', method='average', size=5, overwrite=True,
            quiet=True)
        tmp_rast.add('vs')

        # combine two levels using least variance, using no-null
        # versions of finer z and v
        z_c = 'if(isnull(z%d), 0, z%d)' % (j, j)
        v_c = 'if(isnull(v%d), %f, v%d)' % (j, bigvar, j)
        gs.mapcalc('${smooth} = (${z_c}/${v_c} + zs/vs) / (1/${v_c} + 1/vs)',
            smooth=smooth, z_c=z_c, v_c=v_c, quiet=True)
        gs.mapcalc('v.smooth = 1 / (1/${v_c} + 1/vs)', v_c = v_c,
            quiet=True)

    cleanup()
    return None


def main():

    # process command options
    input = options['input']
    if not gs.find_file(input)['file']:
        gs.fatal(_("Raster map <%s> not found") % input)

    smooth = options['output']
    if gs.find_file(smooth)['file'] and not gs.overwrite():
        gs.fatal(_("Output map <%s> already exists") % smooth)

    sd = options['sd']
    try:
        sd = float(sd)
    except ValueError:
        if not gs.find_file(sd)['file']:
            gs.fatal(_("Raster map <%s> not found") % sd)

    alpha = float(options['alpha'])

    # set aside region for internal use
    gs.use_temp_region()

    # subset input if desired
    region = options.get('region')
    if region:
        if not gs.find_file(sd)['file']:
            gs.fatal(_("Raster map <%s> not found") % region)
        gs.message("Setting region to %s" % region, flag='i')
        gs.run_command('g.region', rast=region, align=input)
    else:
        gs.message("Using existing GRASS region", flag='i')
    gs.debug('='*50)
    gs.debug('\n'.join(gs.parse_command('g.region', 'p').keys()))
    gs.debug('='*50)

    multiscalesmooth(input, smooth, sd, alpha)

    # restore original region
    gs.del_temp_region()

    return None


if __name__ == '__main__':
    options, flags = gs.parser()
    atexit.register(cleanup)
    main()
