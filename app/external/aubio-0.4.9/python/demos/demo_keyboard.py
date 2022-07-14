#! /usr/bin/env python

def get_keyboard_edges(firstnote = 21, lastnote = 108):
    octaves = 10

    # build template of white notes
    scalew  = 12/7.
    xw_temp = [i*scalew for i in range(0,7)]
    # build template of black notes
    scaleb  = 6/7.
    xb_temp = [i*scaleb for i in [1,3,7,9,11]]

    xb,xw = [],[]
    for octave in range(octaves-1):
        for i in xb_temp:
            curnote = i+12*octave
            if  curnote > firstnote-1 and curnote < lastnote+1:
                xb = xb + [curnote]
    for octave in range(octaves-1):
        for i in xw_temp:
            curnote = i+12*octave
            if  curnote > firstnote-1 and curnote < lastnote+1:
                xw = xw + [curnote]

    return xb, xw, 2/3. *scaleb, 1/2. * scalew

def create_keyboard_patches(firstnote, lastnote, ax = None):
    import matplotlib.pyplot as plt
    from matplotlib.path import Path
    import matplotlib.patches as mpatches

    blacks, whites, b_width, w_width = get_keyboard_edges(firstnote, lastnote)

    if not ax:
        fig = plt.figure()
        ax = fig.add_subplot(111)

    verts, codes = [], []
    for white in whites:
        verts += [ (white - w_width, 0), (white - w_width, 1), (white + w_width, 1),  (white + w_width, 0) ]
        verts += [ (white - w_width, 0) ]
        codes  += [Path.MOVETO] + [Path.LINETO] * 4
    path = Path(verts, codes)
    patch = mpatches.PathPatch(path, facecolor= 'white', edgecolor='black', lw=1)
    ax.add_patch(patch)

    verts, codes = [], []
    for black in blacks:
        verts +=  [ (black - b_width, 0.33), (black - b_width, 1), (black + b_width, 1),  (black + b_width, 0.33) ]
        verts += [ (black - b_width, 0.33) ]
        codes += [Path.MOVETO] + [Path.LINETO] * 4
    path = Path(verts, codes)
    patch = mpatches.PathPatch(path, facecolor= 'black', edgecolor='black', lw=1)
    ax.add_patch(patch)

    ax.axis(xmin = firstnote, xmax = lastnote)

if __name__ == '__main__':

    import matplotlib.pyplot as plt
    create_keyboard_patches(firstnote = 58, lastnote = 84)
    plt.show()
