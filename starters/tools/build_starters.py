"""Generate the starter-base MPD files for Brick Racer and Brick Sails.

LDraw coordinates: -Y up, 1 stud = 20 LDU, plate = 8, brick = 24.
Front / bow is towards LDraw -Z for every model (the games' convention).
"""
import os, sys, json
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # ldgeo.py sits next to this file
from ldgeo import Lib, mat

I = (1, 0, 0, 0, 1, 0, 0, 0, 1)
RZ = (0, 0, 1, 0, 1, 0, -1, 0, 0)     # part's long X axis runs along Z (local +X -> world -Z)
R180 = (-1, 0, 0, 0, 1, 0, 0, 0, -1)
R270 = (0, 0, -1, 0, 1, 0, 1, 0, 0)   # local +X -> world +Z
# Wheels: the part's spin axis is its local Z.
AX_POS = (0, 0, 1, 0, 1, 0, -1, 0, 0)  # local +Z -> world +X
AX_NEG = (0, 0, -1, 0, 1, 0, 1, 0, 0)  # local +Z -> world -X

# plates/bricks/tiles keyed by (studs along X, studs along Z) in their default orientation
PLATES = {(1, 1): '3024', (2, 1): '3023b', (3, 1): '3623', (4, 1): '3710', (6, 1): '3666', (8, 1): '3460',
          (10, 1): '4477', (2, 2): '3022', (3, 2): '3021', (4, 2): '3020', (6, 2): '3795', (8, 2): '3034',
          (4, 4): '3031', (6, 4): '3032', (8, 4): '3035', (10, 4): '3030', (12, 4): '3029', (6, 6): '3958',
          (8, 6): '3036', (12, 6): '3028', (16, 6): '3027', (8, 8): '41539'}
BRICKS = {(1, 1): '3005', (2, 1): '3004', (3, 1): '3622', (4, 1): '3010', (6, 1): '3009', (8, 1): '3008',
          (2, 2): '3003', (3, 2): '3002', (4, 2): '3001', (8, 2): '3007', (10, 2): '3006'}
TILES = {(1, 1): '3070b', (2, 1): '3069b', (3, 1): '63864', (4, 1): '2431', (6, 1): '6636', (8, 1): '4162',
         (2, 2): '3068b', (4, 2): '87079'}


class Model:
    def __init__(self):
        self.lines = []   # (comment) or (colour, pos, rot, part)

    def comment(self, text):
        self.lines.append(('0 // ' + text,))

    def add(self, part, colour, x, y, z, rot=I):
        if not part.endswith('.dat'):
            part += '.dat'
        self.lines.append((colour, (x, y, z), rot, part))

    def rect(self, table, colour, x0, x1, z0, z1, y):
        """A part covering the stud rectangle x0..x1, z0..z1 (LDU) with its top at y."""
        w, l = round((x1 - x0) / 20), round((z1 - z0) / 20)
        cx, cz = (x0 + x1) / 2, (z0 + z1) / 2
        if (w, l) in table:
            self.add(table[(w, l)], colour, cx, y, cz, I)
        elif (l, w) in table:
            self.add(table[(l, w)], colour, cx, y, cz, RZ)
        else:
            raise ValueError(f'no part for {w}x{l}')

    def plate(self, *a): self.rect(PLATES, *a)
    def brick(self, *a): self.rect(BRICKS, *a)
    def tile(self, *a): self.rect(TILES, *a)

    def parts(self):
        return [l for l in self.lines if len(l) == 4]

    def ldraw_lines(self):
        out = []
        for l in self.lines:
            if len(l) == 1:
                out.append(l[0])
            else:
                c, (x, y, z), r, p = l
                nums = [x, y, z] + list(r)
                out.append('1 %d %s %s' % (c, ' '.join(fmt(v) for v in nums), p))
        return out


def fmt(v, nd=3):
    v = round(float(v), nd)
    return str(int(v)) if v == int(v) else repr(v)


LIB = Lib()


def bounds(model):
    lo, hi = np.full(3, 1e9), np.full(3, -1e9)
    for c, (x, y, z), r, p in model.parts():
        M = mat([x, y, z] + list(r))
        a, b = LIB.bounds(p, M)
        lo, hi = np.minimum(lo, a), np.maximum(hi, b)
    return lo, hi


def write_mpd(path, key, title, description, model, extra_header, base_title):
    parts = model.parts()
    lo, hi = bounds(model)
    size = hi - lo
    base = key + '-base.ldr'
    header = [
        f'0 FILE {key}.ldr',
        f'0 {title}',
        f'0 Name: {title}',
        '0 Author: Brick Family starter bases',
        '0 !LICENSE Licensed under CC BY 4.0 : see CAreadme.txt',
        f'0 // Description: {description}',
        f'0 // Parts: {len(parts)} (all inside the grouped submodel {base})',
    ] + [f'0 // {h}' for h in extra_header] + [
        f'0 // Bounds LDU: x {fmt(lo[0],0)}..{fmt(hi[0],0)}, y {fmt(lo[1],0)}..{fmt(hi[1],0)}, z {fmt(lo[2],0)}..{fmt(hi[2],0)}'
        f' (size {fmt(size[0],0)} x {fmt(size[1],0)} x {fmt(size[2],0)})',
        '0 // Front / bow towards -Z, -Y up. The whole base is one submodel reference: move it as one piece.',
        f'1 16 0 0 0 1 0 0 0 1 0 0 0 1 {base}',
        '0 NOFILE',
        f'0 FILE {base}',
        f'0 {base_title}',
        f'0 Name: {base}',
        '0 Author: Brick Family starter bases',
    ]
    text = '\n'.join(header + model.ldraw_lines() + ['0 NOFILE']) + '\n'
    with open(path, 'w') as f:
        f.write(text)
    counts = {}
    for c, pos, r, p in parts:
        counts[p] = counts.get(p, 0) + 1
    return {'file': os.path.basename(path), 'title': title, 'parts': len(parts),
            'bounds_min': [round(v, 1) for v in lo], 'bounds_max': [round(v, 1) for v in hi],
            'size': [round(v, 1) for v in size], 'inventory': counts}


# ───────────────────────────── Racer ─────────────────────────────

def wheel_pair(m, rim, tyre, x, y, z, outward_pos, rim_c=71, tyre_c=0, hub_inward=False):
    """Rim + tyre sharing one origin (the brick-studio convention Health checks)."""
    if hub_inward:
        rot = AX_NEG if outward_pos else AX_POS
    else:
        rot = AX_POS if outward_pos else AX_NEG
    m.add(rim, rim_c, x, y, z, rot)
    m.add(tyre, tyre_c, x, y, z, rot)


def holder_axle(m, z, rim, tyre, hub_x, colour=72):
    """4600 wheel holder (top at y=0, pins at y=5) and two wheels."""
    m.add('4600', colour, 0, 0, z)
    wheel_pair(m, rim, tyre, hub_x, 5, z, True)
    wheel_pair(m, rim, tyre, -hub_x, 5, z, False)


def holder_pair(m, z, rim, tyre, rim_hub, colour=72):
    """Two 4488 single-pin holders side by side (4 studs wide, top at y=0, pins
    at y=5 reaching x=+-54) and a wheel on each pin; rim_hub is the rim centre's
    distance from the pin root at x=+-42."""
    m.add('4488', colour, 20, 0, z, I)
    m.add('4488', colour, -20, 0, z, R180)
    wheel_pair(m, rim, tyre, 42 + rim_hub, 5, z, True)
    wheel_pair(m, rim, tyre, -42 - rim_hub, 5, z, False)


def long_plank():
    m = Model()
    m.comment('Front (z=-140) and rear (z=+140) axles: pairs of 4488 holders, 4624 rims, 3641 tyres')
    for z in (-140, 140):
        holder_pair(m, z, '4624', '3641', 8)
    m.comment('Frame layer 1 (y=-8): two 2-wide stringers with staggered joints, wheels run outside them')
    m.plate(4, -40, 0, -180, -20, -8)
    m.plate(4, -40, 0, -20, 140, -8)
    m.plate(4, -40, 0, 140, 180, -8)
    m.plate(4, 0, 40, -180, -140, -8)
    m.plate(4, 0, 40, -140, 20, -8)
    m.plate(4, 0, 40, 20, 180, -8)
    m.comment('Frame layer 2 (y=-16): 2x4 cross plates tie the stringers together')
    for z0 in range(-180, 180, 40):
        m.plate(4, -40, 40, z0, z0 + 40, -16)
    m.comment('Side sills (1-brick rails) leave a 2-wide stud channel down the middle')
    m.brick(72, -40, -20, -140, 20, -40)
    m.brick(72, -40, -20, 20, 140, -40)
    m.brick(72, 20, 40, -140, -20, -40)
    m.brick(72, 20, 40, -20, 140, -40)
    m.comment('Nose and tail: 45 degree 2x2 slopes')
    m.add('3039', 14, -20, -40, -150, I)
    m.add('3039', 14, 20, -40, -150, I)
    m.add('3039', 4, -20, -40, 150, R180)
    m.add('3039', 4, 20, -40, 150, R180)
    return m


def compact_kart():
    m = Model()
    m.comment('Axles 120 LDU apart: pairs of 4488 holders, 42610 Technic rims, 50951 tyres')
    for z in (-60, 60):
        holder_pair(m, z, '42610', '50951', 10)
    m.comment('Two 2x4 spacer plates per axle tie each holder pair and lift the floor over the tyres (tops at y=-14)')
    for z in (-60, 60):
        m.plate(72, -40, 40, z - 20, z + 20, -8)
        m.plate(72, -40, 40, z - 20, z + 20, -16)
    m.comment('Floor layer 1 (y=-24): three 2x8 stringers, 6 studs wide')
    for x0 in (-60, -20, 20):
        m.plate(0, x0, x0 + 40, -80, 80, -24)
    m.comment('Floor layer 2 (y=-32): 2x6 cross plates')
    for z0 in (-80, -40, 0, 40):
        m.plate(1, -60, 60, z0, z0 + 40, -32)
    m.comment('Bumpers: 45 degree slopes front and back')
    for x in (-40, 0, 40):
        m.add('3039', 1, x, -56, -50, I)
        m.add('3039', 1, x, -56, 50, R180)
    m.comment('Side pods: 1x4 bricks between the bumpers')
    m.brick(15, -60, -40, -40, 40, -56)
    m.brick(15, 40, 60, -40, 40, -56)
    m.comment('Number plates: 1x2 tiles behind the front and in front of the rear bumper')
    m.tile(14, -20, 20, -40, -20, -40)
    m.tile(14, -20, 20, 20, 40, -40)
    return m


def wide_offroad():
    m = Model()
    m.comment('Technic rails (1x16 bricks with holes) along both sides; holes at y=0')
    m.add('3703', 72, -70, -10, 0, RZ)
    m.add('3703', 72, 70, -10, 0, RZ)
    m.comment('Axles 10L through the rail holes at z=-100 and z=+100')
    m.add('3737', 0, 0, 0, -100, I)
    m.add('3737', 0, 0, 0, 100, I)
    m.comment('55982 axle-hole rims, hub against the rail, 56891 off-road tyres')
    for z in (-100, 100):
        wheel_pair(m, '55982', '56891', 100, 0, z, True, rim_c=14, hub_inward=True)
        wheel_pair(m, '55982', '56891', -100, 0, z, False, rim_c=14, hub_inward=True)
    m.comment('Deck layer 1 (y=-18): 2x8 cross plates span the 8-stud track')
    for z0 in range(-160, 160, 40):
        m.plate(72, -80, 80, z0, z0 + 40, -18)
    m.comment('Deck layer 2 (y=-26): long 2x8 stringers, staggered')
    for x0, offs in ((-80, 0), (-40, 80), (0, 0), (40, 80)):
        zs = [-160, -80, 0, 80, 160] if offs == 0 else [-160, -120, -40, 40, 120, 160]
        for a, b in zip(zs, zs[1:]):
            m.plate(4, x0, x0 + 40, a, b, -26)
    m.comment('Nose and tail: 45 degree slopes across the deck ends')
    for x in (-60, -20, 20, 60):
        m.add('3039', 0, x, -50, -130, I)
        m.add('3039', 0, x, -50, 130, R180)
    return m


def turntable_kart():
    m = Model()
    m.comment('Steering bogie (front, z=-120): two 4488 holders joined by a 3680/3679 turntable')
    holder_pair(m, -120, '42610', '50951', 10)
    m.add('3680', 72, 0, -8, -120)
    m.add('3679', 72, 0, -8, -120)
    m.comment('Rear axle (z=+120): two 4488 holders and a 2x4 plate, level with the turntable top')
    holder_pair(m, 120, '42610', '50951', 10)
    m.plate(72, -40, 40, 100, 140, -8)
    m.comment('Spine layer 1 (y=-16): 2 studs wide, so the front wheels steer clear of the body')
    m.plate(0, -20, 20, -160, 0, -16)
    m.plate(0, -20, 20, 0, 160, -16)
    m.comment('Layer 2 (y=-24): narrow spine at the axles, 2x6 cross plates between them')
    m.plate(0, -20, 20, -160, -100, -24)
    m.plate(0, -20, 20, -100, -60, -24)
    for z0 in (-60, -20, 20, 60):
        m.plate(4, -60, 60, z0, z0 + 40, -24)
    m.plate(0, -20, 20, 100, 160, -24)
    m.comment('Layer 3 (y=-32): side pods and spine plates bind the cross plates')
    m.plate(4, -60, -20, -60, 100, -32)
    m.plate(4, 20, 60, -60, 100, -32)
    m.plate(0, -20, 20, -60, 100, -32)
    m.plate(0, -20, 20, -120, -60, -32)
    m.plate(0, -20, 20, 100, 120, -32)
    m.comment('Side rails, nose and tail slopes, and two tiles on the spine')
    m.brick(4, -60, -40, -60, 100, -56)
    m.brick(4, 40, 60, -60, 100, -56)
    m.add('3039', 14, 0, -48, -130, I)
    m.add('3039', 14, 0, -48, 130, R180)
    m.tile(0, -20, 20, -100, -80, -40)
    m.tile(0, -20, 20, 100, 120, -40)
    return m


# ───────────────────────────── Sails ─────────────────────────────
# Hull convention (Sails.Boats): hull bricks have their top at y=0, the keel hangs
# below at y=24..48, the bow points to -Z. A flat plate deck sits on top (y=-8).

def run(m, table, c, x0, x1, zs, y):
    """Parts laid end to end along Z between consecutive breakpoints zs."""
    for a, b in zip(zs, zs[1:]):
        m.rect(table, c, x0, x1, a, b, y)


def narrow_racer():
    m = Model()
    c = 1
    m.comment('Upper hull course (y 0..24): 4 studs wide, staggered 2x bricks')
    run(m, BRICKS, c, -40, 0, [-120, -40, 40, 120, 160], 0)
    run(m, BRICKS, c, 0, 40, [-120, -80, 0, 80, 160], 0)
    m.comment('Bow: inverted 2x2 slopes')
    m.add('3660b', c, -20, 0, -130, I)
    m.add('3660b', c, 20, 0, -130, I)
    m.comment('Lower hull course (y 24..48): 2 wide, inverted slopes fore and aft')
    run(m, BRICKS, c, -20, 20, [-100, -20, 60, 120], 24)
    m.add('3660b', c, 0, 24, -110, I)
    m.add('3660b', c, 0, 24, 130, R180)
    m.comment('Deep fin keel (y 48..96): a 2x4 and a 2x2 brick on the centreline')
    m.brick(0, -20, 20, -40, 40, 48)
    m.brick(0, -20, 20, -20, 20, 72)
    m.comment('Flat deck (y -8..0): plates with joints off the hull joints, wing plate at the bow')
    m.plate(15, -40, 40, -120, 80, -8)
    m.plate(15, -40, 40, 80, 160, -8)
    m.add('51739', 15, 0, -8, -140, I)
    m.comment('Gunwales (y -16..-8): 1-wide plates round the deck edge')
    run(m, PLATES, 0, -40, -20, [-120, 80, 160], -16)
    run(m, PLATES, 0, 20, 40, [-120, -40, 80, 160], -16)
    m.plate(0, -20, 20, 140, 160, -16)
    return m


def wide_sturdy():
    m = Model()
    c = 4
    m.comment('Upper hull course (y 0..24): 6 studs wide, three staggered columns')
    run(m, BRICKS, c, -60, -20, [-100, -20, 60, 140], 0)
    run(m, BRICKS, c, -20, 20, [-100, -60, 20, 100, 140], 0)
    run(m, BRICKS, c, 20, 60, [-100, -20, 60, 140], 0)
    m.comment('Blunt bow: three inverted 2x2 slopes')
    for x in (-40, 0, 40):
        m.add('3660b', c, x, 0, -110, I)
    m.comment('Lower hull course (y 24..48): 4 wide, inverted slopes at bow and stern')
    run(m, BRICKS, c, -40, 0, [-80, 0, 80, 100], 24)
    run(m, BRICKS, c, 0, 40, [-80, -40, 40, 100], 24)
    for x in (-20, 20):
        m.add('3660b', c, x, 24, -90, I)
        m.add('3660b', c, x, 24, 110, R180)
    m.comment('Keel (y 48..72): 2 wide with inverted slopes')
    run(m, BRICKS, 0, -20, 20, [-60, 20, 80], 48)
    m.add('3660b', 0, 0, 48, -70, I)
    m.add('3660b', 0, 0, 48, 90, R180)
    m.comment('Flat deck (y -8..0): one 6x12 plate and a 2x6 over the bow')
    m.plate(15, -60, 60, -100, 140, -8)
    m.plate(15, -60, 60, -140, -100, -8)
    m.comment('Gunwales (y -16..-8): 1-wide plates all round')
    run(m, PLATES, 0, -60, -40, [-140, -60, 20, 100, 140], -16)
    run(m, PLATES, 0, 40, 60, [-140, -60, 20, 100, 140], -16)
    m.plate(0, -40, 40, -140, -120, -16)
    m.plate(0, -40, 40, 120, 140, -16)
    return m


def long_cruiser():
    m = Model()
    c = 72
    m.comment('Upper hull course (y 0..24): 4 wide, 26 studs long, staggered')
    run(m, BRICKS, c, -40, 0, [-240, -160, -80, 0, 80, 160, 240, 280], 0)
    run(m, BRICKS, c, 0, 40, [-240, -200, -120, -40, 40, 120, 200, 280], 0)
    m.comment('Bow: inverted 2x2 slopes')
    m.add('3660b', c, -20, 0, -250, I)
    m.add('3660b', c, 20, 0, -250, I)
    m.comment('Lower hull course (y 24..48): 2 wide, inverted slopes fore and aft')
    run(m, BRICKS, c, -20, 20, [-220, -140, -60, 20, 100, 180, 220], 24)
    m.add('3660b', c, 0, 24, -230, I)
    m.add('3660b', c, 0, 24, 230, R180)
    m.comment('Long keel (y 48..72): two 2x8 bricks')
    run(m, BRICKS, 0, -20, 20, [-160, 0, 160], 48)
    m.comment('Flat deck (y -8..0): 4x12 + 4x10 + 4x4 plates, wing plate at the bow')
    m.plate(15, -40, 40, -240, 0, -8)
    m.plate(15, -40, 40, 0, 200, -8)
    m.plate(15, -40, 40, 200, 280, -8)
    m.add('51739', 15, 0, -8, -260, I)
    m.comment('Gunwales (y -16..-8): 1-wide plates down both sides')
    run(m, PLATES, 0, -40, -20, [-240, -40, 160, 280], -16)
    run(m, PLATES, 0, 20, 40, [-240, -80, 80, 240, 280], -16)
    m.plate(0, -20, 20, 260, 280, -16)
    return m


def round_bow():
    m = Model()
    c = 14
    m.comment('Upper hull course (y 0..24): 6 wide, 2x2 round bricks round off the bow corners')
    run(m, BRICKS, c, -60, -20, [-120, -40, 40, 120], 0)
    run(m, BRICKS, c, -20, 20, [-120, -80, 0, 80, 120], 0)
    run(m, BRICKS, c, 20, 60, [-120, -40, 40, 120], 0)
    m.add('3941', c, -40, 0, -140, I)
    m.add('3941', c, 40, 0, -140, I)
    m.add('3660b', c, 0, 0, -130, I)
    m.comment('Lower hull course (y 24..48): 4 wide, curved inverted slopes under the bow')
    run(m, BRICKS, c, -40, 0, [-80, 0, 60, 80], 24)
    run(m, BRICKS, c, 0, 40, [-80, -40, 20, 80], 24)
    for x in (-30, -10, 10, 30):
        m.add('24201', c, x, 24, -90, R180)
    m.add('3660b', c, -20, 24, 90, R180)
    m.add('3660b', c, 20, 24, 90, R180)
    m.comment('Keel (y 48..72): 2 wide with inverted slopes')
    run(m, BRICKS, 0, -20, 20, [-60, 20, 60], 48)
    m.add('3660b', 0, 0, 48, -70, I)
    m.add('3660b', 0, 0, 48, 70, R180)
    m.comment('Flat deck (y -8..0): 6x12 plate, round 2x2 plates over the round bricks')
    m.plate(15, -60, 60, -120, 120, -8)
    m.add('4032b', 15, -40, -8, -140, I)
    m.add('4032b', 15, 40, -8, -140, I)
    m.plate(15, -20, 20, -160, -120, -8)
    m.comment('Gunwales and foredeck (y -16..-8) lock the round bow plates to the main deck')
    run(m, PLATES, 0, -60, -40, [-140, -60, 20, 100, 120], -16)
    run(m, PLATES, 0, 40, 60, [-140, -80, 0, 80, 120], -16)
    m.plate(15, -20, 20, -140, -60, -16)
    m.plate(0, -40, 40, 100, 120, -16)
    return m


def build_all(root_racer, root_sails):
    os.makedirs(root_racer, exist_ok=True)
    os.makedirs(root_sails, exist_ok=True)
    racer = [
        ('long-plank', 'Long Plank Racer Base', long_plank, 'Long, low 4 x 18 stud plank frame, small 4624/3641 wheels outside it on 4488 holders; fixed axles.'),
        ('compact-kart', 'Compact Kart Base', compact_kart, 'Short 6 x 8 stud kart floor over 42610/50951 wheels on 4488 holders; fixed axles.'),
        ('wide-offroad', 'Wide Off-Road Base', wide_offroad, 'Wide 8 x 16 stud deck on Technic rails and axles with 55982/56891 off-road wheels.'),
        ('turntable-kart', 'Turntable Steering Kart Base', turntable_kart, 'Narrow spine with side pods; front axle steers on a 3680/3679 turntable.'),
    ]
    out = {'racer': [], 'sails': []}
    for key, title, fn, desc in racer:
        m = fn()
        info = write_mpd(os.path.join(root_racer, key + '.mpd'), key, title, desc, m,
                         ['Wheels: 4 (rim + tyre pairs on two axles)'], title.replace(' Base', '') + ' chassis (grouped)')
        out['racer'].append(info)
    sails = [
        ('narrow-racer', 'Narrow Racing Hull', narrow_racer, 'Slim 4 x 16 stud hull with a wing-plate bow, flat plate deck and deep fin keel.'),
        ('wide-sturdy', 'Wide Sturdy Hull', wide_sturdy, 'Beamy 6 x 14 stud hull, three courses deep, blunt bow and a short keel.'),
        ('long-cruiser', 'Long Cruiser Hull', long_cruiser, 'Long 4 x 28 stud hull with a pointed bow and a full-length 1-wide keel.'),
        ('round-bow', 'Round Bow Hull', round_bow, 'Round-nosed 6 x 14 stud hull: round bricks and curved inverted slopes at the bow.'),
    ]
    for key, title, fn, desc in sails:
        m = fn()
        info = write_mpd(os.path.join(root_sails, key + '.mpd'), key, title, desc, m,
                         ['Hull only: no mast or sail (add them in the editor)'], title + ' (grouped)')
        out['sails'].append(info)
    return out


if __name__ == '__main__':
    res = build_all(sys.argv[1], sys.argv[2])
    print(json.dumps(res, indent=1))
