"""Fetch catalog MPD bundles and measure real LDraw part geometry (LDraw coords, -Y up)."""
import os, sys, tempfile, urllib.request, functools
import numpy as np
# Downloaded catalog bundles are cached outside the repo.
BDIR = os.environ.get('LDRAW_BUNDLE_CACHE', os.path.join(tempfile.gettempdir(), 'brick-starter-bundles'))
os.makedirs(BDIR, exist_ok=True)

def norm(n): return n.strip().lower().replace('\\', '/')

def fetch_bundle(part):
    part = norm(part)
    path = os.path.join(BDIR, part.replace('/', '_') + '.mpd')
    if not os.path.exists(path):
        stem = part[:-4] if part.endswith('.dat') else part
        url = f'https://catalog.palikkaharrastajat.fi/bundles/{stem}.mpd'
        import time
        for attempt in range(6):
            try:
                data = urllib.request.urlopen(url, timeout=60).read(); break
            except Exception as ex:
                if attempt == 5: raise
                time.sleep(2 + 3 * attempt)
        open(path, 'wb').write(data)
    return open(path, encoding='utf-8', errors='replace').read()

def parse_mpd(text):
    files, cur, order = {}, None, []
    for line in text.splitlines():
        t = line.strip()
        if t.startswith('0 FILE '):
            cur = norm(t[7:]); files[cur] = []; order.append(cur); continue
        if cur is None:
            cur = '__main__'; files[cur] = []; order.append(cur)
        files[cur].append(t)
    return files, order

def mat(vals):
    x, y, z, a, b, c, d, e, f, g, h, i = map(float, vals)
    M = np.eye(4); M[:3, :3] = [[a, b, c], [d, e, f], [g, h, i]]; M[:3, 3] = [x, y, z]
    return M

class Lib:
    def __init__(self): self.files = {}
    def load_part(self, part):
        part = norm(part)
        if part in self.files: return
        files, order = parse_mpd(fetch_bundle(part))
        for k, v in files.items():
            self.files.setdefault(k, v)
            # also index by basename for primitives referenced as e.g. 48/x.dat or s/x.dat
        if part not in self.files and order:
            self.files[part] = files[order[0]]
    def lookup(self, name):
        n = norm(name)
        for cand in (n, 'parts/' + n, 'p/' + n, 'parts/s/' + n.split('/')[-1] if n.startswith('s/') else None):
            if cand and cand in self.files: return self.files[cand]
        return None
    @functools.lru_cache(maxsize=None)
    def points(self, name):
        lines = self.lookup(name)
        if lines is None: raise KeyError(name)
        pts = []
        for t in lines:
            p = t.split()
            if not p: continue
            if p[0] in ('3', '4'):
                n = 3 if p[0] == '3' else 4
                for k in range(n): pts.append([float(v) for v in p[2 + 3 * k:5 + 3 * k]])
            elif p[0] == '1' and len(p) >= 15:
                sub = ' '.join(p[14:])
                M = mat(p[2:14])
                sp = self.points(sub)
                if len(sp):
                    h = np.hstack([sp, np.ones((len(sp), 1))]) @ M.T
                    pts.extend(h[:, :3].tolist())
        a = np.array(pts) if pts else np.zeros((0, 3))
        # dedupe to keep it small
        if len(a) > 5000: a = np.unique(np.round(a, 2), axis=0)
        return a
    def bounds(self, part, M=None):
        self.load_part(part)
        pts = self.points(norm(part))
        if M is not None:
            pts = (np.hstack([pts, np.ones((len(pts), 1))]) @ M.T)[:, :3]
        return pts.min(0), pts.max(0)

if __name__ == '__main__':
    L = Lib()
    for p in sys.argv[1:]:
        lo, hi = L.bounds(p)
        print(f'{p:12s} min {np.round(lo,1)} max {np.round(hi,1)} centre {np.round((lo+hi)/2,1)}')
