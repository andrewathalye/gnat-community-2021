#! /usr/bin/env python

# xspack

# Generate s-pack*.{ads,adb} from templates s-pack.ads.tmpl and s-pack.adb.tmpl

tmpl_base = "s-pack.ad%c.tmpl"


def read_template(part):
    return open(tmpl_base % part).readlines()


def output(pkg, bits, part):
    global tmpl

    bits_str_pad = "%02d" % bits
    if bits > 99:
        bits_str_dbl = "1 %d %d" % ((bits / 10) % 10, bits % 10)
    else:
        bits_str_dbl = "%d %d  " % (bits / 10, bits % 10)
    bits_str = "%d" % bits

    out = open(pkg % (bits, part), 'w')
    skip = False

    for line in tmpl[part]:
        if line.find('@even') == 0 and bits % 2 == 1:
            skip = True
        if line.find('@/even') == 0:
            skip = False
        if line[0] != '@' and not skip:
            line = line.replace('@@', bits_str_pad)
            line = line.replace('@ @  ', bits_str_dbl)
            line = line.replace('@', bits_str)
            out.write(line)


parts = ['s', 'b']
tmpl = {}

for part in parts:
    tmpl[part] = read_template(part)

for bits in range(1, 128):
    if bits & (bits - 1) == 0:
        # Power of two: no package generated
        continue

    if bits > 99:
        pkg_base = "s-pack%3d.ad%c"
    else:
        pkg_base = "s-pack%02d.ad%c"

    for part in parts:
        output(pkg_base, bits, part)
