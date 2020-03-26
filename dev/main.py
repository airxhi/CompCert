import sys
import bap
import re
from translation import insn_map, op_map

def convert(inst):
    if inst[:3] == "Reg":
        x = inst[5:-2]
        if x[0] == "E":
            x = "R" + x[1:]
        return x
    else:
        return eval(inst[4:-1])

def typify(operands):
    ops = re.findall("Reg\(\"[A-Z]*\"\)|Imm\(0x[0-9]*\)", operands)
    ops = list(map(convert, ops))
    return ops

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Wrong number of arguments")
    path = sys.argv[1]

    img = bap.image(path)
    segments = img.segments
    insns_all= {}
    for s in segments:
        for sym in s.symbols:
            # print(">"+sym.name)
            i = 0
            insns = []
            for insn in bap.disasm(sym):
                insns.append((str(insn.name), str(insn.operands)))
                # print(hex(insn.addr- 0xc0000040) + " " + str(insn.name)+str(insn.operands))
            insns_all[sym.name] = insns

    for a in insns_all:
        # print "=============="
        insns = insns_all[a]
        # for i in insns:
        #     print str(i[0]) + " " + str(i[1])
        x = re.findall("Imm\(0x[0-9]*\)", insns[1][1])[1]
        sz = eval(re.findall("0x[0-9]*", x)[0])
        # print "sz       = %d" % sz 

        x = re.findall("Imm\(0x[0-9]*\)", insns[0][1])[0]
        ofs_ra = eval(re.findall("0x[0-9]*", x)[0])
        # print "ofs_ra   = %d" % ofs_ra 

        x = re.findall("Imm\(0x[0-9]*\)", insns[2][1])[1]
        ofs_link = eval(re.findall("0x[0-9]*", x)[0])
        # print "ofs_link = %d" % ofs_link 
        # print "--------------"

        print "Pallocframe %d %d %d" % (sz, ofs_ra, ofs_link)
        ims = insns[3:-2]
        ims = list(map(lambda x : (insn_map[x[0]], typify(x[1])), ims))
        for i in ims:
            out = i[0]
            sep = " "
            for j in op_map[i[0]]:
                out = out + sep
                out = out + str(i[1][j])
            print (out)
        print "Pfreeframe %d %d %d" % (sz, ofs_ra, ofs_link)
        print insn_map[insns[-1][0]]
        print("> " + a)

    # print '\n'.join(insn.asm for insn in bap.disasm(sym))
