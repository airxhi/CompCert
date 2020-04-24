import sys
import bap
import re
from subprocess import Popen, PIPE
from translation import insn_map, op_map

DEBUG = False

def get_type(a):
    return type(a)

def convert(inst):
    if inst[:3] == "Reg":
        x = inst[5:-2]
        if x[0] == "E":
            x = "R" + x[1:]
        return x
    else:
        return eval(inst[4:-1])

def extract_needed(insn_name, ops):
    mapping = op_map[insn_name]
    return [ops[m] for m in mapping]


def twos_comp(val, bits):
    """compute the 2's complement of int value val"""
    if (val & (1 << (bits - 1))) != 0: # if sign bit is set e.g., 8bit: 128-255
        val = val - (1 << bits)        # compute negative value
    return val   

class Instruction:

    def __init__(self, name, operands, address):
        self.name = name
        self.ops = operands
        self.addr = address

    def format_insn(self):
        try:
            self.name = insn_map[str(self.name)]
            self.ops = list(map(lambda op: op.arg, self.ops))
            self.ops = extract_needed(self.name, self.ops)
            for i in range(len(self.ops)):
                if str(self.ops[i])[0] == "E":
                    self.ops[i] = "R" + self.ops[i][1:]  
            for i in range (len(self.ops)):
                try:
                    op = int(self.ops[i])
                    if op > 2**20:
                        self.ops[i] = twos_comp(op, 64)
                except Exception as e:
                    pass
        except Exception as e:
            print "KeyError: " + e.message
            print(self)
            exit()

    def __str__(self):
        return self.name + " " + ' '.join(map(str,self.ops))

class Function:
    
    def __init__(self, name, value):
        self.name = name
        self.loc = value
        self.instructions = []

    def print_instructions(self):
        for insn in self.instructions:
            print(insn)
       

    def __str__(self):
        return '\n'.join([str(insn) for insn in self.instructions]) + "\n> " + self.name

    def do_frame_allocation(self):
        address = self.instructions[0].addr
        sz = self.instructions[0].ops[1]
        if sz == 8:
            sz = 16
        ofs_ra = sz - 8 # subtract word size
        ofs_link = self.instructions[2].ops[3]
        self.instructions = self.instructions[3:]
        self.instructions.insert(0, Instruction("Pallocframe", [sz, ofs_ra, ofs_link], address))
        
        idx = len(self.instructions)-1
        while idx >= 0:
            if self.instructions[idx].name == "Paddl_ri" and self.instructions[idx].ops[0] == "RSP":
                self.instructions[idx] = Instruction("Pfreeframe", [sz, ofs_ra, ofs_link], self.instructions[idx].addr)
            idx -= 1

    def do_relocations(self, relocations, functions):
        for insn in self.instructions:
            if insn.name == "Pcall_s" or insn.name == "Pjmp_s":
                relocation = relocations[insn.addr+1]
                reloc_name, reloc_addr = relocation.split('-')
                if reloc_name == "malloc":
                    insn.ops = [1]
                elif reloc_name == "free":
                    insn.ops = [2]
                else:
                    for f in functions:
                        if reloc_name == f.name:
                            insn.ops = [f.loc]
        label = 1
        found = False
        last_found = 0
        while not found:
            found = True
            for i in range(last_found+1, len(self.instructions)):
                insn = self.instructions[i]
                if insn.name[:4] == "Pjcc" or insn.name[:6] == "Pjmp_l":
                    if insn.ops[0] < 0:
                        last_found = i+1
                    else:
                        last_found = i
                    for j in range(len(self.instructions)):
                        insn2 = self.instructions[j]
                        if insn2.addr == insn.addr + insn.ops[0] or insn2.addr+1 == insn.addr + insn.ops[0] :
                            self.instructions[i].ops[0] = label
                            self.instructions.insert(j+1, Instruction("Plabel", [label], self.instructions[j].addr))
                            label += 1
                            break
                    found=False
                    break


def typify(operands):
    ops = re.findall("Reg\(\"[A-Za-z0-9]*\"\)|Imm\(0x[0-9|a-f]*\)", operands)
    ops = list(map(convert, ops))
    return ops

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Wrong number of arguments")
    path = sys.argv[1]

    relocations = {}
    p = Popen(["objdump", "-r", path], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate()
    rc = p.returncode
    output = output.strip().split('\n')[2:]
    if ".text" in output[0]:
        output = output[2:]
        for o in range(len(output)):
            if output[o] == "":
                break
        output = output[:o]
        for entry in output:
            address, type, relocation = re.split(r'\s{1,}', entry)
            address = int(address, 16)
            relocations[address] = relocation

    functions = []

    img = bap.image(path)
    segments = img.segments
    insns_all= {}

    value = 3
    for s in segments:
        for sym in s.symbols:
            func = Function(sym.name, value)
            value += 1

            for insn in bap.disasm(sym):
                func.instructions.append(Instruction(insn.name, insn.operands,insn.addr - 0xc0000040))
                if DEBUG:
                    print(hex(insn.addr - 0xc0000040) + " " + str(insn.name)+str(insn.operands))
            functions.append(func)
    #functions.reverse()

    for func in functions:
        for insn in func.instructions:
            insn.format_insn()
        func.do_frame_allocation()
        func.do_relocations(relocations, functions)
     # bap gives them in reversed order
    for func in functions:    
        print(func)


    # for a in insns_all:
    #     insns = insns_all[a]
    #     x = re.findall("Imm\(0x[0-9]*\)", insns[1][1])[1]
    #     sz = eval(re.findall("0x[0-9]*", x)[0])
    #     ofs_ra = sz-8 # size - wordsize
    #     x = re.findall("Imm\(0x[0-9]*\)", insns[2][1])[1]
    #     ofs_link = eval(re.findall("0x[0-9]*", x)[0])

    #     print ("Pallocframe %d %d %d" % (sz, ofs_ra, ofs_link))

    #     ims = insns[3:]
    #     ims = list(map(lambda x : (insn_map[x[0]], typify(x[1]), x), ims))

    #     for i in ims:
    #         out = i[0]
    #         sep = " "
    #         if out == "Paddl_ri":
    #             operands = op_map[i[0]]
    #             if i[1][operands[0]] == "RSP":
    #                 print "Pfreeframe %d %d %d" % (sz, ofs_ra, ofs_link)
    #                 continue
    #             else:
    #                 for j in operands:
    #                     out = out + sep
    #                     out = out + str(i[1][j])
    #         if out == "Pcall_s":
    #             relocate_to = relocations[i[2][2]+1] 
    #             if "malloc" in relocate_to:
    #                 print ("Pcall_s 1")
    #             elif "free" in relocate_to:
    #                 print ("Pcall_s 2")
    #             continue
    #         elif out == "Pjmp_s":
    #             relocate_to = relocations[i[2][2]+1] 
    #             if "malloc" in relocate_to:
    #                 print ("Pjmp_s 1")
    #             elif "free" in relocate_to:
    #                 print ("Pjmp_s 2")

    #             # will fail silently if invalid relocation
    #             continue

    #         elif out == "Pjcc":
    #             if "JGE_1" in i[2]:
    #                 out = out + sep
    #                 out = out + "GE" + sep
    #                 out = out + str(i[1][0])
    #         else:
    #             for j in op_map[i[0]]:
    #                 out = out + sep
    #                 out = out + str(i[1][j])
    #         print (out)
    #     # print "Pfreeframe %d %d %d" % (sz, ofs_ra, ofs_link)
    #     # print insn_map[insns[-1][0]]
    #     print("> " + a)

    # print '\n'.join(insn.asm for insn in bap.disasm(sym))
