import sys
import bap

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Wrong number of arguments")
    path = sys.argv[1]
    function = sys.argv[2]

    img = bap.image(path)
    sym = img.get_symbol(function)

    for insn in bap.disasm(sym):
        print(insn.name)
        print(insn.operands)

    print '\n'.join(insn.asm for insn in bap.disasm(sym))

