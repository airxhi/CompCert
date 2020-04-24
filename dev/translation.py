insn_map = {
    "MOV64rr": "Pmov_rr",  # ireg * ireg
    "MOV32ri" : "Pmovl_ri", # of ireg * Int.int
    "MOV64ri32" : "Pmovq_ri",
    # Pmovq_ri of ireg * Int64.int
    # Pmov_rs of ireg * ident
    "MOV32rm" : "Pmovl_rm", # of ireg * addrmode
    # "MOV64rm" : "Pmovq_rm", # of ireg * addrmode
    "MOV32mr" : "Pmovl_mr", # of addrmode * ireg
    # "MOV64mr" : "Pmovq_mr", # of addrmode * ireg
    "MOVAPDrr" : "Pmovsd_ff", # of freg * freg
    # Pmovsd_fi of freg * float
    # Pmovsd_fm of freg * addrmode
    # Pmovsd_mf of addrmode * freg
    # Pmovss_fi of freg * float32
    "MOVSSrm" : "Pmovss_fm", # of freg * addrmode
    # Pmovss_mf of addrmode * freg
    # Pfldl_m of addrmode
    # Pfstpl_m of addrmode
    # Pflds_m of addrmode
    # Pfstps_m of addrmode
    # Pmovb_mr of addrmode * ireg
    # Pmovw_mr of addrmode * ireg
    # Pmovzb_rr of ireg * ireg
    # Pmovzb_rm of ireg * addrmode
    # Pmovsb_rr of ireg * ireg
    # Pmovsb_rm of ireg * addrmode
    # Pmovzw_rr of ireg * ireg
    # Pmovzw_rm of ireg * addrmode
    # Pmovsw_rr of ireg * ireg
    # Pmovsw_rm of ireg * addrmode
    # Pmovzl_rr of ireg * ireg
    # Pmovsl_rr of ireg * ireg
    # Pmovls_rr of ireg
    # Pcvtsd2ss_ff of freg * freg
    # Pcvtss2sd_ff of freg * freg
    # Pcvttsd2si_rf of ireg * freg
    # Pcvtsi2sd_fr of freg * ireg
    "CVTTSS2SIrr" : "Pcvttss2si_rf", # of ireg * freg
    "CVTSI2SSrr" : "Pcvtsi2ss_fr", # of freg * ireg
    # Pcvttsd2sl_rf of ireg * freg
    # Pcvtsl2sd_fr of freg * ireg
    # Pcvttss2sl_rf of ireg * freg
    # Pcvtsl2ss_fr of freg * ireg
    "LEA64_32r" : "Pleal", # of ireg * addrmode
    "LEA64r" : "Pleaq",    # of ireg * addrmode
    # Pnegl of ireg
    # Pnegq of ireg
    "ADD64ri8" : "Paddl_ri", # of ireg * Int.int
    # Paddq_ri of ireg * Int64.int
    "SUB32rr" : "Psubl_rr", # of ireg * ireg
    # Psubq_rr of ireg * ireg
    "IMUL32rr" : "Pimull_rr", # of ireg * ireg
    # Pimulq_rr of ireg * ireg
    # Pimull_ri of ireg * Int.int
    # Pimulq_ri of ireg * Int64.int
    # Pimull_r of ireg
    # Pimulq_r of ireg
    # Pmull_r of ireg
    # Pmulq_r of ireg
    # Pcltd
    # Pcqto
    # Pdivl of ireg
    # Pdivq of ireg
    # Pidivl of ireg
    # Pidivq of ireg
    # Pandl_rr of ireg * ireg
    # Pandq_rr of ireg * ireg
    # Pandl_ri of ireg * Int.int
    # Pandq_ri of ireg * Int64.int
    # Porl_rr of ireg * ireg
    # Porq_rr of ireg * ireg
    # Porl_ri of ireg * Int.int
    # Porq_ri of ireg * Int64.int
    # Pxorl_r of ireg
    # Pxorq_r of ireg
    "XOR32rr" : "Pxorl_rr", # of ireg * ireg
    # Pxorq_rr of ireg * ireg
    # Pxorl_ri of ireg * Int.int
    # Pxorq_ri of ireg * Int64.int
    # Pnotl of ireg
    # Pnotq of ireg
    # Psall_rcl of ireg
    # Psalq_rcl of ireg
    # Psall_ri of ireg * Int.int
    # Psalq_ri of ireg * Int.int
    # Pshrl_rcl of ireg
    # Pshrq_rcl of ireg
    # Pshrl_ri of ireg * Int.int
    # Pshrq_ri of ireg * Int.int
    # Psarl_rcl of ireg
    # Psarq_rcl of ireg
    # Psarl_ri of ireg * Int.int
    # Psarq_ri of ireg * Int.int
    # Pshld_ri of ireg * ireg * Int.int
    # Prorl_ri of ireg * Int.int
    # Prorq_ri of ireg * Int.int
    # Pcmpl_rr of ireg * ireg
    "CMP32rr" : "Pcmpl_rr",
    # Pcmpq_rr of ireg * ireg
    "CMP32ri8" : "Pcmpl_ri", # of ireg * Int.int
    # Pcmpq_ri of ireg * Int64.int
    "TEST32rr" : "Ptestl_rr", # Ptestl_rr of ireg * ireg
    # Ptestq_rr of ireg * ireg
    # Ptestl_ri of ireg * Int.int
    # Ptestq_ri of ireg * Int64.int
    # Pcmov of testcond * ireg * ireg
    # Psetcc of testcond * ireg
    # Paddd_ff of freg * freg
    # Psubd_ff of freg * freg
    # Pmuld_ff of freg * freg
    # Pdivd_ff of freg * freg
    # Pnegd of freg
    # Pabsd of freg
    # Pcomisd_ff of freg * freg
    # Pxorpd_f of freg
    "ADDSSrr" : "Padds_ff", #Padds_ff of freg * freg
    # Psubs_ff of freg * freg
    "MULSSrr" : "Pmuls_ff", # of freg * freg
    # Pdivs_ff of freg * freg
    # Pnegs of freg
    # Pabss of freg
    # Pcomiss_ff of freg * freg
    # Pxorps_f of freg
    "JMP_1" : "Pjmp_l",
    # Pjmp_l of label
    "JMP_4" : "Pjmp_s",
    # Pjmp_s of ident * signature
    # Pjmp_r of ireg * signature
    "JL_1" : "Pjcc_L",
    "JG_1" : "Pjcc_G", 
    "JGE_1" : "Pjcc_GE", # Pjcc of testcond * label
    # Pjcc2 of testcond * testcond * label
    # Pjmptbl of ireg * label list
    # Pcall_s of ident * signature
    "CALL64pcrel32" : "Pcall_s", 
    # Pcall_r of ireg * signature
    # Pmov_rm_a of ireg * addrmode
    "MOV64rm" : "Pmov_rm_a",
    "MOV64mr" : "Pmov_mr_a", # of addrmode * ireg
    # Pmovsd_fm_a of freg * addrmode
    # Pmovsd_mf_a of addrmode * freg
    # Plabel of label
    # Pallocframe of coq_Z * Ptrofs.int * Ptrofs.int
    # Pfreeframe of coq_Z * Ptrofs.int * Ptrofs.int
    # Pbuiltin of external_function * preg builtin_arg list * preg builtin_res
    # Padcl_ri of ireg * Int.int
    # Padcl_rr of ireg * ireg
    # Paddl_mi of addrmode * Int.int
    # Paddl_rr of ireg * ireg
    # Pbsfl of ireg * ireg
    # Pbsfq of ireg * ireg
    # Pbsrl of ireg * ireg
    # Pbsrq of ireg * ireg
    # Pbswap64 of ireg
    # Pbswap32 of ireg
    # Pbswap16 of ireg
    # Pcfi_adjust of Int.int
    # Pfmadd132 of freg * freg * freg
    # Pfmadd213 of freg * freg * freg
    # Pfmadd231 of freg * freg * freg
    # Pfmsub132 of freg * freg * freg
    # Pfmsub213 of freg * freg * freg
    # Pfmsub231 of freg * freg * freg
    # Pfnmadd132 of freg * freg * freg
    # Pfnmadd213 of freg * freg * freg
    # Pfnmadd231 of freg * freg * freg
    # Pfnmsub132 of freg * freg * freg
    # Pfnmsub213 of freg * freg * freg
    # Pfnmsub231 of freg * freg * freg
    # Pmaxsd of freg * freg
    # Pminsd of freg * freg
    # Pmovb_rm of ireg * addrmode
    # Pmovsq_mr of addrmode * freg
    # Pmovsq_rm of freg * addrmode
    # Pmovsb
    # Pmovsw
    # Pmovw_rm of ireg * addrmode
    # Pnop
    # Prep_movsl
    # Psbbl_rr of ireg * ireg
    # Psqrtsd of freg * freg
    # Psubl_ri of ireg * Int.int
    "SUB64ri8" : "Psubq_ri", # of ireg * Int64.int
    "RETQ" : "Pret"
}

op_map = {
    "Pmov_rr": [0,1],  # ireg * ireg
    "Pmovl_ri" : [0,1], # of ireg * Int.int
    "Pmovq_ri" : [0,1], # of ireg * Int.int
    # Pmovq_ri of ireg * Int64.int
    # Pmov_rs of ireg * ident
    "Pmovl_rm" : [0,1,3,2,4], #of ireg * addrmode
    "Pmovq_rm" : [0,1,3,2,4], # of ireg * addrmode
    "Pmovl_mr" : [0,2,1,3,5], #of addrmode * ireg
    "Pmovq_mr" : [0,2,1,3,5], #of addrmode * ireg
    "Pmovsd_ff" : [0,1], # of freg * freg
    # Pmovsd_fi of freg * float
    # Pmovsd_fm of freg * addrmode
    # Pmovsd_mf of addrmode * freg
    # Pmovss_fi of freg * float32
    "Pmovss_fm" : [0,1,3,2,4], # of freg * addrmode
    # Pmovss_mf of addrmode * freg
    # Pfldl_m of addrmode
    # Pfstpl_m of addrmode
    # Pflds_m of addrmode
    # Pfstps_m of addrmode
    # Pmovb_mr of addrmode * ireg
    # Pmovw_mr of addrmode * ireg
    # Pmovzb_rr of ireg * ireg
    # Pmovzb_rm of ireg * addrmode
    # Pmovsb_rr of ireg * ireg
    # Pmovsb_rm of ireg * addrmode
    # Pmovzw_rr of ireg * ireg
    # Pmovzw_rm of ireg * addrmode
    # Pmovsw_rr of ireg * ireg
    # Pmovsw_rm of ireg * addrmode
    # Pmovzl_rr of ireg * ireg
    # Pmovsl_rr of ireg * ireg
    # Pmovls_rr of ireg
    # Pcvtsd2ss_ff of freg * freg
    # Pcvtss2sd_ff of freg * freg
    # Pcvttsd2si_rf of ireg * freg
    # Pcvtsi2sd_fr of freg * ireg
    "Pcvttss2si_rf" : [0,1], # of ireg * freg
    "Pcvtsi2ss_fr" : [0,1], # of freg * ireg
    # Pcvttsd2sl_rf of ireg * freg
    # Pcvtsl2sd_fr of freg * ireg
    # Pcvttss2sl_rf of ireg * freg
    # Pcvtsl2ss_fr of freg * ireg
    "Pleal" : [0,1,3,2,4], # of ireg * addrmode
    "Pleaq" : [0],    # of ireg * addrmode
    # Pnegl of ireg
    # Pnegq of ireg
    "Paddl_ri" : [0,2], # of ireg * Int.int
    "Paddq_ri" : [0,2], # of ireg * Int64.int
    "Psubl_rr" : [0,2], # of ireg * ireg
    # Psubq_rr of ireg * ireg
    "Pimull_rr" : [0,2], # of ireg * ireg
    # Pimulq_rr of ireg * ireg
    # Pimull_ri of ireg * Int.int
    # Pimulq_ri of ireg * Int64.int
    # Pimull_r of ireg
    # Pimulq_r of ireg
    # Pmull_r of ireg
    # Pmulq_r of ireg
    # Pcltd
    # Pcqto
    # Pdivl of ireg
    # Pdivq of ireg
    # Pidivl of ireg
    # Pidivq of ireg
    # Pandl_rr of ireg * ireg
    # Pandq_rr of ireg * ireg
    # Pandl_ri of ireg * Int.int
    # Pandq_ri of ireg * Int64.int
    # Porl_rr of ireg * ireg
    # Porq_rr of ireg * ireg
    # Porl_ri of ireg * Int.int
    # Porq_ri of ireg * Int64.int
    # Pxorl_r of ireg
    # Pxorq_r of ireg
    "Pxorl_rr" : [0,2], # of ireg * ireg
    # Pxorq_rr of ireg * ireg
    # Pxorl_ri of ireg * Int.int
    # Pxorq_ri of ireg * Int64.int
    # Pnotl of ireg
    # Pnotq of ireg
    # Psall_rcl of ireg
    # Psalq_rcl of ireg
    # Psall_ri of ireg * Int.int
    # Psalq_ri of ireg * Int.int
    # Pshrl_rcl of ireg
    # Pshrq_rcl of ireg
    # Pshrl_ri of ireg * Int.int
    # Pshrq_ri of ireg * Int.int
    # Psarl_rcl of ireg
    # Psarq_rcl of ireg
    # Psarl_ri of ireg * Int.int
    # Psarq_ri of ireg * Int.int
    # Pshld_ri of ireg * ireg * Int.int
    # Prorl_ri of ireg * Int.int
    # Prorq_ri of ireg * Int.int
    "Pcmpl_rr" : [0,1], # of ireg * ireg
    # Pcmpq_rr of ireg * ireg
    "Pcmpl_ri" : [0,1], # of ireg * Int.int
    # Pcmpq_ri of ireg * Int64.int
    "Ptestl_rr" : [0,1], # of ireg * ireg
    # Ptestq_rr of ireg * ireg
    # Ptestl_ri of ireg * Int.int
    # Ptestq_ri of ireg * Int64.int
    # Pcmov of testcond * ireg * ireg
    # Psetcc of testcond * ireg
    # Paddd_ff of freg * freg
    # Psubd_ff of freg * freg
    # Pmuld_ff of freg * freg
    # Pdivd_ff of freg * freg
    # Pnegd of freg
    # Pabsd of freg
    # Pcomisd_ff of freg * freg
    # Pxorpd_f of freg
    "Padds_ff" : [0,2],
    # Padds_ff of freg * freg
    # Psubs_ff of freg * freg
    "Pmuls_ff" : [0,2], # of freg * freg
    # Pdivs_ff of freg * freg
    # Pnegs of freg
    # Pabss of freg
    # Pcomiss_ff of freg * freg
    # Pxorps_f of freg
    "Pjmp_l" : [0],
    # Pjmp_l of label
    "Pjmp_s" : [],
    # Pjmp_s of ident * signature
    # Pjmp_r of ireg * signature
    "Pjcc_L" : [0],
    "Pjcc_GE" : [0], #  of testcond * label
    "Pjcc_G"  : [0],
    # Pjcc2 of testcond * testcond * label
    # Pjmptbl of ireg * label list
    "Pcall_s" : [], # of ident * signature
    "Pcall_r" : [0], # of ireg * signature
    "Pmov_rm_a" : [0,1,2,3,4], # of ireg * addrmode
    "Pmov_mr_a" : [0,2,1,3,5], # of addrmode * ireg
    # Pmovsd_fm_a of freg * addrmode
    # Pmovsd_mf_a of addrmode * freg
    # Plabel of label
    # Pallocframe of coq_Z * Ptrofs.int * Ptrofs.int
    # Pfreeframe of coq_Z * Ptrofs.int * Ptrofs.int
    # Pbuiltin of external_function * preg builtin_arg list * preg builtin_res
    # Padcl_ri of ireg * Int.int
    # Padcl_rr of ireg * ireg
    # Paddl_mi of addrmode * Int.int
    # Paddl_rr of ireg * ireg
    # Pbsfl of ireg * ireg
    # Pbsfq of ireg * ireg
    # Pbsrl of ireg * ireg
    # Pbsrq of ireg * ireg
    # Pbswap64 of ireg
    # Pbswap32 of ireg
    # Pbswap16 of ireg
    # Pcfi_adjust of Int.int
    # Pfmadd132 of freg * freg * freg
    # Pfmadd213 of freg * freg * freg
    # Pfmadd231 of freg * freg * freg
    # Pfmsub132 of freg * freg * freg
    # Pfmsub213 of freg * freg * freg
    # Pfmsub231 of freg * freg * freg
    # Pfnmadd132 of freg * freg * freg
    # Pfnmadd213 of freg * freg * freg
    # Pfnmadd231 of freg * freg * freg
    # Pfnmsub132 of freg * freg * freg
    # Pfnmsub213 of freg * freg * freg
    # Pfnmsub231 of freg * freg * freg
    # Pmaxsd of freg * freg
    # Pminsd of freg * freg
    # Pmovb_rm of ireg * addrmode
    # Pmovsq_mr of addrmode * freg
    # Pmovsq_rm of freg * addrmode
    # Pmovsb
    # Pmovsw
    # Pmovw_rm of ireg * addrmode
    # Pnop
    # Prep_movsl
    # Psbbl_rr of ireg * ireg
    # Psqrtsd of freg * freg
    # Psubl_ri of ireg * Int.int
    "Psubq_ri" : [0,2], # of ireg * Int64.int
    "Pret" : []
}