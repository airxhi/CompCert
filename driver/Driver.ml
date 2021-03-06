(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

open Printf
open Commandline
open Clflags
open CommonOptions
open Timing
open Driveraux
open Frontend
open Assembler
open Linker
open Diagnostics

(* added by me *)
open Memory
open Asm
open Globalenvs
open Format
open Camlcoq
open AST
open Integers
open Values
open Events
open Ctypes
open Csyntax
open Csem
open Cexec
open Interp
open BinInt
open Memdata
open Coqlib
open Decidableplus
open Datatypes

let print_instruction i = match i with
| Pmov_rr (r1, r2) -> Printf.printf "Pmov_rr" ; ()
| Pmovl_ri (r1, r2) -> Printf.printf "Pmovl_ri" ; ()
| Pmovq_ri (r1, r2) -> Printf.printf "Pmovq_ri" ; ()
| Pmov_rs (r1, r2) -> Printf.printf "Pmov_rs" ; ()
| Pmovl_rm (r1, r2) -> Printf.printf "Pmovl_rm" ; ()
| Pmovq_rm (r1, r2) -> Printf.printf "Pmovq_rm" ; ()
| Pmovl_mr (r1, r2) -> Printf.printf "Pmovl_mr" ; ()
| Pmovq_mr (r1, r2) -> Printf.printf "Pmovq_mr" ; ()
| Pmovsd_ff (r1, r2) -> Printf.printf "Pmovsd_ff" ; ()
| Pmovsd_fi (r1, r2) -> Printf.printf "Pmovsd_fi" ; ()
| Pmovsd_fm (r1, r2) -> Printf.printf "Pmovsd_fm" ; ()
| Pmovsd_mf (r1, r2) -> Printf.printf "Pmovsd_mf" ; ()
| Pmovss_fi (r1, r2) -> Printf.printf "Pmovss_fi" ; ()
| Pmovss_fm (r1, r2) -> Printf.printf "Pmovss_fm" ; ()
| Pmovss_mf (r1, r2) -> Printf.printf "Pmovss_mf" ; ()
| Pfldl_m r -> Printf.printf "Pfldl_m" ; ()
| Pfstpl_m r -> Printf.printf "Pfstpl_m" ; ()
| Pflds_m r -> Printf.printf "Pflds_m" ; ()
| Pfstps_m r -> Printf.printf "Pfstps_m" ; ()
| Pmovb_mr (r1, r2) -> Printf.printf "Pmovb_mr" ; ()
| Pmovw_mr (r1, r2) -> Printf.printf "Pmovw_mr" ; ()
| Pmovzb_rr (r1, r2) -> Printf.printf "Pmovzb_rr" ; ()
| Pmovzb_rm (r1, r2) -> Printf.printf "Pmovzb_rm" ; ()
| Pmovsb_rr (r1, r2) -> Printf.printf "Pmovsb_rr" ; ()
| Pmovsb_rm (r1, r2) -> Printf.printf "Pmovsb_rm" ; ()
| Pmovzw_rr (r1, r2) -> Printf.printf "Pmovzw_rr" ; ()
| Pmovzw_rm (r1, r2) -> Printf.printf "Pmovzw_rm" ; ()
| Pmovsw_rr (r1, r2) -> Printf.printf "Pmovsw_rr" ; ()
| Pmovsw_rm (r1, r2) -> Printf.printf "Pmovsw_rm" ; ()
| Pmovzl_rr (r1, r2) -> Printf.printf "Pmovzl_rr" ; ()
| Pmovsl_rr (r1, r2) -> Printf.printf "Pmovsl_rr" ; ()
| Pmovls_rr r -> Printf.printf "Pmovls_rr" ; ()
| Pcvtsd2ss_ff (r1, r2) -> Printf.printf "Pcvtsd2ss_ff" ; ()
| Pcvtss2sd_ff (r1, r2) -> Printf.printf "Pcvtss2sd_ff" ; ()
| Pcvttsd2si_rf (r1, r2) -> Printf.printf "Pcvttsd2si_rf" ; ()
| Pcvtsi2sd_fr (r1, r2) -> Printf.printf "Pcvtsi2sd_fr" ; ()
| Pcvttss2si_rf (r1, r2) -> Printf.printf "Pcvttss2si_rf" ; ()
| Pcvtsi2ss_fr (r1, r2) -> Printf.printf "Pcvtsi2ss_fr" ; ()
| Pcvttsd2sl_rf (r1, r2) -> Printf.printf "Pcvttsd2sl_rf" ; ()
| Pcvtsl2sd_fr (r1, r2) -> Printf.printf "Pcvtsl2sd_fr" ; ()
| Pcvttss2sl_rf (r1, r2) -> Printf.printf "Pcvttss2sl_rf" ; ()
| Pcvtsl2ss_fr (r1, r2) -> Printf.printf "Pcvtsl2ss_fr" ; ()
| Pleal (r1, r2) -> Printf.printf "Pleal" ; ()
| Pleaq (r1, r2) -> Printf.printf "Pleaq" ; ()
| Pnegl r -> Printf.printf "Pnegl" ; ()
| Pnegq r -> Printf.printf "Pnegq" ; ()
| Paddl_ri (r1, r2) -> Printf.printf "Paddl_ri" ; ()
| Paddq_ri (r1, r2) -> Printf.printf "Paddq_ri" ; ()
| Psubl_rr (r1, r2) -> Printf.printf "Psubl_rr" ; ()
| Psubq_rr (r1, r2) -> Printf.printf "Psubq_rr" ; ()
| Pimull_rr (r1, r2) -> Printf.printf "Pimull_rr" ; ()
| Pimulq_rr (r1, r2) -> Printf.printf "Pimulq_rr" ; ()
| Pimull_ri (r1, r2) -> Printf.printf "Pimull_ri" ; ()
| Pimulq_ri (r1, r2) -> Printf.printf "Pimulq_ri" ; ()
| Pimull_r r -> Printf.printf "Pimull_r" ; ()
| Pimulq_r r -> Printf.printf "Pimulq_r" ; ()
| Pmull_r r -> Printf.printf "Pmull_r" ; ()
| Pmulq_r r -> Printf.printf "Pmulq_r" ; ()
| Pcltd -> Printf.printf "Pcltd" ; ()
| Pcqto -> Printf.printf "Pcqto" ; ()
| Pdivl r -> Printf.printf "Pdivl" ; ()
| Pdivq r -> Printf.printf "Pdivq" ; ()
| Pidivl r -> Printf.printf "Pidivl" ; ()
| Pidivq r -> Printf.printf "Pidivq" ; ()
| Pandl_rr (r1, r2) -> Printf.printf "Pandl_rr" ; ()
| Pandq_rr (r1, r2) -> Printf.printf "Pandq_rr" ; ()
| Pandl_ri (r1, r2) -> Printf.printf "Pandl_ri" ; ()
| Pandq_ri (r1, r2) -> Printf.printf "Pandq_ri" ; ()
| Porl_rr (r1, r2) -> Printf.printf "Porl_rr" ; ()
| Porq_rr (r1, r2) -> Printf.printf "Porq_rr" ; ()
| Porl_ri (r1, r2) -> Printf.printf "Porl_ri" ; ()
| Porq_ri (r1, r2) -> Printf.printf "Porq_ri" ; ()
| Pxorl_r r -> Printf.printf "Pxorl_r" ; ()
| Pxorq_r r -> Printf.printf "Pxorq_r" ; ()
| Pxorl_rr (r1, r2) -> Printf.printf "Pxorl_rr" ; ()
| Pxorq_rr (r1, r2) -> Printf.printf "Pxorq_rr" ; ()
| Pxorl_ri (r1, r2) -> Printf.printf "Pxorl_ri" ; ()
| Pxorq_ri (r1, r2) -> Printf.printf "Pxorq_ri" ; ()
| Pnotl r -> Printf.printf "Pnotl" ; ()
| Pnotq r -> Printf.printf "Pnotq" ; ()
| Psall_rcl r -> Printf.printf "Psall_rcl" ; ()
| Psalq_rcl r -> Printf.printf "Psalq_rcl" ; ()
| Psall_ri (r1, r2) -> Printf.printf "Psall_ri" ; ()
| Psalq_ri (r1, r2) -> Printf.printf "Psalq_ri" ; ()
| Pshrl_rcl r -> Printf.printf "Pshrl_rcl" ; ()
| Pshrq_rcl r -> Printf.printf "Pshrq_rcl" ; ()
| Pshrl_ri (r1, r2) -> Printf.printf "Pshrl_ri" ; ()
| Pshrq_ri (r1, r2) -> Printf.printf "Pshrq_ri" ; ()
| Psarl_rcl r -> Printf.printf "Psarl_rcl" ; ()
| Psarq_rcl r -> Printf.printf "Psarq_rcl" ; ()
| Psarl_ri (r1, r2) -> Printf.printf "Psarl_ri" ; ()
| Psarq_ri (r1, r2) -> Printf.printf "Psarq_ri" ; ()
| Pshld_ri (r1, r2, r3) -> Printf.printf "Pshld_ri" ; ()
| Prorl_ri (r1, r2) -> Printf.printf "Prorl_ri" ; ()
| Prorq_ri (r1, r2) -> Printf.printf "Prorq_ri" ; ()
| Pcmpl_rr (r1, r2) -> Printf.printf "Pcmpl_rr" ; ()
| Pcmpq_rr (r1, r2) -> Printf.printf "Pcmpq_rr" ; ()
| Pcmpl_ri (r1, r2) -> Printf.printf "Pcmpl_ri" ; ()
| Pcmpq_ri (r1, r2) -> Printf.printf "Pcmpq_ri" ; ()
| Ptestl_rr (r1, r2) -> Printf.printf "Ptestl_rr" ; ()
| Ptestq_rr (r1, r2) -> Printf.printf "Ptestq_rr" ; ()
| Ptestl_ri (r1, r2) -> Printf.printf "Ptestl_ri" ; ()
| Ptestq_ri (r1, r2) -> Printf.printf "Ptestq_ri" ; ()
| Pcmov (r1, r2, r3) -> Printf.printf "Pcmov" ; ()
| Psetcc (r1, r2) -> Printf.printf "Psetcc" ; ()
| Paddd_ff (r1, r2) -> Printf.printf "Paddd_ff" ; ()
| Psubd_ff (r1, r2) -> Printf.printf "Psubd_ff" ; ()
| Pmuld_ff (r1, r2) -> Printf.printf "Pmuld_ff" ; ()
| Pdivd_ff (r1, r2) -> Printf.printf "Pdivd_ff" ; ()
| Pnegd r -> Printf.printf "Pnegd" ; ()
| Pabsd r -> Printf.printf "Pabsd" ; ()
| Pcomisd_ff (r1, r2) -> Printf.printf "Pcomisd_ff" ; ()
| Pxorpd_f r -> Printf.printf "Pxorpd_f" ; ()
| Padds_ff (r1, r2) -> Printf.printf "Padds_ff" ; ()
| Psubs_ff (r1, r2) -> Printf.printf "Psubs_ff" ; ()
| Pmuls_ff (r1, r2) -> Printf.printf "Pmuls_ff" ; ()
| Pdivs_ff (r1, r2) -> Printf.printf "Pdivs_ff" ; ()
| Pnegs r -> Printf.printf "Pnegs" ; ()
| Pabss r -> Printf.printf "Pabss" ; ()
| Pcomiss_ff (r1, r2) -> Printf.printf "Pcomiss_ff" ; ()
| Pxorps_f r -> Printf.printf "Pxorps_f" ; ()
| Pjmp_l r -> Printf.printf "Pjmp_l" ; ()
| Pjmp_s (r1, r2) -> Printf.printf "Pjmp_s " ; Printf.printf "%d" (P.to_int r1); ()
| Pjmp_r (r1, r2) -> Printf.printf "Pjmp_r" ; ()
| Pjcc (r1, r2) -> Printf.printf "Pjcc" ; 
  let _ = match r1 with
    | Cond_g -> Printf.printf " %s" "G" 
    | Cond_l -> Printf.printf " %s" "L"
    | _ -> Printf.printf " NotG" in
  Printf.printf " %d" (Camlcoq.P.to_int r2); ()
| Pjcc2 (r1, r2, r3) -> Printf.printf "Pjcc2" ; ()
| Pjmptbl (r1, r2) -> Printf.printf "Pjmptbl" ; ()
| Pcall_s (r1, r2) -> Printf.printf "Pcall_s" ; ()
| Pcall_r (r1, r2) -> Printf.printf "Pcall_r" ; ()
| Pret -> Printf.printf "Pret" ; ()
| Pmov_rm_a (r1, r2) -> Printf.printf "Pmov_rm_a" ; ()
| Pmov_mr_a (r1, r2) -> Printf.printf "Pmov_mr_a" ; ()
| Pmovsd_fm_a (r1, r2) -> Printf.printf "Pmovsd_fm_a" ; ()
| Pmovsd_mf_a (r1, r2) -> Printf.printf "Pmovsd_mf_a" ; ()
| Plabel r -> Printf.printf "Plabel" ; Printf.printf " %d" (Camlcoq.P.to_int r); ()
| Pallocframe (sz, ofs_ra, ofs_link) -> 
  let open Camlcoq in
  let open Printf in
  printf "Pallocframe ";
  printf "{sz = %d; " (Z.to_int sz); 
  printf "ofs_ra = %d; " (Z.to_int ofs_ra); 
  printf "ofs_link = %d}" (Z.to_int ofs_link);
  ()
| Pfreeframe (sz, ofs_ra, ofs_link) ->
  let open Camlcoq in
  let open Printf in
  printf "Pfreeframe ";
  printf "{sz = %d; " (Z.to_int sz); 
  printf "ofs_ra = %d; " (Z.to_int ofs_ra); 
  printf "ofs_link = %d}" (Z.to_int ofs_link);
  ()
| Pbuiltin (r1, r2, r3) -> Printf.printf "Pbuiltin" ; ()
| Padcl_ri (r1, r2) -> Printf.printf "Padcl_ri" ; ()
| Padcl_rr (r1, r2) -> Printf.printf "Padcl_rr" ; ()
| Paddl_mi (r1, r2) -> Printf.printf "Paddl_mi" ; ()
| Paddl_rr (r1, r2) -> Printf.printf "Paddl_rr" ; ()
| Pbsfl (r1, r2) -> Printf.printf "Pbsfl" ; ()
| Pbsfq (r1, r2) -> Printf.printf "Pbsfq" ; ()
| Pbsrl (r1, r2) -> Printf.printf "Pbsrl" ; ()
| Pbsrq (r1, r2) -> Printf.printf "Pbsrq" ; ()
| Pbswap64 r -> Printf.printf "Pbswap64" ; ()
| Pbswap32 r -> Printf.printf "Pbswap32" ; ()
| Pbswap16 r -> Printf.printf "Pbswap16" ; ()
| Pcfi_adjust r -> Printf.printf "Pcfi_adjust" ; ()
| Pfmadd132 (r1, r2, r3) -> Printf.printf "Pfmadd132" ; ()
| Pfmadd213 (r1, r2, r3) -> Printf.printf "Pfmadd213" ; ()
| Pfmadd231 (r1, r2, r3) -> Printf.printf "Pfmadd231" ; ()
| Pfmsub132 (r1, r2, r3) -> Printf.printf "Pfmsub132" ; ()
| Pfmsub213 (r1, r2, r3) -> Printf.printf "Pfmsub213" ; ()
| Pfmsub231 (r1, r2, r3) -> Printf.printf "Pfmsub231" ; ()
| Pfnmadd132 (r1, r2, r3) -> Printf.printf "Pfnmadd132" ; ()
| Pfnmadd213 (r1, r2, r3) -> Printf.printf "Pfnmadd213" ; ()
| Pfnmadd231 (r1, r2, r3) -> Printf.printf "Pfnmadd231" ; ()
| Pfnmsub132 (r1, r2, r3) -> Printf.printf "Pfnmsub132" ; ()
| Pfnmsub213 (r1, r2, r3) -> Printf.printf "Pfnmsub213" ; ()
| Pfnmsub231 (r1, r2, r3) -> Printf.printf "Pfnmsub231" ; ()
| Pmaxsd (r1, r2) -> Printf.printf "Pmaxsd" ; ()
| Pminsd (r1, r2) -> Printf.printf "Pminsd" ; ()
| Pmovb_rm (r1, r2) -> Printf.printf "Pmovb_rm" ; ()
| Pmovsq_mr (r1, r2) -> Printf.printf "Pmovsq_mr" ; ()
| Pmovsq_rm (r1, r2) -> Printf.printf "Pmovsq_rm" ; ()
| Pmovsb -> Printf.printf "Pmovsb" ; ()
| Pmovsw -> Printf.printf "Pmovsw" ; ()
| Pmovw_rm (r1, r2) -> Printf.printf "Pmovw_rm" ; ()
| Pnop -> Printf.printf "Pnop" ; ()
| Prep_movsl -> Printf.printf "Prep_movsl" ; ()
| Psbbl_rr (r1, r2) -> Printf.printf "Psbbl_rr" ; ()
| Psqrtsd (r1, r2) -> Printf.printf "Psqrtsd" ; ()
| Psubl_ri (r1, r2) -> Printf.printf "Psubl_ri" ; ()
| Psubq_ri (r1, r2) -> Printf.printf "Psubq_ri" ; ()

(** val do_ef_malloc :
    world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_malloc v m =
  (match do_alloc_size v with
  | Some sz ->
    let (m', b) =
      Mem.alloc m (Z.opp (size_chunk coq_Mptr)) (Ptrofs.unsigned sz)
    in
    (match Mem.store coq_Mptr m' b (Z.opp (size_chunk coq_Mptr)) v with
      | Some m'' -> Some (Vptr (b, Ptrofs.zero), m'')
      | None -> None)
  | None -> None)

(** val do_ef_free :
    world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_free v m =
  (match v with
    | Vptr (b, lo) ->
      (match Mem.load coq_Mptr m b
                (Z.sub (Ptrofs.unsigned lo) (size_chunk coq_Mptr)) with
        | Some vsz ->
          (match do_alloc_size vsz with
          | Some sz ->
            if zlt Z0 (Ptrofs.unsigned sz)
            then (match Mem.free m b
                          (Z.sub (Ptrofs.unsigned lo)
                            (size_chunk coq_Mptr))
                          (Z.add (Ptrofs.unsigned lo)
                            (Ptrofs.unsigned sz)) with
                  | Some m' -> Some (Vundef, m')
                  | None -> None)
            else None
            | _ -> None)
          | None -> None)
      | _ -> None)

(** val do_ef_memcpy :
  coq_Z -> coq_Z -> world -> coq_val list -> Mem.mem ->
  (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_memcpy sz al vargs m =
  match vargs with
  | [] -> None
  | v :: l ->
  (match v with
    | Vptr (bdst, odst) ->
      (match l with
      | [] -> None
      | v0 :: l0 ->
        (match v0 with
          | Vptr (bsrc, osrc) ->
            (match l0 with
            | [] ->
              if (&&)
                    ((||) (coq_Decidable_eq_Z al (Zpos Coq_xH))
                      ((||) (coq_Decidable_eq_Z al (Zpos (Coq_xO Coq_xH)))
                        ((||)
                          (coq_Decidable_eq_Z al (Zpos (Coq_xO (Coq_xO
                            Coq_xH))))
                          (coq_Decidable_eq_Z al (Zpos (Coq_xO (Coq_xO
                            (Coq_xO Coq_xH))))))))
                    ((&&) (coq_Decidable_ge_Z sz Z0)
                      ((&&) (coq_Decidable_divides al sz)
                        ((&&)
                          (if coq_Decidable_gt_Z sz Z0
                          then coq_Decidable_divides al
                                  (Ptrofs.unsigned osrc)
                          else true)
                          ((&&)
                            (if coq_Decidable_gt_Z sz Z0
                            then coq_Decidable_divides al
                                    (Ptrofs.unsigned odst)
                            else true)
                            ((||)
                              (negb (coq_Decidable_eq_positive bsrc bdst))
                              ((||)
                                (coq_Decidable_eq_Z (Ptrofs.unsigned osrc)
                                  (Ptrofs.unsigned odst))
                                ((||)
                                  (coq_Decidable_le_Z
                                    (Z.add (Ptrofs.unsigned osrc) sz)
                                    (Ptrofs.unsigned odst))
                                  (coq_Decidable_le_Z
                                    (Z.add (Ptrofs.unsigned odst) sz)
                                    (Ptrofs.unsigned osrc)))))))))
              then (match Mem.loadbytes m bsrc (Ptrofs.unsigned osrc) sz with
                    | Some bytes ->
                      (match Mem.storebytes m bdst (Ptrofs.unsigned odst)
                                bytes with
                        | Some m' -> Some (Vundef, m')
                        | None -> None)
                    | None -> None)
              else None
            | _ :: _ -> None)
          | _ -> None))
    | _ -> None)
  

(* Name used for version string etc. *)
let tool_name = "C verified compiler"

(* Optional sdump suffix *)
let sdump_suffix = ref ".json"

let nolink () =
  !option_c || !option_S || !option_E || !option_interp

let object_filename sourcename suff =
  if nolink () then
    output_filename ~final: !option_c sourcename suff ".o"
  else
    tmp_file ".o"

(* From CompCert C AST to asm *)

let compile_c_file sourcename ifile ofile =
  (* Prepare to dump Clight, RTL, etc, if requested *)
  let set_dest dst opt ext =
    dst := if !opt then Some (output_filename sourcename ".c" ext)
      else None in
  set_dest Cprint.destination option_dparse ".parsed.c";
  set_dest PrintCsyntax.destination option_dcmedium ".compcert.c";
  set_dest PrintClight.destination option_dclight ".light.c";
  set_dest PrintCminor.destination option_dcminor ".cm";
  set_dest PrintRTL.destination option_drtl ".rtl";
  set_dest Regalloc.destination_alloctrace option_dalloctrace ".alloctrace";
  set_dest PrintLTL.destination option_dltl ".ltl";
  set_dest PrintMach.destination option_dmach ".mach";
  set_dest AsmToJSON.destination option_sdump !sdump_suffix;
  (* Parse the ast *)
  let csyntax = parse_c_file sourcename ifile in
  (* Convert to Asm *)
  let asm =
    match Compiler.apply_partial
               (Compiler.transf_c_program csyntax)
               Asmexpand.expand_program with
    | Errors.OK asm ->
        asm
    | Errors.Error msg ->
      let loc = file_loc sourcename in
        fatal_error loc "%a"  print_error msg in
  (* Dump Asm in binary and JSON format *)
  (* insert code here *)
  (* Optional monad type, match with None/Maybe to extract Mem.mem.mem *)
  
  let test_asm = match Compiler.transf_c_program csyntax with
    | Errors.OK test_asm -> test_asm
    | Errors.Error msg ->
      let loc = file_loc sourcename in
        fatal_error loc "%a"  print_error msg in

  match Genv.init_mem test_asm with
    | None -> ()
    | Some memory ->  
  let ge = Genv.globalenv test_asm in
  (* Pregmap.init initializes the register map *)
  (* extraction/Maps.ml line 384 EMap module *)

  (* let b = Bap_main.init () in *)

  let regset = Pregmap.init Vundef in
  let pc_init = Genv.symbol_address ge test_asm.prog_main Ptrofs.zero in
  let regset = Pregmap.set PC pc_init regset in
  let regset = Pregmap.set RA coq_Vnullptr regset in

  let open Camlcoq in

  (* let publics = List.map (fun s -> P.to_int s) asm.prog_public in
    List.iter (fun z -> Printf.printf "%d\n" z) publics; *)

  (* Printf.printf "%d\n" (P.to_int asm.prog_main); *)

  (*initial_state p (State rs0 m0).*)
  let rec step (ge : (Asm.fundef, unit) Globalenvs.Genv.t) (asm : Asm.program) 
    (rs : (Asm.PregEq.t -> Values.coq_val)) (m : Memory.Mem.mem) =
  match Pregmap.get PC rs with
    | Vptr(b, ofs) ->

    let offset = Integers.Ptrofs.intval ofs in
    
    Printf.printf "%d" (P.to_int b);
    Printf.printf " ";
    Printf.printf "%d" (Z.to_int offset);
    Printf.printf " ";
    let c_f = Genv.find_funct_ptr ge b in (* Some (Internal f) *)
    begin
    match c_f with
      | Some (Internal f) ->
        let inst = find_instr (Ptrofs.unsigned ofs) f.fn_code in
        begin
          match inst with
          | Some i -> 
            print_instruction i;
            Printf.printf "\n";
            let o = exec_instr ge f i rs m in
            begin
            match o with
            | Next (r1, m1) -> step ge asm r1 m1
            | Stuck -> Printf.printf "Execution of \""; print_instruction i; Printf.printf "\" instruction invalid!\n"; None
            end
          | _ -> Printf.printf "no inst\n"; None
        end
      | Some (External f) -> 
        begin
          match f with
          | EF_malloc -> 
            Printf.printf "Malloc\n";
            let v = Pregmap.get (IR RDI) rs in
            let m'' = do_ef_malloc v m in
            begin
            match m'' with
                | Some (res, m'') -> 
                  let rs' = (set_pair (loc_external_result (ef_sig f)) res (undef_caller_save_regs rs)) in
                  let rs' = Pregmap.set PC (Pregmap.get RA rs) rs' in
                  step ge asm rs' m''
                | None -> None
            end
          | EF_free -> 
            Printf.printf "Free\n";
            let v = Pregmap.get (IR RDI) rs in
            let m'' = do_ef_free v m in
            begin
            match m'' with
                | Some (res, m'') -> 
                  let rs' = (set_pair (loc_external_result (ef_sig f)) res (undef_caller_save_regs rs)) in
                  let rs' = Pregmap.set PC (Pregmap.get RA rs) rs' in
                  step ge asm rs' m''
                | None -> None
            end
          | EF_memcpy (sz, al) ->
            Printf.printf "memcpy\n";
            let v = Pregmap.get (IR RDI) rs in
            let m'' = do_ef_memcpy sz al [v] m in
            begin
            match m'' with
                | Some (res, m'') -> 
                  let rs' = (set_pair (loc_external_result (ef_sig f)) res (undef_caller_save_regs rs)) in
                  let rs' = Pregmap.set PC (Pregmap.get RA rs) rs' in
                  step ge asm rs' m''
                | None -> None
            end
          | _ -> Printf.printf "Not defined!\n"; None
        end
      | _ -> Printf.printf "Who knows :o\n"; None
    end
    | _ -> Some rs (* final state *)
  in

  Printf.printf "-----------------------\nStepping through Asm...\n-----------------------\n";
  let result = step ge test_asm regset memory in
  let z = match result with
  | Some a -> 
    let q = Pregmap.get (IR RAX) a in
    begin
    match q with
      | Vundef -> Printf.printf "Vundef";None
      | Vint a -> 
        begin 
        match a with 
        | Zpos a -> 
          begin match a with
            | Coq_xI a -> Printf.printf "xI\n"; ()
            | Coq_xO a -> Printf.printf "xO\n"; ()
            | Coq_xH -> Printf.printf "xH\n"; ()
          end;
          () 
        | Z0 -> Printf.printf "Z0\n"; ()
        | Zneg a -> Printf.printf "Zneg\n"; () 
        end;
        None
      | Vlong a-> Printf.printf "Vlong\n";None
      | Vfloat a-> Printf.printf "Vfloat\n";None
      | Vsingle a-> Printf.printf "Vsingle\n";None
      | Vptr (a,b)-> Printf.printf "Vptr\n";None
    end
  | None -> None in

  (* AsmToJSON.print_if test_asm sourcename;
  (* Print Asm in text form *)
  let oc = open_out "asm_out.s" in
  PrintAsm.print_program oc test_asm;
  close_out oc; *)

  AsmToJSON.print_if asm sourcename;
  (* Print Asm in text form *)
  let oc = open_out ofile in
  PrintAsm.print_program oc asm;
  close_out oc
(* From C source to asm *)

let compile_i_file sourcename preproname =
  if !option_interp then begin
    Machine.config := Machine.compcert_interpreter !Machine.config;
    let csyntax = parse_c_file sourcename preproname in
    Interp.execute csyntax;
        ""
  end else if !option_S then begin
    compile_c_file sourcename preproname
      (output_filename ~final:true sourcename ".c" ".s");
    ""
  end else begin
    let asmname =
      if !option_dasm
      then output_filename sourcename ".c" ".s"
      else tmp_file ".s" in
    compile_c_file sourcename preproname asmname;
    let objname = object_filename sourcename ".c" in
    assemble asmname objname;
    objname
  end

(* Processing of a .c file *)

let process_c_file sourcename =
  ensure_inputfile_exists sourcename;
  if !option_E then begin
    preprocess sourcename (output_filename_default "-");
    ""
  end else begin
    let preproname = if !option_dprepro then
      output_filename sourcename ".c" ".i"
    else
      tmp_file ".i" in
    preprocess sourcename preproname;
    compile_i_file sourcename preproname
  end

(* Processing of a .i / .p file (preprocessed C) *)

let process_i_file sourcename =
  ensure_inputfile_exists sourcename;
  compile_i_file sourcename sourcename

(* Processing of .S and .s files *)

let process_s_file sourcename =
  ensure_inputfile_exists sourcename;
  let objname = object_filename sourcename ".s" in
  assemble sourcename objname;
  objname

let process_S_file sourcename =
  ensure_inputfile_exists sourcename;
  if !option_E then begin
    preprocess sourcename (output_filename_default "-");
    ""
  end else begin
    let preproname = tmp_file ".s" in
    preprocess sourcename preproname;
    let objname = object_filename sourcename ".S" in
    assemble preproname objname;
    objname
  end

(* Processing of .h files *)

let process_h_file sourcename =
  if !option_E then begin
    ensure_inputfile_exists sourcename;
    preprocess sourcename (output_filename_default "-");
    ""
  end else
    fatal_error no_loc "input file %s ignored (not in -E mode)\n" sourcename

let target_help =
  if Configuration.arch = "arm" && Configuration.model <> "armv6" then
{|Target processor options:
  -mthumb        Use Thumb2 instruction encoding
  -marm          Use classic ARM instruction encoding
|}
else
  ""

let toolchain_help =
  if not Configuration.gnu_toolchain then begin
{|Toolchain options:
  -t tof:env     Select target processor for the diab toolchain
|} end else
    ""

let usage_string =
  version_string tool_name ^
  {|Usage: ccomp [options] <source files>
Recognized source files:
  .c             C source file
  .i or .p       C source file that should not be preprocessed
  .s             Assembly file
  .S or .sx      Assembly file that must be preprocessed
  .o             Object file
  .a             Library file
Processing options:
  -c             Compile to object file only (no linking), result in <file>.o
  -E             Preprocess only, send result to standard output
  -S             Compile to assembler only, save result in <file>.s
  -o <file>      Generate output in <file>
|} ^
  prepro_help ^
  language_support_help ^
 DebugInit.debugging_help ^
{|Optimization options: (use -fno-<opt> to turn off -f<opt>)
  -O             Optimize the compiled code [on by default]
  -O0            Do not optimize the compiled code
  -O1 -O2 -O3    Synonymous for -O
  -Os            Optimize for code size in preference to code speed
  -ftailcalls    Optimize function calls in tail position [on]
  -fconst-prop   Perform global constant propagation  [on]
  -ffloat-const-prop <n>  Control constant propagation of floats
                   (<n>=0: none, <n>=1: limited, <n>=2: full; default is full)
  -fcse          Perform common subexpression elimination [on]
  -fredundancy   Perform redundancy elimination [on]
  -finline       Perform inlining of functions [on]
  -finline-functions-called-once Integrate functions only required by their
                 single caller [on]
Code generation options: (use -fno-<opt> to turn off -f<opt>)
  -ffpu          Use FP registers for some integer operations [on]
  -fsmall-data <n>  Set maximal size <n> for allocation in small data area
  -fsmall-const <n>  Set maximal size <n> for allocation in small constant area
  -falign-functions <n>  Set alignment (in bytes) of function entry points
  -falign-branch-targets <n>  Set alignment (in bytes) of branch targets
  -falign-cond-branches <n>  Set alignment (in bytes) of conditional branches
|} ^
 target_help ^
 toolchain_help ^
 assembler_help ^
 linker_help ^
{|Tracing options:
  -dprepro       Save C file after preprocessing in <file>.i
  -dparse        Save C file after parsing and elaboration in <file>.parsed.c
  -dc            Save generated Compcert C in <file>.compcert.c
  -dclight       Save generated Clight in <file>.light.c
  -dcminor       Save generated Cminor in <file>.cm
  -drtl          Save RTL at various optimization points in <file>.rtl.<n>
  -dltl          Save LTL after register allocation in <file>.ltl
  -dmach         Save generated Mach code in <file>.mach
  -dasm          Save generated assembly in <file>.s
  -dall          Save all generated intermediate files in <file>.<ext>
  -sdump         Save info for post-linking validation in <file>.json
|} ^
  general_help ^
  warning_help ^
  {|Interpreter mode:
  -interp        Execute given .c files using the reference interpreter
  -quiet         Suppress diagnostic messages for the interpreter
  -trace         Have the interpreter produce a detailed trace of reductions
  -random        Randomize execution order
  -all           Simulate all possible execution orders
  -link <file>   Link compiled object file
|}

let print_usage_and_exit () =
  printf "%s" usage_string; exit 0

let enforce_buildnr nr =
  let build = int_of_string Version.buildnr in
  if nr != build then
    fatal_error no_loc "Mismatching builds: This is CompCert build %d, but QSK requires build %d.\n\
Please use matching builds of QSK and CompCert." build nr

let dump_mnemonics destfile =
  let oc = open_out_bin destfile in
  let pp = Format.formatter_of_out_channel oc in
  AsmToJSON.pp_mnemonics pp;
  Format.pp_print_flush pp ();
  close_out oc;
  exit 0

let optimization_options = [
  option_ftailcalls; option_fconstprop; option_fcse; option_fredundancy; option_finline_functions_called_once;
]

let set_all opts () = List.iter (fun r -> r := true) opts
let unset_all opts () = List.iter (fun r -> r := false) opts

let num_source_files = ref 0

let num_input_files = ref 0

let cmdline_actions =
  let f_opt name ref =
    [Exact("-f" ^ name), Set ref; Exact("-fno-" ^ name), Unset ref] in
  [
(* Getting help *)
  Exact "-help", Unit print_usage_and_exit;
  Exact "--help", Unit print_usage_and_exit;]
(* Getting version info *)
  @ version_options tool_name @
(* Enforcing CompCert build numbers for QSKs and mnemonics dump *)
  (if Version.buildnr <> "" then
    [ Exact "-qsk-enforce-build", Integer enforce_buildnr;
      Exact "--qsk-enforce-build", Integer enforce_buildnr;
      Exact "-dump-mnemonics", String  dump_mnemonics;
    ]
   else []) @
(* Processing options *)
 [ Exact "-c", Set option_c;
  Exact "-E", Set option_E;
  Exact "-S", Set option_S;
  Exact "-o", String(fun s -> option_o := Some s);
  Prefix "-o", Self (fun s -> let s = String.sub s 2 ((String.length s) - 2) in
                              option_o := Some s);]
  (* Preprocessing options *)
    @ prepro_actions @
  (* Language support options *)
    language_support_options
  (* Debugging options *)
    @ DebugInit.debugging_actions @
(* Code generation options -- more below *)
 [
  Exact "-O0", Unit (unset_all optimization_options);
  Exact "-O", Unit (set_all optimization_options);
  _Regexp "-O[123]$", Unit (set_all optimization_options);
  Exact "-Os", Set option_Osize;
  Exact "-fsmall-data", Integer(fun n -> option_small_data := n);
  Exact "-fsmall-const", Integer(fun n -> option_small_const := n);
  Exact "-ffloat-const-prop", Integer(fun n -> option_ffloatconstprop := n);
  Exact "-falign-functions", Integer(fun n -> option_falignfunctions := Some n);
  Exact "-falign-branch-targets", Integer(fun n -> option_falignbranchtargets := n);
  Exact "-falign-cond-branches", Integer(fun n -> option_faligncondbranchs := n);] @
(* Target processor options *)
  (if Configuration.arch = "arm" then
    if Configuration.model = "armv6" then
      [ Exact "-marm", Ignore ] (* Thumb needs ARMv6T2 or ARMv7 *)
    else
      [ Exact "-mthumb", Set option_mthumb;
        Exact "-marm", Unset option_mthumb; ]
   else []) @
(* Toolchain options *)
    (if not Configuration.gnu_toolchain then
       [Exact "-t", String (fun arg -> push_linker_arg "-t"; push_linker_arg arg;
                             prepro_options := arg :: "-t" :: !prepro_options;
                             assembler_options := arg :: "-t" :: !assembler_options;)]
     else
       []) @
(* Assembling options *)
  assembler_actions @
(* Linking options *)
  linker_actions @
(* Tracing options *)
 [ Exact "-dprepro", Set option_dprepro;
  Exact "-dparse", Set option_dparse;
  Exact "-dc", Set option_dcmedium;
  Exact "-dclight", Set option_dclight;
  Exact "-dcminor", Set option_dcminor;
  Exact "-drtl", Set option_drtl;
  Exact "-dltl", Set option_dltl;
  Exact "-dalloctrace", Set option_dalloctrace;
  Exact "-dmach", Set option_dmach;
  Exact "-dasm", Set option_dasm;
  Exact "-dall", Self (fun _ ->
    option_dprepro := true;
    option_dparse := true;
    option_dcmedium := true;
    option_dclight := true;
    option_dcminor := true;
    option_drtl := true;
    option_dltl := true;
    option_dalloctrace := true;
    option_dmach := true;
    option_dasm := true);
  Exact "-sdump", Set option_sdump;
  Exact "-sdump-suffix", String (fun s -> option_sdump := true; sdump_suffix:= s);
  Exact "-sdump-folder", String (fun s -> AsmToJSON.sdump_folder := s);] @
(* General options *)
   general_options @
(* Diagnostic options *)
  warning_options @
(* Interpreter mode *)
 [ Exact "-interp", Set option_interp;
  Exact "-quiet", Unit (fun () -> Interp.trace := 0);
  Exact "-trace", Unit (fun () -> Interp.trace := 2);
  Exact "-random", Unit (fun () -> Interp.mode := Interp.Random);
  Exact "-all", Unit (fun () -> Interp.mode := Interp.All);
  Exact "-link", String(fun s -> Printf.printf "%s <- file\n" s; Interp.link := s)
 ]
(* Optimization options *)
(* -f options: come in -f and -fno- variants *)
  @ f_opt "tailcalls" option_ftailcalls
  @ f_opt "const-prop" option_fconstprop
  @ f_opt "cse" option_fcse
  @ f_opt "redundancy" option_fredundancy
  @ f_opt "inline" option_finline
  @ f_opt "inline-functions-called-once" option_finline_functions_called_once
(* Code generation options *)
  @ f_opt "fpu" option_ffpu
  @ f_opt "sse" option_ffpu (* backward compatibility *)
  @ [
(* Catch options that are not handled *)
  Prefix "-", Self (fun s ->
      fatal_error no_loc "Unknown option `%s'" s);
(* File arguments *)
  Suffix ".c", Self (fun s ->
      push_action process_c_file s; incr num_source_files; incr num_input_files);
  Suffix ".i", Self (fun s ->
      push_action process_i_file s; incr num_source_files; incr num_input_files);
  Suffix ".p", Self (fun s ->
      push_action process_i_file s; incr num_source_files; incr num_input_files);
  Suffix ".s", Self (fun s ->
      push_action process_s_file s; incr num_source_files; incr num_input_files);
  Suffix ".S", Self (fun s ->
      push_action process_S_file s; incr num_source_files; incr num_input_files);
  Suffix ".sx", Self (fun s ->
      push_action process_S_file s; incr num_source_files; incr num_input_files);
  Suffix ".o", Self (fun s -> push_linker_arg s; incr num_input_files);
  Suffix ".a", Self (fun s -> push_linker_arg s; incr num_input_files);
  (* GCC compatibility: .o.ext files and .so files are also object files *)
  _Regexp ".*\\.o\\.", Self (fun s -> push_linker_arg s; incr num_input_files);
  Suffix ".so", Self (fun s -> push_linker_arg s; incr num_input_files);
  (* GCC compatibility: .h files can be preprocessed with -E *)
  Suffix ".h", Self (fun s ->
      push_action process_h_file s; incr num_source_files; incr num_input_files);
  ]

let _ =
  try
    Gc.set { (Gc.get()) with
                Gc.minor_heap_size = 524288; (* 512k *)
                Gc.major_heap_increment = 4194304 (* 4M *)
           };
    Printexc.record_backtrace true;
    Frontend.init ();
    parse_cmdline cmdline_actions;
    DebugInit.init (); (* Initialize the debug functions *)
    if nolink () && !option_o <> None && !num_source_files >= 2 then
      fatal_error no_loc "ambiguous '-o' option (multiple source files)";
    if !num_input_files = 0 then
      fatal_error no_loc "no input file";
    let linker_args = time "Total compilation time" perform_actions () in
    if not (nolink ()) && linker_args <> [] then begin
      linker (output_filename_default "a.out") linker_args
    end;
    check_errors ()
  with
  | Sys_error msg
  | CmdError msg -> error no_loc "%s" msg; exit 2
  | Abort -> error_summary (); exit 2
  | e -> crash e
