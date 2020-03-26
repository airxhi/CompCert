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

(* Interpreting CompCert C sources *)

open Format
open Camlcoq
open AST
open Integers
open Values
open Memory
open Globalenvs
open Events
open Ctypes
open Csyntax
open Csem

let print_instruction i = 
  let open Asm in
  match i with
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
| Pjmp_s (r1, r2) -> Printf.printf "Pjmp_s" ; ()
| Pjmp_r (r1, r2) -> Printf.printf "Pjmp_r" ; ()
| Pjcc (r1, r2) -> Printf.printf "Pjcc" ; ()
| Pjcc2 (r1, r2, r3) -> Printf.printf "Pjcc2" ; ()
| Pjmptbl (r1, r2) -> Printf.printf "Pjmptbl" ; ()
| Pcall_s (r1, r2) -> Printf.printf "Pcall_s" ; ()
| Pcall_r (r1, r2) -> Printf.printf "Pcall_r" ; ()
| Pret -> Printf.printf "Pret" ; ()
| Pmov_rm_a (r1, r2) -> Printf.printf "Pmov_rm_a" ; ()
| Pmov_mr_a (r1, r2) -> Printf.printf "Pmov_mr_a" ; ()
| Pmovsd_fm_a (r1, r2) -> Printf.printf "Pmovsd_fm_a" ; ()
| Pmovsd_mf_a (r1, r2) -> Printf.printf "Pmovsd_mf_a" ; ()
| Plabel r -> Printf.printf "Plabel" ; ()
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

(* open Regular.Std
open Bap.Std
open Bap_plugins.Std
open Bap_core_theory
open Bap_main *)
(* Configuration *)

let trace = ref 1   (* 0 if quiet, 1 if normally verbose, 2 if full trace *)

type mode = First | Random | All

let mode = ref First

(* let test_code : ((string * Asm.code) list ref) = ref [] *)
let test_code = ref []

(* file to link *)
type link = A | B
let link = ref B

(* Printing events *)

let print_id_ofs p (id, ofs) =
  let id = extern_atom id and ofs = camlint_of_coqint ofs in
  if ofs = 0l
  then fprintf p " %s" id
  else fprintf p " %s%+ld" id ofs

let print_eventval p = function
  | EVint n -> fprintf p "%ld" (camlint_of_coqint n)
  | EVfloat f -> fprintf p "%.15F" (camlfloat_of_coqfloat f)
  | EVsingle f -> fprintf p "%.15F" (camlfloat_of_coqfloat32 f)
  | EVlong n -> fprintf p "%LdLL" (camlint64_of_coqint n)
  | EVptr_global(id, ofs) -> fprintf p "&%a" print_id_ofs (id, ofs)

let print_eventval_list p = function
  | [] -> ()
  | v1 :: vl ->
      print_eventval p v1;
      List.iter (fun v -> fprintf p ",@ %a" print_eventval v) vl

let print_event p = function
  | Event_syscall(id, args, res) ->
      fprintf p "extcall %s(%a) -> %a"
                (camlstring_of_coqstring id)
                print_eventval_list args
                print_eventval res
  | Event_vload(chunk, id, ofs, res) ->
      fprintf p "volatile load %s[&%s%+ld] -> %a"
                (PrintAST.name_of_chunk chunk)
                (extern_atom id) (camlint_of_coqint ofs)
                print_eventval res
  | Event_vstore(chunk, id, ofs, arg) ->
      fprintf p "volatile store %s[&%s%+ld] <- %a"
                (PrintAST.name_of_chunk chunk)
                (extern_atom id) (camlint_of_coqint ofs)
                print_eventval arg
  | Event_annot(text, args) ->
      fprintf p "annotation \"%s\" %a"
                (camlstring_of_coqstring text)
                print_eventval_list args

(* Printing states *)

let name_of_fundef prog fd =
  let rec find_name = function
  | [] -> "<unknown function>"
  | (id, Gfun fd') :: rem ->
      if fd == fd' then extern_atom id else find_name rem
  | (id, Gvar v) :: rem ->
      find_name rem
  in find_name prog.Ctypes.prog_defs

let name_of_function prog fn =
  let rec find_name = function
  | [] -> "<unknown function>"
  | (id, Gfun(Ctypes.Internal fn')) :: rem ->
      if fn == fn' then extern_atom id else find_name rem
  | (id, _) :: rem ->
      find_name rem
  in find_name prog.Ctypes.prog_defs

let invert_local_variable e b =
  Maps.PTree.fold
    (fun res id (b', _) -> if b = b' then Some id else res)
    e None

let print_pointer ge e p (b, ofs) =
  match invert_local_variable e b with
  | Some id -> print_id_ofs p (id, ofs)
  | None ->
      match Genv.invert_symbol ge b with
      | Some id -> print_id_ofs p (id, ofs)
      | None -> ()

let print_val = PrintCsyntax.print_value

let print_val_list p vl =
  match vl with
  | [] -> ()
  | v1 :: vl ->
      print_val p v1;
      List.iter (fun v -> fprintf p ",@ %a" print_val v) vl

let print_state p (prog, ge, s) =
  match s with
  | State(f, s, k, e, m) ->
      PrintCsyntax.print_pointer_hook := print_pointer ge.genv_genv e;
      fprintf p "in function %s, statement@ @[<hv 0>%a@]"
              (name_of_function prog f)
              PrintCsyntax.print_stmt s
  | ExprState(f, r, k, e, m) ->
      PrintCsyntax.print_pointer_hook := print_pointer ge.genv_genv e;
      fprintf p "in function %s, expression@ @[<hv 0>%a@]"
              (name_of_function prog f)
              PrintCsyntax.print_expr r
  | Callstate(fd, args, k, m) ->
      PrintCsyntax.print_pointer_hook := print_pointer ge.genv_genv Maps.PTree.empty;
      fprintf p "calling@ @[<hov 2>%s(%a)@]"
              (name_of_fundef prog fd)
              print_val_list args
  | Returnstate(res, k, m) ->
      PrintCsyntax.print_pointer_hook := print_pointer ge.genv_genv Maps.PTree.empty;
      fprintf p "returning@ %a"
              print_val res
  | Stuckstate ->
      fprintf p "stuck after an undefined expression"

(* Comparing memory states *)

let compare_mem m1 m2 =
  (* assumes nextblocks were already compared equal *)
  (* should permissions be taken into account? *)
  Pervasives.compare m1.Mem.mem_contents m2.Mem.mem_contents

(* Comparing continuations *)

let some_expr = Eval(Vint Int.zero, Tvoid)

let rank_cont = function
  | Kstop -> 0
  | Kdo _ -> 1
  | Kseq _ -> 2
  | Kifthenelse _ -> 3
  | Kwhile1 _ -> 4
  | Kwhile2 _ -> 5
  | Kdowhile1 _ -> 6
  | Kdowhile2 _ -> 7
  | Kfor2 _ -> 8
  | Kfor3 _ -> 9
  | Kfor4 _ -> 10
  | Kswitch1 _ -> 11
  | Kswitch2 _ -> 12
  | Kreturn _ -> 13
  | Kcall _ -> 14

let rec compare_cont k1 k2 =
  if k1 == k2 then 0 else
  match k1, k2 with
  | Kstop, Kstop -> 0
  | Kdo k1, Kdo k2 -> compare_cont k1 k2
  | Kseq(s1, k1), Kseq(s2, k2) ->
      let c = compare s1 s2 in if c <> 0 then c else compare_cont k1 k2
  | Kifthenelse(s1, s1', k1), Kifthenelse(s2, s2', k2) ->
      let c = compare (s1,s1') (s2,s2') in
      if c <> 0 then c else compare_cont k1 k2
  | Kwhile1(e1, s1, k1), Kwhile1(e2, s2, k2) ->
      let c = compare (e1,s1) (e2,s2) in
      if c <> 0 then c else compare_cont k1 k2
  | Kwhile2(e1, s1, k1), Kwhile2(e2, s2, k2) ->
      let c = compare (e1,s1) (e2,s2) in
      if c <> 0 then c else compare_cont k1 k2
  | Kdowhile1(e1, s1, k1), Kdowhile1(e2, s2, k2) ->
      let c = compare (e1,s1) (e2,s2) in
      if c <> 0 then c else compare_cont k1 k2
  | Kdowhile2(e1, s1, k1), Kdowhile2(e2, s2, k2) ->
      let c = compare (e1,s1) (e2,s2) in
      if c <> 0 then c else compare_cont k1 k2
  | Kfor2(e1, s1, s1', k1), Kfor2(e2, s2, s2', k2) ->
      let c = compare (e1,s1,s1') (e2,s2,s2') in
      if c <> 0 then c else compare_cont k1 k2
  | Kfor3(e1, s1, s1', k1), Kfor3(e2, s2, s2', k2) ->
      let c = compare (e1,s1,s1') (e2,s2,s2') in
      if c <> 0 then c else compare_cont k1 k2
  | Kfor4(e1, s1, s1', k1), Kfor4(e2, s2, s2', k2) ->
      let c = compare (e1,s1,s1') (e2,s2,s2') in
      if c <> 0 then c else compare_cont k1 k2
  | Kswitch1(sl1, k1), Kswitch1(sl2, k2) ->
      let c = compare sl1 sl2 in
      if c <> 0 then c else compare_cont k1 k2
  | Kreturn k1, Kreturn k2 ->
      compare_cont k1 k2
  | Kcall(f1, e1, c1, ty1, k1), Kcall(f2, e2, c2, ty2, k2) ->
      let c = compare (f1, e1, c1 some_expr, ty1) (f2, e2, c2 some_expr, ty2) in
      if c <> 0 then c else compare_cont k1 k2
  | _, _ ->
      compare (rank_cont k1) (rank_cont k2)

(* Comparing states *)

let rank_state = function
  | State _ -> 0
  | ExprState _ -> 1
  | Callstate _ -> 2
  | Returnstate _ -> 3
  | Stuckstate -> assert false

let mem_state = function
  | State(f, s, k, e, m) -> m
  | ExprState(f, r, k, e, m) -> m
  | Callstate(fd, args, k, m) -> m
  | Returnstate(res, k, m) -> m
  | Stuckstate -> assert false

let compare_state s1 s2 =
  if s1 == s2 then 0 else
  let c = P.compare (mem_state s1).Mem.nextblock (mem_state s2).Mem.nextblock in
  if c <> 0 then c else begin
  match s1, s2 with
  | State(f1,s1,k1,e1,m1), State(f2,s2,k2,e2,m2) ->
      let c = compare (f1,s1,e1) (f2,s2,e2) in if c <> 0 then c else
      let c = compare_cont k1 k2 in if c <> 0 then c else
      compare_mem m1 m2
  | ExprState(f1,r1,k1,e1,m1), ExprState(f2,r2,k2,e2,m2) ->
      let c = compare (f1,r1,e1) (f2,r2,e2) in if c <> 0 then c else
      let c = compare_cont k1 k2 in if c <> 0 then c else
      compare_mem m1 m2
  | Callstate(fd1,args1,k1,m1), Callstate(fd2,args2,k2,m2) ->
      let c = compare (fd1,args1) (fd2,args2) in if c <> 0 then c else
      let c = compare_cont k1 k2 in if c <> 0 then c else
      compare_mem m1 m2
  | Returnstate(res1,k1,m1), Returnstate(res2,k2,m2) ->
      let c = compare res1 res2 in if c <> 0 then c else
      let c = compare_cont k1 k2 in if c <> 0 then c else
      compare_mem m1 m2
  | _, _ ->
      compare (rank_state s1) (rank_state s2)
  end

(* Maps of states already explored. *)

module StateMap =
  Map.Make(struct
             type t = state
             let compare = compare_state
           end)

(* Extract a string from a global pointer *)

let extract_string m blk ofs =
  let b = Buffer.create 80 in
  let rec extract blk ofs =
    match Mem.load Mint8unsigned m blk ofs with
    | Some(Vint n) ->
        let c = Char.chr (Z.to_int n) in
        if c = '\000' then begin
          Some(Buffer.contents b)
        end else begin
          Buffer.add_char b c;
          extract blk (Z.succ ofs)
        end
    | _ ->
        None in
  extract blk ofs

(* Emulation of printf *)

(* All ISO C 99 formats *)

let re_conversion = Str.regexp (
  "\\(%[-+0# ]*[0-9]*\\(\\.[0-9]*\\)?\\)" (* group 1: flags, width, precision *)
^ "\\(\\|[lhjztL]\\|hh\\|ll\\)"            (* group 3: length modifier *)
^ "\\([aAcdeEfgGinopsuxX%]\\)"            (* group 4: conversion specifier *)
)

external format_float: string -> float -> string
  = "caml_format_float"
external format_int32: string -> int32 -> string
  = "caml_int32_format"
external format_int64: string -> int64 -> string
  = "caml_int64_format"

let format_value m flags length conv arg =
  match conv.[0], length, arg with
  | ('d'|'i'|'u'|'o'|'x'|'X'|'c'), (""|"h"|"hh"|"l"|"z"|"t"), Vint i ->
      format_int32 (flags ^ conv) (camlint_of_coqint i)
  | ('d'|'i'|'u'|'o'|'x'|'X'|'c'), (""|"h"|"hh"|"l"|"z"|"t"), _ ->
      "<int argument expected>"
  | ('d'|'i'|'u'|'o'|'x'|'X'), ("ll"|"j"), Vlong i ->
      format_int64 (flags ^ conv) (camlint64_of_coqint i)
  | ('d'|'i'|'u'|'o'|'x'|'X'), ("ll"|"j"), _ ->
      "<long long argument expected"
  | ('f'|'e'|'E'|'g'|'G'|'a'), (""|"l"), Vfloat f ->
      format_float (flags ^ conv) (camlfloat_of_coqfloat f)
  | ('f'|'e'|'E'|'g'|'G'|'a'), "", _ ->
      "<float argument expected"
  | 's', "", Vptr(blk, ofs) ->
      begin match extract_string m blk ofs with
      | Some s -> s
      | None -> "<bad string>"
      end
  | 's', "", _ ->
      "<pointer argument expected>"
  | 'p', "", Vptr(blk, ofs) ->
      Printf.sprintf "<%ld%+ld>" (P.to_int32 blk) (camlint_of_coqint ofs)
  | 'p', "", Vint i ->
      format_int32 (flags ^ "x") (camlint_of_coqint i)
  | 'p', "", _ ->
      "<int or pointer argument expected>"
  | _, _, _ ->
      "<unrecognized format>"

let do_printf m fmt args =

  let b = Buffer.create 80 in
  let len = String.length fmt in

  let opt_search_forward pos =
    try Some(Str.search_forward re_conversion fmt pos)
    with Not_found -> None in

  let rec scan pos args =
    if pos < len then begin
    match opt_search_forward pos with
    | None ->
        Buffer.add_substring b fmt pos (len - pos)
    | Some pos1 ->
        Buffer.add_substring b fmt pos (pos1 - pos);
        let flags = Str.matched_group 1 fmt
        and length = Str.matched_group 3 fmt
        and conv = Str.matched_group 4 fmt
        and pos' = Str.match_end() in
        if conv = "%" then begin
          Buffer.add_char b '%';
          scan pos' args
        end else begin
          match args with
          | [] ->
              Buffer.add_string b "<missing argument>";
              scan pos' []
          | arg :: args' ->
              Buffer.add_string b (format_value m flags length conv arg);
              scan pos' args'
        end
    end
  in scan 0 args; Buffer.contents b

(* Implementation of external functions *)

let (>>=) opt f = match opt with None -> None | Some arg -> f arg

(* Like eventval_of_val, but accepts static globals as well *)

let convert_external_arg ge v t =
  match v with
  | Vint i -> Some (EVint i)
  | Vfloat f -> Some (EVfloat f)
  | Vsingle f -> Some (EVsingle f)
  | Vlong n -> Some (EVlong n)
  | Vptr(b, ofs) ->
      Senv.invert_symbol ge b >>= fun id -> Some (EVptr_global(id, ofs))
  | _ -> None


let rec convert_external_args ge vl tl =
  match vl, tl with
  | [], [] -> Some []
  | v1::vl, t1::tl ->
      convert_external_arg ge v1 t1 >>= fun e1 ->
      convert_external_args ge vl tl >>= fun el -> Some (e1 :: el)
  | _, _ -> None

  let rec step_insns (ge : (Asm.fundef, unit) Globalenvs.Genv.t) (asm : Asm.program) 
  (rs : (Asm.PregEq.t -> Values.coq_val)) (m : Memory.Mem.mem) =
match Asm.Pregmap.get PC rs with
  | Vptr(b, ofs) -> 
  
  let c_f = Genv.find_funct_ptr ge b in (* Some (Internal f) *)
  begin
  match c_f with
    | Some (Internal f) ->
      let inst = Asm.find_instr (Ptrofs.unsigned ofs) f.fn_code in
      begin
        match inst with
        | Some i -> 
          print_instruction i;
          Printf.printf "\n";
          let o = Asm.exec_instr ge f i rs m in
          
          begin
          match o with
          | Next (r1, m1) -> step_insns ge asm r1 m1
          | Stuck -> Printf.printf "Execution of \""; print_instruction i; Printf.printf "\" instruction invalid!\n"; None
          end
        | _ -> Printf.printf "no inst\n"; None
      end
    | Some (External f) -> 
      begin
        match f with
        | _ -> Printf.printf "Not defined!\n"; None
      end
    | _ -> Printf.printf "Who knows :o\n"; None
  end
  | _ -> Some rs (* final state *)

let print_type (c_val : Values.coq_val) =
  let open Printf in
  match c_val with
  | Vptr _    -> printf "Vptr, "
  | Vundef    -> printf "Undef"
  | Vint _    -> printf "Vint"
  | Vlong _   -> printf "Vlong"
  | Vfloat _  -> printf "Vfloat"
  | Vsingle _ -> printf "Vsingle"



let do_external_function id sg ge w args m =
  match camlstring_of_coqstring id, args with
  | "printf", Vptr(b, ofs) :: args' ->
      extract_string m b ofs >>= fun fmt ->
      let fmt' = do_printf m fmt args' in
      let len = coqint_of_camlint (Int32.of_int (String.length fmt')) in
      Format.print_string fmt';
      flush stdout;
      convert_external_args ge args sg.sig_args >>= fun eargs ->
      Some(((w, [Event_syscall(id, eargs, EVint len)]), Vint len), m)
  | str, _ -> 
    List.iter print_type args;
    let (x, code) = List.find (fun s -> fst s = str) !test_code in
    let z : Asm.coq_function = { fn_sig=sg; fn_code=code } in
    let extr_fun : Asm.fundef = AST.Internal z in
    let fun_def : (Asm.fundef, unit) globdef = Gfun extr_fun in
    let local_asm : Asm.program = {prog_defs = [(P.of_int 1, fun_def)]; prog_public = [P.of_int 1]; prog_main= P.of_int 1} in
    let _ = PrintAsm.print_program stdout local_asm in 
    let fge = Genv.globalenv local_asm in
    let regset = Asm.Pregmap.init Vundef in
    let pc_init = Genv.symbol_address fge local_asm.prog_main Ptrofs.zero in
    let regset = Asm.Pregmap.set PC pc_init regset in
    let regset = Asm.Pregmap.set RA Vundef regset in
    (* because we are in x86_64 only rpair One's will be returned *)
    let q = Conventions1.loc_arguments sg in
    let map_to_regs (s : Locations.loc AST.rpair) =
      match s with
      | One a -> 
        begin
          match a with
          | R mreg -> Some (Asmgen.ireg_of mreg)
          | _ -> None
        end
      | _ -> None
      in
    let rec set_regs rs args (regs : Asm.ireg Errors.res option list) = 
      match args with
        | [] -> Some rs
        | a::args2 -> 
        begin
        match (List.hd regs) with
          | Some t -> 
          begin
          match t with
            | OK r -> set_regs (Asm.Pregmap.set (IR r) a regset) args2 (List.tl regs)
            | _ -> None
          end
          | _ -> None
        end
      in 

    let xs = List.map map_to_regs q in
    let regset = set_regs regset args xs in
    match regset with
    | None -> None
    | Some regset ->

    (* let regset = Asm.Pregmap.set (IR RDI) (List.hd args) regset in
    let regset = Asm.Pregmap.set (IR RSI) (List.nth args 1) regset in *)
    flush stdout;
    (* let regset = Asm.Pregmap.set (IR RSI) (List.nth args' 1) regset in *)
    let result = step_insns fge local_asm regset m in
    match result with
      | None -> None
      | Some rs ->
    let ret_val = Asm.Pregmap.get (IR RAX) rs in
    convert_external_args ge args sg.sig_args >>= fun eargs ->
    let _ = match ret_val with
      | Vundef -> Printf.printf "undef\n"
      | _ -> () in
    Some (((w, []), ret_val), m)


let do_inline_assembly txt sg ge w args m = None

(* Implementing external functions producing observable events *)

let rec world ge m =
  lazy (Determinism.World(world_io ge m, world_vload ge m, world_vstore ge m))

and world_io ge m id args =
  None

and world_vload ge m chunk id ofs =
  Genv.find_symbol ge.genv_genv id >>= fun b ->
  Mem.load chunk m b ofs >>= fun v ->
  Cexec.eventval_of_val ge v (type_of_chunk chunk) >>= fun ev ->
  Some(ev, world ge m)

and world_vstore ge m chunk id ofs ev =
  Genv.find_symbol ge.genv_genv id >>= fun b ->
  Cexec.val_of_eventval ge ev (type_of_chunk chunk) >>= fun v ->
  Mem.store chunk m b ofs v >>= fun m' ->
  Some(world ge m')

let do_event p ge time w ev =
  if !trace >= 1 then
    fprintf p "@[<hov 2>Time %d: observable event:@ %a@]@."
              time print_event ev;
  (* Return new world after external action *)
  match ev with
  | Event_vstore(chunk, id, ofs, v) ->
      begin match Determinism.nextworld_vstore w chunk id ofs v with
      | None -> assert false
      | Some w' -> w'
      end
  | _ -> w

let rec do_events p ge time w t =
  match t with
  | [] -> w
  | ev :: t' -> do_events p ge time (do_event p ge time w ev) t'

(* Debugging stuck expressions *)

let (|||) a b = a || b (* strict boolean or *)

let diagnose_stuck_expr p ge w f a kont e m =
  let rec diagnose k a =
  (* diagnose subexpressions first *)
  let found =
    match k, a with
    | LV, Ederef(r, ty) -> diagnose RV r
    | LV, Efield(r, f, ty) -> diagnose RV r
    | RV, Evalof(l, ty) -> diagnose LV l
    | RV, Eaddrof(l, ty) -> diagnose LV l
    | RV, Eunop(op, r1, ty) -> diagnose RV r1
    | RV, Ebinop(op, r1, r2, ty) -> diagnose RV r1 ||| diagnose RV r2
    | RV, Ecast(r1, ty) -> diagnose RV r1
    | RV, Econdition(r1, r2, r3, ty) -> diagnose RV r1
    | RV, Eassign(l1, r2, ty) -> diagnose LV l1 ||| diagnose RV r2
    | RV, Eassignop(op, l1, r2, tyres, ty) -> diagnose LV l1 ||| diagnose RV r2
    | RV, Epostincr(id, l, ty) -> diagnose LV l
    | RV, Ecomma(r1, r2, ty) -> diagnose RV r1
    | RV, Eparen(r1, tycast, ty) -> diagnose RV r1
    | RV, Ecall(r1, rargs, ty) -> diagnose RV r1 ||| diagnose_list rargs
    | RV, Ebuiltin(ef, tyargs, rargs, ty) -> diagnose_list rargs
    | _, _ -> false in
  if found then true else begin
    let l = Cexec.step_expr ge do_external_function do_inline_assembly e w k a m in
    if List.exists (fun (ctx,red) -> red = Cexec.Stuckred) l then begin
      PrintCsyntax.print_pointer_hook := print_pointer ge.genv_genv e;
      fprintf p "@[<hov 2>Stuck subexpression:@ %a@]@."
              PrintCsyntax.print_expr a;
      true
    end else false
  end

  and diagnose_list al =
    match al with
    | Enil -> false
    | Econs(a1, al') -> diagnose RV a1 ||| diagnose_list al'

  in diagnose RV a

let diagnose_stuck_state p ge w = function
  | ExprState(f,a,k,e,m) -> ignore(diagnose_stuck_expr p ge w f a k e m)
  | _ -> ()

(* Execution of a single step.  Return list of triples
   (reduction rule, next state, next world). *)

(* p is the outchannel, prog is probs the program, 
    ge the global environment, s is the step???, w is unkown*)

let do_step p prog ge time s w =
  match Cexec.at_final_state s with
  | Some r ->
      if !trace >= 1 then
        fprintf p "Time %d: program terminated (exit code = %ld)@."
                  time (camlint_of_coqint r);
      begin match !mode with
      | All -> []
      | First | Random -> exit (Int32.to_int (camlint_of_coqint r))
      end
  | None ->
      let l = Cexec.do_step ge do_external_function do_inline_assembly w s in
      if l = []
      || List.exists (fun (Cexec.TR(r,t,s)) -> s = Stuckstate) l
      then begin
        pp_set_max_boxes p 1000;
        fprintf p "@[<hov 2>Stuck state: %a@]@." print_state (prog, ge, s);
        diagnose_stuck_state p ge w s;
        fprintf p "ERROR: Undefined behavior@.";
        exit 126
      end else begin
        List.map (fun (Cexec.TR(r, t, s')) -> (r, s', do_events p ge time w t)) l
      end

(* Exploration of a single execution. *)

let rec explore_one p prog ge time s w =
  if !trace >= 2 then
    fprintf p "@[<hov 2>Time %d:@ %a@]@." time print_state (prog, ge, s);
  let succs = do_step p prog ge time s w in
  if succs <> [] then begin
    let (r, s', w') =
      match !mode with
      | First -> List.hd succs
      | Random -> List.nth succs (Random.int (List.length succs))
      | All -> assert false in
    if !trace >= 2 then
      fprintf p "--[%s]-->@." (camlstring_of_coqstring r);
    explore_one p prog ge (time + 1) s' w'
  end

(* Exploration of all possible executions. *)

let rec explore_all p prog ge time states =
  if !trace >= 2 then begin

    List.iter
      (fun (n, s, w) ->
         fprintf p "@[<hov 2>State %d.%d: @ %a@]@."
                   time n print_state (prog, ge, s))
      states
  end;
  let rec explore_next nextstates seen numseen = function
  | [] ->
      List.rev nextstates
  | (n, s, w) :: states ->
      add_reducts nextstates seen numseen states n (do_step p prog ge time s w)

  and add_reducts nextstates seen numseen states n = function
  | [] ->
      explore_next nextstates seen numseen states
  | (r, s', w') :: reducts ->
      let (n', nextstates', seen', numseen') =
        try
          (StateMap.find s' seen, nextstates, seen, numseen)
        with Not_found ->
          (numseen,
           (numseen, s', w') :: nextstates,
           StateMap.add s' numseen seen,
           numseen + 1) in
      if !trace >= 2 then begin
        fprintf p "Transition state %d.%d --[%s]--> state %d.%d@."
                  time n (camlstring_of_coqstring r) (time + 1) n'
      end;
      add_reducts nextstates' seen' numseen' states n reducts
  in
    let nextstates = explore_next [] StateMap.empty 1 states in
    if nextstates <> [] then explore_all p prog ge (time + 1) nextstates

(* The variant of the source program used to build the world for
   executing events.
   Volatile variables are turned into non-volatile ones, so that
     reads and writes can be performed.
   Other variables are turned into empty vars, so that
     reads and writes just fail.
   Functions are preserved, although they are not used. *)

let world_program prog =
  let change_def (id, gd) =
    match gd with
    | Gvar gv ->
        let gv' =
          if gv.gvar_volatile then
            {gv with gvar_readonly = false; gvar_volatile = false}
          else
            {gv with gvar_init = []} in
        (id, Gvar gv')
    | Gfun fd ->
        (id, gd) in
 {prog with Ctypes.prog_defs = List.map change_def prog.Ctypes.prog_defs}

(* Massaging the program to get a suitable "main" function *)

let change_main_function p old_main old_main_ty =
  let old_main = Evalof(Evar(old_main, old_main_ty), old_main_ty) in
  let arg1 = Eval(Vint(coqint_of_camlint 0l), type_int32s) in
  let arg2 = arg1 in
  let body =
    Sreturn(Some(Ecall(old_main, Econs(arg1, Econs(arg2, Enil)), type_int32s))) in
  let new_main_fn =
    { fn_return = type_int32s; fn_callconv = cc_default;
      fn_params = []; fn_vars = []; fn_body = body } in
  let new_main_id = intern_string "___main" in
  { prog_main = new_main_id;
    Ctypes.prog_defs = (new_main_id, Gfun(Ctypes.Internal new_main_fn)) :: p.Ctypes.prog_defs;
    Ctypes.prog_public = p.Ctypes.prog_public;
    prog_types = p.prog_types;
    prog_comp_env = p.prog_comp_env }

let rec find_main_function name = function
  | [] -> None
  | (id, Gfun fd) :: gdl -> if id = name then Some fd else find_main_function name gdl
  | (id, Gvar v) :: gdl -> find_main_function name gdl

let fixup_main p =
  match find_main_function p.Ctypes.prog_main p.Ctypes.prog_defs with
  | None ->
      fprintf err_formatter "ERROR: no main() function@.";
      None
  | Some main_fd ->
      match type_of_fundef main_fd with
      | Tfunction(Tnil, Ctypes.Tint(I32, Signed, _), _) ->
          Some p
      | Tfunction(Tcons(Ctypes.Tint _, Tcons(Tpointer(Tpointer(Ctypes.Tint(I8,_,_),_),_), Tnil)),
                  Ctypes.Tint _, _) as ty ->
          Some (change_main_function p p.Ctypes.prog_main ty)
      | _ ->
          fprintf err_formatter "ERROR: wrong type for main() function@.";
          None


let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let rec to_int n o = match n with
  | 0 -> o
  | _ -> to_int (n-1) (Datatypes.S o)

let linear_addr reg ofs = Asm.Addrmode(Some reg, None, Coq_inl ofs)
let global_addr id ofs = Asm.Addrmode(None, None, Coq_inr(id, ofs))


(* Execution of a whole program *)
(* takes program as parsed csyntax *)
let execute prog =
  let oc_in = Unix.open_process_in "python ./main.py ./test_obj.o extr" in
  let lines = ref [] in
  let x = 
  try 
    while true do
      lines := input_line oc_in :: !lines
    done; !lines
  with End_of_file -> close_in oc_in; 
    List.rev !lines in

  List.iter print_endline x;

  print_endline "-----------------";

  (* let addr1 = linear_addr RSP (Z.of_uint 16) in
  let addr2 = linear_addr RSP (Z.of_uint 0) in

  (* stupidly did Int64.repr (BinNums.Z.of_nat (to_int 8)) before, write up*)
  (* let insn1 = Asm.Psubq_ri (Asm.RSP, Z.of_uint 8) in
  let insn2 = Asm.Pleaq (RAX, addr1) in
  let insn3 = Asm.Pmovq_mr (addr2, RAX) in *)
  let insn1 = Asm.Pallocframe (Z.of_uint 8, Z.of_uint 0, Z.of_uint 0) in
  let insn4 = Asm.Pmov_rr (RAX, RDI) in 
  let insn5 = Asm.Psubl_rr (RAX, RSI) in
  let insn6 = Asm.Pfreeframe (Z.of_uint 8, Z.of_uint 0, Z.of_uint 0) in
  (* let insn6 = Asm.Paddq_ri (RSP, Z.of_uint 8) in *)
  let insn7 = Asm.Pret in *)
(* 
  let functions = ref *) 

  let getz a i = Z.of_uint (int_of_string (List.nth a i)) in
  let getr a i = match List.nth a i with
    | "RAX" -> Asm.RAX
    | "RBX" -> Asm.RBX
    | "RCX" -> Asm.RCX
    | "RDX" -> Asm.RDX
    | "RSI" -> Asm.RSI
    | "RDI" -> Asm.RDI
    | "RBP" -> Asm.RBP
    | "RSP" -> Asm.RSP
    | "R8"  -> Asm.R8 
    | "R9"  -> Asm.R9
    | "R10" -> Asm.R10
    | "R11" -> Asm.R11
    | "R12" -> Asm.R12
    | "R13" -> Asm.R13
    | "R14" -> Asm.R14
    | "R15" -> Asm.R15
    | _ ->     Asm.RAX
  in
    

  let rec set_asm (a :: list) (l : Asm.instruction list) = 
    let (f :: (b : string list)) = Str.split (Str.regexp " ") a in
    match f with
    | ">" -> if (List.length l) > 0 then 
      begin
        test_code := !test_code @ [((List.nth b 0), l)]; if List.length list > 0 then set_asm list [] else ()
      end
      else ()
    | i   -> 
      match i with
      | "Pallocframe"   -> set_asm list (l @ [Asm.Pallocframe (getz b 0, getz b 1, getz b 2)])
      | "Pfreeframe"    -> set_asm list (l @ [Asm.Pfreeframe (getz b 0, getz b 1, getz b 2)])
      | "Pmovl_ri"      -> set_asm list (l @ [Asm.Pmovl_ri (getr b 0, getz b 1)])
      | "Pcall_r"       -> set_asm list (l @ [Asm.Pnop])
      | "Pcallxorl_rr"  -> set_asm list (l @ [Asm.Pxorl_rr (getr b 0, getr b 1)])
      | "Pret"          -> set_asm list (l @ [Asm.Pret])
      | "Pmov_rr"       -> set_asm list (l @ [Asm.Pmov_rr (getr b 0, getr b 1)])
      | "Psubl_rr"      -> set_asm list (l @ [Asm.Psubl_rr (getr b 0, getr b 1)])
      | "Pleal"         -> set_asm list (l @ [Asm.Pleal (getr b 0, Addrmode (Some (getr b 1), Some (getr b 2, getz b 3), Coq_inl (getz b 4)) )])
      | _ -> Printf.printf "Unkown instruction: %s" i;
  in

  (* let fun_asm : (string * Asm.code) = ("extr", [insn1; insn4; insn5; insn6; insn7]) in *)
  set_asm x []; 
  (* test_code := !test_code @ [fun_asm]; *)

  (* test_code := [("extr", [insn1; insn2; insn3; insn4; insn5; insn6; insn7])]; *)

  Random.self_init();
  let p = std_formatter in
  pp_set_max_indent p 30;
  pp_set_max_boxes p 10;
  match fixup_main prog with
  | None -> exit 126
  | Some prog1 ->
      let wprog = world_program prog1 in
      let wge = globalenv wprog in
      match Genv.init_mem (program_of_program wprog) with
      | None ->
          fprintf p "ERROR: World memory state undefined@."; exit 126
      | Some wm ->
      match Cexec.do_initial_state prog1 with
      | None ->
          fprintf p "ERROR: Initial state undefined@."; exit 126
      | Some(ge, s) ->
          match !mode with
          | First | Random ->
              explore_one p prog1 ge 0 s (world wge wm)
          | All ->
              explore_all p prog1 ge 0 [(1, s, world wge wm)]
