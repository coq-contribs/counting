(*i camlp4deps: "parsing/grammar.cma" i*)
(*i camlp4use: "pa_extend.cmp" i*)

open Term
open Pp
open Flags
open Stdarg

(* Computes the size of a term *)
let rec constr_size c =
  let aconstr_size = Array.fold_left (fun n c -> n + constr_size c) 0 in
  match kind_of_term c with
    | Rel i -> 1
    | Var id -> 1
    | Meta _ -> 1
    | Evar (i, constr_array) -> 1 + aconstr_size constr_array
    | Sort s -> 1
    | Cast (term, _, typ) -> constr_size term + constr_size typ
    | Prod (name, typ, typ') -> constr_size typ + constr_size typ'
    | Lambda (name, typ, constr) -> constr_size typ + constr_size constr
    | LetIn (name, constr, typ, constr') ->
	constr_size constr + constr_size typ + constr_size constr'
    | App (constr, constr_array) ->
	constr_size constr + aconstr_size constr_array
    | Const const -> 1
    | Ind(mult_ind, i) -> 1
    | Construct ((mult_ind, i), i')-> 1
    | Case (case_info, constr, constr', constr_array) ->
	constr_size constr + constr_size constr' + aconstr_size constr_array
    | Fix ((int_array, int),(name_a, type_a, constr_a)) ->
	aconstr_size type_a + aconstr_size constr_a + Array.length name_a
    | CoFix (int, (name_a, type_a, constr_a)) ->
	aconstr_size type_a + aconstr_size constr_a + Array.length name_a
    | Proj (_, c) -> constr_size c

(* References holding the state *)
let total_proofs_size = ref 0
let total_defs_size = ref 0
let count = ref false

(* Functions corresponding to new vernaculars *)
let start_counting () = count := true
let stop_counting () = count := false
let reset () = total_proofs_size := 0; total_defs_size := 0
let print_count () =
  if_verbose Feedback.msg_notice (str "Total size of definitions so far : " ++ int !total_defs_size);
  if_verbose Feedback.msg_notice (str "Total size of proofs so far : " ++ int !total_proofs_size);
  if_verbose Feedback.msg_notice (str "Counting is currently " ++ str (if !count then "on" else "off"))

(* Hooks for recording stats *)
let extract_size pft =
  if !count then
    try
      total_proofs_size := !total_proofs_size +
	List.fold_left (fun acc t -> acc + constr_size t)
	  0 (Proof.partial_proof pft)
    with _ -> ()

let entry_size {Entries.const_entry_body = body} =
  if !count then
    let ((c, _), _) = Future.force body in
    total_defs_size := !total_defs_size + constr_size c

(* Initialization : setting hooks up and registering the state
   in the summary table *)
let _ =
  let entryhk = Command.get_declare_definition_hook () in
  Command.set_declare_definition_hook (fun c -> entryhk c; entry_size c);
  (* nb : theres no get_save_hook to avoid overwriting an existing hook *)
  Lemmas.set_save_hook extract_size;
  Summary.declare_summary "definition and proof count"
    {
      Summary.freeze_function =
	(fun _ -> (!total_defs_size, !total_proofs_size, !count));
      Summary.unfreeze_function =
	(fun (d, p, c) ->
	   total_defs_size := d; total_proofs_size := p; count := c);
      Summary.init_function =
	(fun () -> reset (); stop_counting ())
    }

(* Size of a particular object *)
let size_of cref =
  match Nametab.global cref with
    | Globnames.ConstRef sp ->
	begin
	  match Global.body_of_constant sp with
	    | None -> if_verbose Feedback.msg_notice (str "No body : cannot count.")
	    | Some c ->
		let n = constr_size c in
		if_verbose Feedback.msg_notice (str "Size of " ++
			 Libnames.pr_reference cref ++ str " : " ++ int n)
	end
    |  _ -> if_verbose Feedback.msg_notice (str "Only works for constants.")

(* Syntax extensions *)
VERNAC COMMAND EXTEND StartCounting CLASSIFIED AS SIDEFF
 ["Start" "Counting"] ->
   [ start_counting () ]
     END

VERNAC COMMAND EXTEND StopCounting CLASSIFIED AS SIDEFF
 ["Stop" "Counting"] ->
   [ stop_counting () ]
END

VERNAC COMMAND EXTEND ResetCounting CLASSIFIED AS SIDEFF
 ["Reset" "Count"] ->
   [ reset () ]
END

VERNAC COMMAND EXTEND DisplayCounting CLASSIFIED AS QUERY
 ["Print" "Count"] ->
   [ print_count () ]
END

VERNAC COMMAND EXTEND SizeOf CLASSIFIED AS QUERY
 ["Size" global(id)] ->
   [ size_of id ]
END
