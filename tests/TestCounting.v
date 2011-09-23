Require Import Counting.

(** Testing all commands *)

Print Count.

Definition z := nat.
Size z.
Size tt.
Definition z' := 90.
Size z'.

Print Count.

Start Counting.

Definition z'' := 90.
Print Count.

Goal z'' = 90.
reflexivity.
Qed.
Print Count.

Stop Counting.
Print Count.

Goal z' = z''.
reflexivity.
Qed.
Print Count.

Reset Count.
Print Count.
