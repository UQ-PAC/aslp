
(** Types and supporting functions required for reader-writer-state monad.

    r is an immutable local environment passed to computations,
    w is a type of values produced alongside main computation results, and
    s is a local state which is passed to computations and can be modified.

    Note: Here, "modified" means immutable updates and replacing the current
    value with a new augmented version.
    *)
module type S = sig
  type r
  type w
  type s

  val mempty : w
  val mappend : w -> w -> w
end

(** Constructs a reader-writer-state monad using the given type specifications.

    This provides a "rws" type which is parametrised by a result type.
    For example, "int rws" is a computation using the reader-writer-state
    types as described above and eventually returning an int result.

    Also provides usual functional programming constructs for working
    with the monadic type.

    In the Let module, bindings are given for let*, and*, let+, and and+ for
    composing monadic computations using let syntax.
    *)
module RWSBase (T : S) = struct
  include T

  (** A reader-writer-state monad.
      This is a function which takes an immutable "environment",
      a local mutable "state", and produces a value of some type 'a,
      along with a new local state and side-effects of writes. *)
  type 'a rws = r -> s -> 'a * s * w

  (* monad definition for rws *)

  (** Applies the given function to the result of the computation. *)
  let fmap (f: 'a -> 'b) (x: 'a rws): 'b rws =
    fun r s ->
      let (a,s',w) = x r s in
      (f a, s', w)

  (** A computation returning a constant value and making no state changes. *)
  let pure (a: 'a): 'a rws =
    fun _ s -> (a, s, T.mempty)

  (** Compose computations in sequence,
      passing the result of the first into the second. *)
  let bind (x: 'a rws) (f: 'a -> 'b rws): 'b rws =
    fun r s ->
      let (a, s', w) = x r s in
      let (b, s'', w') = f a r s' in
      (b, s'', T.mappend w w')

  (* rws-specific utility functions *)

  (* READER *)

  (** A computation returning the immutable reader environment. *)
  let read: r rws = fun r s -> (r, s, mempty)

  (** A computation reading the environment and applying the given function to it. *)
  let reads (f: r -> 'a): 'a rws = fun r s -> (f r, s, mempty)

  (* WRITER *)

  (** A computation writing the given values. *)
  let write (w: w): unit rws = fun _ s -> ((), s, w)


  (* STATE *)

  (** A computation returning the value of the local mutable state.  *)
  let get: s rws = fun _ s -> (s, s, mempty)

  (** A computation reading the local state and applying the given function.  *)
  let gets (f: s -> 'a): 'a rws = fun _ s -> (f s, s, mempty)

  (** A computation setting the local state to the given value. *)
  let put (s: s): unit rws = fun _ _ -> ((), s, mempty)

  (** A computation modifying the local state using the given function.  *)
  let modify (f: s -> s): unit rws = fun _ s -> ((), f s, mempty)

  (** A computation returning some value and also modifying state using the given function.  *)
  let stateful (f: s -> 'a * s): 'a rws =
    fun _ s -> let (a,s') = f s in (a, s', mempty)


  (** Runs a computation transiently without modifying the state or writer.
      Instead, returns the final state and writer values alongside the result. *)
  let locally (x: 'a rws): ('a * s * w) rws =
    fun r s ->
      let (a,s',w) = x r s in
      ((a,s',w), s, T.mempty)

  (** Runs a computation transiently without modifying the state or writer.
      Instead, returns only the final state and writer values.

      This is just `locally` but discarding the result value of the computation. *)
  let locally_ (x: 'a rws): (s * w) rws =
    fun r s ->
      let (a,s',w) = x r s in
      ((s',w), s, T.mempty)

  let defer (f: unit -> 'a): 'a rws =
    fun r s ->
      (f (), s, T.mempty)

  (** Runs a computation catching an exception if one is thrown.
      Returns either the result or the thrown exception. *)
  let catcherror (x: 'a rws): ('a, exn * Printexc.raw_backtrace) Result.t rws =
    fun r s ->
      try let (x,s',w') = x r s in (Ok x, s', w')
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        (Error (e, bt), s, mempty)

end

(** Constructs a RWS monad using the given signature.  *)
module Make(T : S) = struct

  include RWSBase(T)

  include Monad.Make(struct
    type 'a m = 'a rws
    include RWSBase(T)
  end)

end


module Test = struct
  module Hi = Make(struct
    type r = unit
    type w = unit list
    type s = unit
    let mempty = []
    let mappend = (@)
  end);;

  open Hi;;
  open Hi.Let;;

  let test: int Hi.rws = fun x y -> (100, y, [(); (); (); ()])

  let a = let* x = test and* y = pure "a" in pure x;;

  let main () =
    let* xx =
      try reads (fun e -> (* raise (Invalid_argument "a"); *) 99999)
      with Invalid_argument _ -> pure 55 in
    let* a = Hi.pure "asdf"
    and+ b = test
    and+ c = test in
      Printf.printf "%s %d\n" a b;
      Printf.printf "xx = %d\n" xx;
      pure a;;
end;;
