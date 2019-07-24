include Core_kernel

module type S = sig
  include Applicative.S2
  include Monad.S2 with type ('a, 'state) t := ('a, 'state) t

  val get : ('state, 'state) t
  val put : 'state -> (unit, 'state) t
  val modify : ('state -> 'state) -> (unit, 'state) t
  val with_state : ('a, 'state) t -> f:('state -> 'state) -> ('a, 'state) t
  val run_state : ('a, 'state) t -> init:'state -> 'a * 'state
end

module State : S = struct
  module T = struct
    type ('a, 'state) t = 'state -> 'a * 'state

    let return (x : 'a) : ('a, 'state) t = fun s -> (x, s)

    let map_ (x : ('a, 'state) t) ~(f : 'a -> 'b) : ('b, 'state) t =
     fun s ->
      let y, s' = x s in
      (f y, s')

    let map = `Custom map_

    let bind (x : ('a, 'state) t) ~(f : 'a -> ('b, 'state) t) : ('b, 'state) t
        =
     fun s ->
      let a, s' = x s in
      (f a) s'

    let apply (f : ('a -> 'b, 'state) t) (x : ('a, 'state) t) : ('b, 'state) t
        =
     fun s ->
      let g, s' = f s in
      let y, s'' = x s' in
      (g y, s'')

    let get : ('state, 'state) t = fun s -> (s, s)
    let put s : (unit, 'state) t = fun _ -> ((), s)
    let modify f : (unit, 'state) t = fun s -> ((), f s)
    let run_state x ~init = x init

    let with_state (x : ('a, 'state) t) ~(f : 'state -> 'state) :
        ('a, 'state) t =
     fun s -> x @@ f s
  end

  include T
  include Applicative.Make2 (T)
  include Monad.Make2 (T)
end

module Cps = struct
  module State = struct
    module T = struct
      type ('a, 'state) t = {apply: 'r. 'state -> ('a -> 'state -> 'r) -> 'r}

      let unapply x st k = x.apply st k
      let run_state x ~init = unapply x init (fun a st -> (a, st))
      let return x = {apply= (fun e k -> k x e)}
      let map_ x ~f = {apply= (fun e k -> unapply x e (fun x -> k @@ f x))}
      let map = `Custom map_

      let bind x ~f =
        {apply= (fun e k -> unapply x e @@ fun a e' -> unapply (f a) e' k)}

      let apply f x =
        { apply=
            (fun e k ->
              unapply f e @@ fun g e' -> unapply x e' (fun x -> k @@ g x) ) }

      let get = {apply= (fun e k -> k e e)}
      let put s = {apply= (fun _ k -> k () s)}
      let modify f = {apply= (fun e k -> k () @@ f e)}

      let with_state (x : ('a, 'state) t) ~(f : 'state -> 'state) :
          ('a, 'state) t =
        bind x ~f:(fun y -> bind (modify f) ~f:(fun _ -> return y))
    end

    include T
    include Applicative.Make2 (T)
    include Monad.Make2 (T)
  end
end
