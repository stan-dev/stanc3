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

    let apply kf kv
        =
     fun s ->
      let f, s' = kf s in
      let v, s'' = kv s' in
      (f v, s'')

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

module Right = struct 
  
  module State : S = struct 
    module T = struct
      type ('a, 'state) t = 'state -> 'a * 'state

      let return x = fun s -> (x, s)

      let map_ kv ~f = 
      fun s ->
        let v, s' = kv s in
        (f v, s')

      let map = `Custom map_

      let bind fv ~f = 
      fun s ->
        let v, s' = fv s in
        (f v) s'

      let apply kf kv
          =
      fun s ->
        let v, s' = kv s in
        let f, s'' = kf s' in
        (f v, s'')

      let get  = fun s -> (s, s)
      let put s = fun _ -> ((), s)
      let modify f  = fun s -> ((), f s)
      let run_state x ~init = x init

      let with_state fv ~f =
          fun s -> fv @@ f s
    end

    include T
    include Applicative.Make2 (T)
    include Monad.Make2 (T)
  end 
  module Cps = struct 
    module State = struct 
      module T = struct
        type ('a, 'state) t = {apply: 'r. 'state -> ('a -> 'state -> 'r) -> 'r}

        let unapply {apply} s k = apply s k
        let run_state x ~init = unapply x init (fun a s -> (a, s))
        let return x = {apply= (fun s k -> k x s)}
        let map_ x ~f = {apply= (fun s k -> unapply x s (fun x -> k @@ f x))}
        let map = `Custom map_

        let bind x ~f =
          {apply= (fun s k -> unapply x s @@ fun a s' -> unapply (f a) s' k)}

        let apply kf kv =
          { apply=
              (fun s k ->
                unapply kv s @@ fun v s' -> unapply kf s' (fun f -> k @@ f v) ) }

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
end




