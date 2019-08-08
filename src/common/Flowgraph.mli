module type Basic = sig
  type t

  module Label : Label.S

  val flow_of_t : t -> (Label.t * Label.t) list
  val initial_label_of_t : t -> Label.t
  val final_labels_of_t : t -> Label.Set.t
  val associations_of_t : t -> t Label.Map.t
  val t_of_associations : t Label.Map.t -> t option
end

module type S = sig
  type t

  module Label : Label.S

  val flow_of_t : t -> (Label.t * Label.t) list
  val extremal_labels_of_t : t -> Label.Set.t
  val associations_of_t : t -> t Label.Map.t
  val t_of_associations : t Label.Map.t -> t option
end

module Make (X : Basic) : S with type t = X.t and module Label = X.Label

module Make_reverse (X : Basic) :
  S with type t = X.t and module Label = X.Label
