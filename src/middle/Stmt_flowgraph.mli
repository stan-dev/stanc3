open Common

module Forward :
  Flowgraph.S with type t = Stmt.Labelled.t and module Label = Int_label

module Reverse :
  Flowgraph.S with type t = Stmt.Labelled.t and module Label = Int_label
