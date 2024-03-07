let pp_uchar ppf u =
  let u_int = Uchar.to_int u in
  if u_int < 128 then Fmt.string ppf (Char.chr u_int |> Char.escaped)
  else Fmt.pf ppf "U+%04X" u_int

let is_ascii s =
  let rec loop max b i =
    if i > max then true
    else if Bytes.get_uint8 b i < 128 then loop max b (i + 1)
    else false in
  let b = Bytes.of_string s in
  loop (Bytes.length b - 1) b 0

let normalize = Uunf_string.normalize_utf_8 `NFKC

let foldi_uchars ~f acc str =
  let len = String.length str in
  let rec loop pos acc =
    if pos == len then acc
    else
      let decode = String.get_utf_8_uchar str pos in
      let char_length = Uchar.utf_decode_length decode in
      let uchar = Uchar.utf_decode_uchar decode in
      let acc = f acc pos uchar in
      loop (pos + char_length) acc in
  loop 0 acc

let iteri_uchars ~f str =
  let f' buf pos c =
    f pos c;
    Buffer.add_utf_8_uchar buf c;
    buf in
  let s_after =
    Buffer.contents
    @@ foldi_uchars ~f:f' (Buffer.create (String.length str)) str in
  (* another sanity check *)
  if not (String.equal str s_after) then
    Core.(
      ICE.internal_compiler_error
        [%message
          "Failed to round-trip unicode string!"
            (str : string)
            (s_after : string)])

(* WIP:

   While not strictly necessary, there are some additional restrictions which
   are good to implement for validation and preventing strings that are visually
   identical from being distinct identifiers.
   A good summary can be found here: https://perl11.org/blog/unicode-identifiers.html

   Most of these are only a problem if you assume maliciousness of the user,
   so they may not be important for an initial version in Stan.
*)

(* Defined in https://www.unicode.org/reports/tr39/#Confusable_Detection *)
let confusable x y =
  let skeleton x =
    let x = Uunf_string.normalize_utf_8 `NFD x in
    let f acc _ c =
      if Uucp.Gen.is_default_ignorable c then ()
      else
        (* TODO!! replace with prototype - need data? *)
        Buffer.add_utf_8_uchar acc c;
      acc in
    let buf = foldi_uchars ~f (Buffer.create (String.length x)) x in
    let x = Buffer.contents buf in
    let x = Uunf_string.normalize_utf_8 `NFD x in
    x in
  String.compare (skeleton x) (skeleton y)

module ScriptSet = Set.Make (Uucp.Script)

(** copied from UUCP's definition of [Uucp.Script.t] *)
let all =
  ScriptSet.of_list
    [ `Adlm; `Aghb; `Ahom; `Arab; `Armi; `Armn; `Avst; `Bali; `Bamu; `Bass; `Batk
    ; `Beng; `Bhks; `Bopo; `Brah; `Brai; `Bugi; `Buhd; `Cakm; `Cans; `Cari
    ; `Cham; `Cher; `Chrs; `Copt; `Cpmn; `Cprt; `Cyrl; `Deva; `Diak; `Dogr
    ; `Dsrt; `Dupl; `Egyp; `Elba; `Elym; `Ethi; `Geor; `Glag; `Gong; `Gonm
    ; `Goth; `Gran; `Grek; `Gujr; `Guru; `Hang; `Hani; `Hano; `Hatr; `Hebr
    ; `Hira; `Hluw; `Hmng; `Hmnp; `Hrkt; `Hung; `Ital; `Java; `Kali; `Kana
    ; `Kawi; `Khar; `Khmr; `Khoj; `Knda; `Kthi; `Kits; `Lana; `Laoo; `Latn
    ; `Lepc; `Limb; `Lina; `Linb; `Lisu; `Lyci; `Lydi; `Mahj; `Maka; `Mand
    ; `Mani; `Marc; `Medf; `Mend; `Merc; `Mero; `Mlym; `Modi; `Mong; `Mroo
    ; `Mtei; `Mult; `Mymr; `Nagm; `Nand; `Narb; `Nbat; `Newa; `Nkoo; `Nshu
    ; `Ogam; `Olck; `Orkh; `Orya; `Osge; `Osma; `Ougr; `Palm; `Pauc; `Perm
    ; `Phag; `Phli; `Phlp; `Phnx; `Plrd; `Prti; `Qaai; `Rjng; `Rohg; `Runr
    ; `Samr; `Sarb; `Saur; `Sgnw; `Shaw; `Shrd; `Sidd; `Sind; `Sinh; `Sogd
    ; `Sogo; `Sora; `Soyo; `Sund; `Sylo; `Syrc; `Tagb; `Takr; `Tale; `Talu
    ; `Taml; `Tang; `Tavt; `Telu; `Tfng; `Tglg; `Thaa; `Thai; `Tibt; `Tirh
    ; `Tnsa; `Toto; `Ugar; `Vaii; `Vith; `Wara; `Wcho; `Xpeo; `Xsux; `Yezi
    ; `Yiii; `Zanb; `Zinh; `Zyyy; `Zzzz ]

let extended s =
  if ScriptSet.mem `Zyyy s || ScriptSet.mem `Zinh s then all else s

(* Defined in https://www.unicode.org/reports/tr39/#Restriction_Level_Detection *)
let restriction_level x =
  let f acc _ c =
    let scripts =
      Uucp.Script.script_extensions c |> ScriptSet.of_list |> extended in
    scripts :: acc in
  let soss = foldi_uchars ~f [] x in
  let resolved = List.fold_right ScriptSet.inter soss all in
  if not @@ ScriptSet.is_empty resolved then `Single
  else `Unrestricted (* TODO implement levels 3-5 *)
