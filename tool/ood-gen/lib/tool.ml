type metadata = {
  name : string;
  source : string;
  license : string;
  synopsis : string;
  description : string;
  lifecycle : string;
}
[@@deriving of_yaml]

module Lifecycle = struct
  type t = [ `Incubate | `Active | `Sustain | `Deprecate ]

  let to_string = function
    | `Incubate -> "incubate"
    | `Active -> "active"
    | `Sustain -> "sustain"
    | `Deprecate -> "deprecate"

  let of_string = function
    | "incubate" -> Ok `Incubate
    | "active" -> Ok `Active
    | "sustain" -> Ok `Sustain
    | "deprecate" -> Ok `Deprecate
    | s -> Error (`Msg ("Unknown lifecycle type: " ^ s))
end

type t = {
  name : string;
  slug : string;
  source : string;
  license : string;
  synopsis : string;
  description : string;
  lifecycle : Lifecycle.t;
}
[@@deriving
  stable_record ~version:metadata ~modify:[ lifecycle; description ]
    ~remove:[ slug ]]

let of_metadata m =
  of_metadata m ~slug:(Utils.slugify m.name)
    ~modify_lifecycle:(Utils.decode_or_raise Lifecycle.of_string)
    ~modify_description:(fun v -> Omd.of_string v |> Omd.to_html)

let decode s =
  Import.Result.apply (Ok of_metadata) (metadata_of_yaml s)

let all () =
  Utils.yaml_sequence_file decode "tools.yml"

let pp_lifecycle ppf v =
  Fmt.pf ppf "%s"
    (match v with
    | `Incubate -> "`Incubate"
    | `Active -> "`Active"
    | `Sustain -> "`Sustain"
    | `Deprecate -> "`Deprecate")

let pp ppf v =
  Fmt.pf ppf
    {|
  { name = %a
  ; slug = %a
  ; source = %a
  ; license = %a
  ; synopsis = %a
  ; description = %a
  ; lifecycle = %a
  }|}
    Pp.string v.name Pp.string v.slug Pp.string v.source Pp.string v.license
    Pp.string v.synopsis Pp.string v.description pp_lifecycle v.lifecycle

let pp_list = Pp.list pp

let template () =
  Format.asprintf
    {|
type lifecycle =
  [ `Incubate
  | `Active
  | `Sustain
  | `Deprecate
  ]

type t =
  { name : string
  ; slug : string
  ; source : string
  ; license : string
  ; synopsis : string
  ; description : string
  ; lifecycle : lifecycle
  }
  
let all = %a
|}
    pp_list (all ())
