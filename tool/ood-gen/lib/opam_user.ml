type metadata = {
  name : string;
  email : string option;
  github_username : string option;
  avatar : string option;
}
[@@deriving of_yaml]

let all () = Utils.yaml_sequence_file metadata_of_yaml "opam-users.yml"

let pp ppf v =
  Fmt.pf ppf
    {|
  { name = %a
  ; email = %a
  ; github_username = %a
  ; avatar = %a
  }|}
    Pp.string v.name (Pp.option Pp.string) v.email (Pp.option Pp.string)
    v.github_username (Pp.option Pp.string) v.avatar

let pp_list = Pp.list pp

let template () =
  Format.asprintf
    {|
type t =
  { name : string
  ; email : string option
  ; github_username : string option
  ; avatar : string option
  }
  
let all = %a
|}
    pp_list (all ())
