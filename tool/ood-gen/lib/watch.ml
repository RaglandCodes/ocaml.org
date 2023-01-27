type metadata = {
  name : string;
  embed_path : string;
  thumbnail_path : string;
  description : string option;
  published_at : string;
  language : string;
  category : string;
}
[@@deriving of_yaml]

let all () =
  Utils.yaml_sequence_file metadata_of_yaml "watch.yml"

let pp ppf v =
  Fmt.pf ppf
    {|
  { name = %a
  ; embed_path = %a
  ; thumbnail_path = %a
  ; description = %a
  ; published_at = %a
  ; language = %a
  ; category = %a
  }|}
    Pp.string v.name Pp.string v.embed_path Pp.string v.thumbnail_path
    Pp.(option string)
    v.description Pp.string v.published_at Pp.string v.language Pp.string
    v.category

let pp_list = Pp.list pp

let template () =
  Format.asprintf
    {|

  type t =
  { name: string;
    embed_path : string;
    thumbnail_path : string;
    description : string option;
    published_at : string;
    language : string;
    category : string;
  }
  
let all = %a
|}
    pp_list (all ())
