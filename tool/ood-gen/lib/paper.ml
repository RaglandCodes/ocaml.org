type link = { description : string; uri : string } [@@deriving of_yaml]

type metadata = {
  title : string;
  publication : string;
  authors : string list;
  abstract : string;
  tags : string list;
  year : int;
  links : link list;
  featured : bool;
}
[@@deriving of_yaml]

type t = {
  title : string;
  slug : string;
  publication : string;
  authors : string list;
  abstract : string;
  tags : string list;
  year : int;
  links : link list;
  featured : bool;
}
[@@deriving stable_record ~version:metadata ~remove:[ slug ]]

let of_metadata m = of_metadata m ~slug:(Utils.slugify m.title)
let decode s = Result.map of_metadata (metadata_of_yaml s)

let all () =
  Utils.yaml_sequence_file decode "papers.yml"
  |> List.sort (fun p1 p2 ->
         (2 * Int.compare p2.year p1.year) + String.compare p1.title p2.title)

let pp_link ppf (v : link) =
  Fmt.pf ppf
    {|
        { description = %a
        ; uri = %a
        }|}
    Pp.string v.description Pp.string v.uri

let pp ppf v =
  Fmt.pf ppf
    {|
  { title = %a
  ; slug = %a
  ; publication = %a
  ; authors = %a
  ; abstract = %a
  ; tags = %a
  ; year = %i
  ; links = %a
  ; featured = %a
  }|}
    Pp.string v.title Pp.string v.slug Pp.string v.publication
    (Pp.list Pp.string) v.authors Pp.string v.abstract (Pp.list Pp.string)
    v.tags v.year (Pp.list pp_link) v.links Pp.bool v.featured

let pp_list = Pp.list pp

let template () =
  Format.asprintf
    {|
  type link = { description : string; uri : string }

type t =
  { title : string
  ; slug : string
  ; publication : string
  ; authors : string list
  ; abstract : string
  ; tags : string list
  ; year : int
  ; links : link list
  ; featured : bool
  }

let all = %a
|}
    pp_list (all ())
