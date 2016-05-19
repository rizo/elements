
let lookup_rule g t =
  let sym  = t.value in
  let name = show_literal sym in
  let precedence = lookup_precedence g name |? (g.default sym).precedence in
  { sym; name; precedence }



let lookup_rule g t =
  let sym  = t.value in
  let name = show_literal sym in
  let precedence = option (g.default sym).precedence (lookup_precedence g name) in
  { sym; name; precedence }


let lookup_rule g t =
  let sym  = t.value in
  let name = show_literal sym in
  let precedence = Option.with_default (g.default sym).precedence (lookup_precedence g name) in
  { sym; name; precedence }


let lookup_rule g t =
  let sym  = t.value in
  let name = show_literal sym in
  let precedence = Option.force ~default:(g.default sym).precedence (lookup_precedence g name) in
  { sym; name; precedence }

