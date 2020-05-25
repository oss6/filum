open Base
open Re2

let escape_characters_map = [
  ("<", "&lt;"),
  (">", "&gt;"),
  ("&", "&amp;"),
  ("\"", "&quot;"),
  ("'", "&#x27;"),
  ("`", "&#x60;")
]

let escape_html s =
  replace_exn
  ~f:(
    fun x ->
      List.Assoc.find_exn escape_characters_map
      ~equal:String.equal
      (Match.get_exn x ~sub:(`Index 0))
  )
  (create_exn "[<>&\"'`]+")
  s
