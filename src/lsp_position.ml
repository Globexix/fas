let char_at text i =
  let length = String.length text in
  let byte k = Char.code text.[k] in
  let within lo hi k = k < length && byte k >= lo && byte k <= hi in
  let tail k = within 0x80 0xBF k in
  let lead = byte i in
  if lead < 0x80 then (1, 1)
  else if lead >= 0xC2 && lead <= 0xDF && tail (i + 1) then (2, 1)
  else if
    lead >= 0xE0 && lead <= 0xEF
    && within
         (if lead = 0xE0 then 0xA0 else 0x80)
         (if lead = 0xED then 0x9F else 0xBF)
         (i + 1)
    && tail (i + 2)
  then (3, 1)
  else if
    lead >= 0xF0 && lead <= 0xF4
    && within
         (if lead = 0xF0 then 0x90 else 0x80)
         (if lead = 0xF4 then 0x8F else 0xBF)
         (i + 1)
    && tail (i + 2)
    && tail (i + 3)
  then (4, 2)
  else (1, 1)

let position_of_offset text offset =
  let offset = max 0 (min offset (String.length text)) in
  let rec line_start i line start =
    if i >= offset then (line, start)
    else if text.[i] = '\n' then line_start (i + 1) (line + 1) (i + 1)
    else line_start (i + 1) line start
  in
  let line, start = line_start 0 0 0 in
  let rec units i acc =
    if i >= offset then acc
    else
      let width, size = char_at text i in
      units (i + width) (acc + size)
  in
  (line, units start 0)

let offset_of_position text (line, character) =
  let length = String.length text in
  let rec find_line i current =
    if current = line then Some i
    else
      match String.index_from_opt text i '\n' with
      | None -> None
      | Some j -> find_line (j + 1) (current + 1)
  in
  match if line < 0 then None else find_line 0 0 with
  | None -> length
  | Some start ->
      let at_break i =
        i >= length
        || text.[i] = '\n'
        || (text.[i] = '\r' && i + 1 < length && text.[i + 1] = '\n')
      in
      let rec walk i units =
        if at_break i then i
        else
          let width, size = char_at text i in
          if units + size > character then i else walk (i + width) (units + size)
      in
      walk start 0
