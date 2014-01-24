
{
  open RiscvParser
  open Printf
  open Ustring.Op
  open Info
  exception Lex_error of Msg.message
 
let reserved_strings = [
  (* Keywords *)
  ("j",             fun i -> Parser.J{i=i;v=()}); 
  ("J",             fun i -> Parser.J{i=i;v=()}); 
  ("jal",           fun i -> Parser.JAL{i=i;v=()}); 
  ("JAL",           fun i -> Parser.JAL{i=i;v=()}); 
  ("beq",           fun i -> Parser.beq{i=i;v=()}); 
  ("BEQ",           fun i -> Parser.BEQ{i=i;v=()}); 
  
  (* Symbolic Tokens *)
  ("=",             fun i -> Parser.EQ{i=i;v=()});
  ("(",             fun i -> Parser.LPAREN{i=i;v=()});
  (")",             fun i -> Parser.RPAREN{i=i;v=()});
  (".",             fun i -> Parser.DOT{i=i;v=()});
  (",",             fun i -> Parser.COMMA{i=i;v=()});
]


(* Info handling *)
let tabsize = ref 8
let filename = ref (us"")
let rowno = ref 1
let colno = ref 0
let last_info = ref NoInfo
let utf8strlen s = Ustring.length (Ustring.from_utf8 s)
let newrow() =
  incr rowno;
  colno := 0
(* Updates both columns and rows in a safe way *)
let count_ustring s = 
  rowno := !rowno + (Ustring.count s (uc '\n'));
  colno := try Ustring.length s - Ustring.rindex s (uc '\n') - 1
	     with Not_found -> !colno + Ustring.length s
let count_utf8 s = count_ustring (Ustring.from_utf8 s)
let colcount_fast s = colno := !colno + (String.length s)
let colcount_utf8 s = colno := !colno + (utf8strlen s)
let add_colno i = colno := !colno + i
let mkinfo_fast s = 
  last_info := Info(!filename,!rowno,!colno,!rowno,!colno+(String.length s));
  colcount_fast s; !last_info
let mkinfo_utf8_fast s = 
  last_info := Info(!filename,!rowno,!colno,!rowno,!colno + (utf8strlen s));
  colcount_utf8 s; !last_info
(* mkinfo_ustring also counts newlines correctly in string [s] *)
let mkinfo_ustring s =  
  let row = !rowno in 
  let col = !colno in
  count_ustring s;
  last_info := Info(!filename,row,col,!rowno,!colno);
  !last_info

(* Init the lexer with file name and tab-size. *)
let init file_name tab_size =
  filename := file_name;
  rowno := 1;
  colno := 0;
  tabsize := tab_size 

(* Handle identifiers, keywords, and operators *)
type buildfun = info -> Parser.token
let (str_tab : (string,buildfun) Hashtbl.t) = 
  Hashtbl.create 1024
let _ = List.iter (fun (str,f) -> Hashtbl.add str_tab str f) 
  reserved_strings

(* Make identfier, keyword, or operator  *)
let mkid s =
  try 
    let f = Hashtbl.find str_tab s in f (mkinfo_fast s)    
  with Not_found ->   
    let s2 = Ustring.from_utf8 s in
    Parser.IDENT {i=mkinfo_ustring s2; v=s2}

(* String handling *)
let string_buf = Buffer.create 80

(* Parse error message *)
let parse_error_message() =
  (PARSE_ERROR,ERROR,!last_info,[])


}

let utf8_1byte = ['\x00'-'\x7F']
let utf8_2byte = ['\xC0'-'\xDF'] ['\x80'-'\xBF']
let utf8_3byte = ['\xE0'-'\xEF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
let utf8_4byte = ['\xF0'-'\xF7'] ['\x80'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']

let ascii = utf8_1byte
let noascii =  utf8_2byte | utf8_3byte | utf8_4byte 
let utf8 = ascii | noascii
let us_letter = ['A'-'Z'] | ['a'-'z'] 
let newline = ('\013' | '\010' | "\013\010")
let whitespace = (' '| '\012') 
let tab = '\t'
let digit = ['0'-'9']
let s_escape = "\\'" | "\\\"" | "\\?"  | "\\\\" |
               "\\a"  | "\\b" | "\\f"  | "\\n" | "\\r" | "\\t" | "\\v"
let nondigit = ('_' | us_letter)
let ident = (nondigit (digit | nondigit)*)
let symtok =  "=" | "(" | ")" | "." | ","
let line_comment = ("//" | ";") [^ '\013' '\010']*  

(* Main lexing *)
rule main = parse
  | whitespace+ as s
      { colcount_fast s; main lexbuf }
  | line_comment 
      { main lexbuf }
  | "/*" as s 
      { Buffer.reset string_buf ;  
	Buffer.add_string string_buf s; section_comment lexbuf; 
	count_utf8 (Buffer.contents string_buf);
	main lexbuf}
  | tab 
      { add_colno !tabsize; main lexbuf }
  | newline
      { newrow(); main lexbuf }
  | ident | symtok as s
      { mkid s }
  | eof
      { Parser.EOF }
  | utf8 as c
      { let s = Ustring.from_utf8 c in
	raise (Lex_error (LEX_UNKNOWN_CHAR,ERROR,mkinfo_utf8_fast c,[s])) }


(* Section comment *)
and section_comment = parse
  | "*/" as s
      { Buffer.add_string string_buf s }
  | eof
      { let s = Ustring.from_utf8 ("/*" ^ (Buffer.contents string_buf)) in
	raise (Lex_error (LEX_COMMENT_NOT_TERMINATED,ERROR, 
	 	 mkinfo_ustring s, [s])) }
  | _ as c
      { Buffer.add_char string_buf c; section_comment lexbuf }


 

