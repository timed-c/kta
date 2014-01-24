%{ 

  open Ustring.Op
  open Info
  open AsmSyntax

  (** Create a new info, taking left and right part *)
  let mkinfo fi1 fi2 =  
    match (fi1,fi2) with
      | (Info(fn,r1,c1,_,_), Info(_,_,_,r2,c2)) -> Info(fn,r1,c1,r2,c2)
      | (Info(fn,r1,c1,r2,c2), NoInfo) -> Info(fn,r1,c1,r2,c2)
      | (NoInfo, Info(fn,r1,c1,r2,c2)) -> Info(fn,r1,c1,r2,c2)
      | (_,_) -> NoInfo

%}

/* Misc tokens */
%token EOF
%token <Ustring.ustring tokendata> IDENT

/* Keywords */
%token <unit tokendata> J      
%token <unit tokendata> JAL      
%token <unit tokendata> BEQ      

/* Symbolic Tokens */
%token <unit tokendata> EQ            /* "="  */
%token <unit tokendata> LPAREN        /* "("  */
%token <unit tokendata> RPAREN        /* ")"  */
%token <unit tokendata> DOT           /* "."  */
%token <unit tokendata> COMMA         /* ","  */


%start instruction  
%type <pinst> instruction

%%


instruction:
  | inst EOF
      { $1 }
 
inst:
  | JAL IDENT 
      { PInst(IAbsJmp(mkinfo $1.i $2.i, OpJAL,0), $2.v)) }


/*
term:
  | app_left 
      { $1 }
  | LAM IDENT DOT term
      { let fi = mkinfo $1.i (tm_info $4) in
        TmLam(fi,$2.v,$4) }
  
app_left:
  | atom
      { $1 }
  | app_left atom
      { let fi = mkinfo (tm_info $1) (tm_info $2) in
        TmApp(fi,$1,$2) }

atom:
  | IDENT
      { TmVar($1.i,$1.v) }
  | LPAREN term RPAREN
      { $2 }

  */    
     









      

