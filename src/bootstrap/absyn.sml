(* This structure defines the format of the abstact syntax tree. *)
structure Absyn = struct
   type pos = int * int

   (* Exception handler expression.  sym is optional in the default case,
    * where we handle all types of exceptions and therefore aren't given
    * an exception type.
    *)
   datatype ExnHandler = ExnHandler of {sym: Symbol.symbol option,
                                        id: UniChar.Data, expr: Expr,
                                        symtab: Symbol.symtab, ty: Types.Type,
                                        pos: pos}

   (* Allow type constructors to appear as the branch of a case expression,
    * with value bindings for the elements in the constructor.
    *)
   and Branch = RegularBranch of BaseExpr
              | UnionBranch of Symbol.symbol * Symbol.symbol list

   (* Wrap the basic expression type in things every expression has -
    * a position, a type, and a possible exception handler.
    *)
   and Expr = Expr of {expr: BaseExpr, pos: pos, ty: Types.Type,
                       exnHandler: {handlers: ExnHandler list,
                                    default: ExnHandler option,
                                    ty: Types.Type, pos: pos} option}

   and BaseExpr = BooleanExp of bool
                | BottomExp
                | CaseExp of {test: Expr, default: Expr option,
                              branches: (Branch * Expr) list}
                | DeclExp of {decls: Decl list, expr: Expr,
                              symtab: Symbol.symtab}
                | ExnExp of {sym: Symbol.symbol, ty: Types.Type,
                             values: (Symbol.symbol * Expr) list}
                | ExprLstExp of Expr list
                | FunCallExp of {function: Symbol.symbol, args: Expr list,
                                 tyArgs: Ty list, frees: Symbol.symbol list}
                | IdExp of Symbol.symbol
                | IfExp of {test: Expr, then': Expr, else': Expr}
                | IntegerExp of int
                | RaiseExp of Expr
                | RecordAssnExp of (Symbol.symbol * Expr) list
                | RecordRefExp of {record: BaseExpr, ele: Symbol.symbol}
                | StringExp of UniChar.Data

   and Ty = BottomTy of pos
          | ExnTy of {exn': (Symbol.symbol * Ty * pos) list, pos: pos}
          | IdTy of {sym: Symbol.symbol, pos: pos}
          | ListTy of {lst: Ty, pos: pos}
          | RecordTy of {record: (Symbol.symbol * Ty * pos) list, pos: pos}
          | UnionTy of {tycons: (Symbol.symbol * Ty option * pos) list, pos: pos}

   and Decl = Absorb of {module: Symbol.symbol, pos: pos}
              (* Each element of calls must be a FunCallExp. *)
            | FunDecl of {sym: Symbol.symbol, retval: Ty option, pos: pos,
                          formals: (Symbol.symbol * Ty * pos) list,
                          tyFormals: Symbol.symbol list,
                          calls: Expr list, body: Expr, symtab: Symbol.symtab}
            | ModuleDecl of {sym: Symbol.symbol, decl: Decl list, pos: pos,
                             symtab: Symbol.symtab}
            | TyDecl of {sym: Symbol.symbol, ty: Types.Type, absynTy: Ty,
                         pos: pos}
            | ValDecl of {sym: Symbol.symbol, ty: Types.Type,
                          absynTy: Ty option, init: Expr, pos: pos}

   fun write stream hdr ast = let
      fun say str = TextIO.output (stream, str)
      fun sayln str = TextIO.output (stream, str ^ "\n")

      fun indent 0 = ()
        | indent i = ( say "   " ; indent (i-1) )

      (* Surround the output of the function f with braces, printing a header
       * the start as well.  f should be a curried function whose last param is
       * the indentation level.
       *)
      fun << i hdr f = ( indent i ; sayln (hdr ^ " = { ") ; f (i+1) )

      (* Print a list by calling the function f over each element of lst.  f
       * should be a curried function whose last parameter is the indentation
       * level.  In the typical use case, printLst will be passed into << as
       * its function, and << will pass the indentation level.
       *)
      fun printLst lst f i = app (f i) lst

      fun writeIdLst lst i = let
         fun writeOne i (sym, ty, _) =
            (indent i ; sayln ("symbol = " ^ Symbol.toString sym) ;
             indent i ; sayln "ty = " ; writeTy (i+1) ty ;
             indent i ; say ",")
      in
         << i "id_lst" (printLst lst writeOne)
      end

      and writeOptionalIdLst lst i = let
         fun writeOne i (sym, ty, _) = let
            fun optTy i (SOME ty) = (indent i ; sayln "ty = " ; writeTy (i+1) ty)
              | optTy i _         = ()
         in
            indent i ; sayln ("symbol = " ^ Symbol.toString sym) ;
            optTy i ty ;
            indent i ; say ","
         end
      in
         << i "id_lst" (printLst lst writeOne)
      end

      and writeExnHandler i (ExnHandler{sym, id, expr, ...}) =
         (indent i ; sayln "exn_handler = {" ;
          case sym of SOME v => (indent (i+1) ; sayln ("sym = " ^ Symbol.toString v))
                    | NONE => ();
          indent (i+1) ; sayln ("id = " ^ UniChar.Data2String id) ;
          indent (i+1) ; sayln "expr =" ; writeExpr (i+2) expr ;
          indent i ; sayln "}")

      and writeBranch i (RegularBranch expr) = writeBaseExpr i expr

      and writeExpr i (Expr{expr, exnHandler, ...}) = let
         fun writeExnHandlers i {handlers, default, ty, pos} = let
            fun optHandler i (SOME h) = << i "default" (fn i => writeExnHandler i h)
              | optHandler i _        = ()
         in
            << i "exn_handler" (fn i => (<< (i+1) "handlers"
                                            (printLst handlers writeExnHandler) ;
                                         optHandler (i+1) default))
         end
      in
         indent i ; sayln "expr = " ; writeBaseExpr (i+1) expr ;
         case exnHandler of SOME v => writeExnHandlers (i+1) v | NONE => ()
      end

      and writeBaseExpr i (BooleanExp v) =
             (indent i ; sayln ("BOOLEAN(" ^ Bool.toString v ^ ")"))
        | writeBaseExpr i BottomExp =
             (indent i ; sayln "BOTTOM")
        | writeBaseExpr i (CaseExp{test, default, branches}) =
             (indent i ; sayln "case = {" ;
              indent (i+1) ; sayln "test =" ; writeExpr (i+2) test ;
(*            indent (i+1) ; sayln "branches = [" ; writeBranchLst (i+2) branches ; indent (i+1) ; sayln "]" *)
              case default of SOME v => (indent (i+1) ; say "default =" ; writeExpr (i+2) v)
                            | NONE => () ;
              indent i ; sayln "}")
        | writeBaseExpr i (IdExp v) =
             (indent i ; sayln ("id = " ^ Symbol.toString v))
        | writeBaseExpr i (IfExp{test, then', else'}) =
             (indent i ; sayln "if = {" ;
              indent (i+1) ; sayln "test = " ; writeExpr (i+2) test ;
              indent (i+1) ; sayln "then = " ; writeExpr (i+2) then' ;
              indent (i+1) ; sayln "else = " ; writeExpr (i+2) else' ;
              indent i ; sayln "}")
        | writeBaseExpr i (IntegerExp v) =
             (indent i ; sayln ("INTEGER(" ^ Int.toString v ^ ")"))
        | writeBaseExpr i (RaiseExp expr) =
             (indent (i+1) ; sayln "raise_expr = "; writeExpr (i+2) expr)
        | writeBaseExpr i (RecordRefExp{record, ele}) =
             (indent i ; sayln "record_expr = {" ;
              indent (i+1) ; sayln "record =" ; writeBaseExpr (i+2) record ;
              indent (i+1) ; sayln ("ele = " ^ Symbol.toString ele))
        | writeBaseExpr i (StringExp v) =
             (indent i ; sayln ("STRING(" ^ UniChar.Data2String v ^ ")"))

      and writeTy i (BottomTy _) = (indent i ; sayln "ty = BOTTOM")
        | writeTy i (ExnTy{exn', ...}) = (indent i ; sayln "ty = EXN" ; writeIdLst exn' (i+1))
        | writeTy i (IdTy{sym, ...}) = (indent i ; sayln ("ty = " ^ Symbol.toString sym))
        | writeTy i (ListTy{lst, ...}) = (indent i ; sayln "ty = LIST " ; writeTy (i+1) lst)
        | writeTy i (RecordTy{record, ...}) = << i "ty = RECORD " (writeIdLst record)
        | writeTy i (UnionTy{tycons, ...}) = << i "ty = UNION" (writeOptionalIdLst tycons)

      and writeDecl i (Absorb{module, ...}) =
             (indent i ; sayln ("absorb = " ^ Symbol.toString module))
        | writeDecl i (TyDecl{sym, absynTy, ...}) =
             (indent i ; sayln "ty_decl = {" ;
              indent (i+1) ; sayln ("symbol = " ^ Symbol.toString sym) ;
              indent (i+1) ; sayln "ty =" ; writeTy (i+2) absynTy ;
              indent i ; sayln "}")
        | writeDecl i (ValDecl{sym, absynTy, init, ...}) =
             (indent i ; sayln "val_decl = {" ;
              indent (i+1) ; sayln ("symbol = " ^ Symbol.toString sym) ;
              case absynTy of SOME(ty) => (indent (i+1) ; sayln "ty =" ; writeTy (i+2) ty)
                            | NONE => () ;
              indent (i+1) ; sayln "init =" ; writeExpr (i+2) init)
   in
      sayln ("\n" ^ hdr ^ "\n========================================") ; writeTy 0 ast
   end
end
