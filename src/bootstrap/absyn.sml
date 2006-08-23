(* This structure defines the format of the abstact syntax tree. *)
structure Absyn = struct
   type pos = int * int

   datatype ExnHandler = ExnHandler of {sym: Symbol.symbol, id: UniChar.Data,
                                        expr: Expr, symtab: Symbol.symtab,
                                        ty: Types.Type, pos: pos}

   (* Allow type constructors to appear as the branch of a case expression,
    * with value bindings for the elements in the constructor.
    *)
   and Branch = RegularBranch of Expr
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
                             values: {sym: Symbol.symbol, expr: Expr} list}
                | ExprLstExp of Expr list
                | FunCallExp of {function: Symbol.symbol, args: Expr list,
                                 frees: Symbol.symbol list}
                | IdExp of Symbol.symbol
                | IfExp of {test: Expr, then': Expr, else': Expr}
                | IntegerExp of int
                | RaiseExp of Expr
                | RecordAssnExp of (Symbol.symbol * Expr) list
                | RecordRefExp of {record: Expr, ele: Symbol.symbol}
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
end
