    Stmt_ConstDecl(Type_Bits(2),"ftype",Expr_Slices(Expr_Var("enc"),[Slice_LoWd(22,2)]))

    Stmt_ConstDecl(Type_Bits(8),"imm8",Expr_Slices(Expr_Var("enc"),[Slice_LoWd(13,8)]))

    Stmt_ConstDecl(Type_Bits(5),"Rd",Expr_Slices(Expr_Var("enc"),[Slice_LoWd(0,5)]))

    Stmt_VarDecl(Type_Constructor("integer"),"d",Expr_TApply("UInt.1",[5],[Expr_Var("Rd")]))

    Stmt_VarDeclsNoInit(Type_Constructor("integer"),["datasize"])

    Stmt_Case(Expr_Var("ftype"),[(Alt_Alt([(Pat_LitBits("00"))],None,[
Stmt_Assign(LExpr_Var("datasize"),32)
]));(Alt_Alt([(Pat_LitBits("01"))],None,[
Stmt_Assign(LExpr_Var("datasize"),64)
]));(Alt_Alt([(Pat_LitBits("10"))],None,[
Stmt_Dep_Undefined
]));(Alt_Alt([(Pat_LitBits("11"))],None,[
Stmt_If(Expr_TApply("HaveFP16Ext.0",[],[]),[
Stmt_Assign(LExpr_Var("datasize"),16)
],[],[
Stmt_Dep_Undefined
])
]))],None)

    Stmt_VarDecl(Type_Bits(Expr_Var("datasize")),"imm",Expr_TApply("VFPExpandImm.0",[Expr_Var("datasize")],[Expr_Var("imm8")]))

    Stmt_TCall("CheckFPAdvSIMDEnabled64.0",[],[])

    Stmt_Assign(LExpr_Write("V.set.0",[(Expr_Var("datasize"))],[(Expr_Var("d"))]),Expr_Var("imm"))
