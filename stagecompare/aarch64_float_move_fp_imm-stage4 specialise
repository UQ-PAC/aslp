    Stmt_ConstDecl(Type_Bits(2),"ftype",Expr_Slices(Expr_Var("enc"),[Slice_LoWd(22,2)]))

    Stmt_ConstDecl(Type_Bits(8),"imm8",Expr_Slices(Expr_Var("enc"),[Slice_LoWd(13,8)]))

    Stmt_ConstDecl(Type_Bits(5),"Rd",Expr_Slices(Expr_Var("enc"),[Slice_LoWd(0,5)]))

    Stmt_VarDecl(Type_Constructor("integer"),"d",Expr_TApply("UInt.1",[5],[Expr_Var("Rd")]))

    Stmt_VarDeclsNoInit(Type_Constructor("integer"),["datasize"])

    Stmt_If(Expr_In(Expr_Var("ftype"),Pat_Set([(Pat_LitBits("00"))])),[
Stmt_Assign(LExpr_Var("datasize"),32)
],[],[
Stmt_If(Expr_In(Expr_Var("ftype"),Pat_Set([(Pat_LitBits("01"))])),[
Stmt_Assign(LExpr_Var("datasize"),64)
],[],[
Stmt_If(Expr_In(Expr_Var("ftype"),Pat_Set([(Pat_LitBits("10"))])),[
Stmt_Throw("UNSUPPORTED")
],[],[
Stmt_If(Expr_In(Expr_Var("ftype"),Pat_Set([(Pat_LitBits("11"))])),[
Stmt_If(Expr_TApply("HaveFP16Ext.0",[],[]),[
Stmt_Assign(LExpr_Var("datasize"),16)
],[],[
Stmt_Throw("UNSUPPORTED")
])
],[],[
Stmt_Throw("UNREACHABLE")
])
])
])
])

    Stmt_VarDecl(Type_Bits(Expr_Var("datasize")),"imm",Expr_TApply("VFPExpandImm.0",[Expr_Var("datasize")],[Expr_Var("imm8")]))

    Stmt_TCall("CheckFPAdvSIMDEnabled64.0",[],[])

    Stmt_Assign(LExpr_Write("V.set.0",[(Expr_Var("datasize"))],[(Expr_Var("d"))]),Expr_Var("imm"))
