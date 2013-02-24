package main

import (
	"fmt"
	"go/ast"
	"go/token"
)

var prefix string = "go:"

func (c *Compiler) emit(format string, params ...interface{}) {
	fmt.Fprintf(c.wr, format, params...)
}

func (c *Compiler) emitRaw(output string) {
	fmt.Fprint(c.wr, output)
}

func (c *Compiler) emitArrayType(node *ast.ArrayType) {
	if node.Len == nil {
		c.emit("(%sslice ", prefix)
	} else if _, ok := node.Len.(*ast.Ellipsis); ok {
		c.emit("(%sarray... ", prefix)
	} else {
		c.emit("(%sarray ", prefix)
		c.emitExpr(node.Len)
		c.emit(" ")
	}
	c.emitType(node.Elt)
	c.emit(")")
}

func (c *Compiler) emitAssignStmt(node *ast.AssignStmt) {
	c.emit("(%s%s ", prefix, goBinaryOpToSchemeOp(node.Tok.String()))
	sep := "("
	if len(node.Lhs) == 1 {
		switch a := node.Lhs[0].(type) {
		case *ast.Ident:
			c.emitIdent(a)
			goto right
		case *ast.SelectorExpr:
			c.emitSelectorExpr(a)
			goto right
		}
	}
	for _, expr := range node.Lhs {
		c.emit(sep)
		c.emitExpr(expr)
		sep = " "
	}
	c.emit(")")
right:
	for _, expr := range node.Rhs {
		c.emit(" ")
		c.emitExpr(expr)
	}
	c.emit(")")
}

func (c *Compiler) emitBasicLit(node *ast.BasicLit) {
	switch node.Kind {
	case token.CHAR:
		c.emit("#\\%s", goCharToSchemeChar(node))
	case token.STRING:
		// TODO newlines
		c.emitRaw(goStringToSchemeString(node))
	default:
		// avoid printf's (MISSING):
		c.emitRaw(goStringToSchemeString(node))
	}
}

func (c *Compiler) emitBinaryExpr(node *ast.BinaryExpr) {
	c.emit("(%s%s ", prefix, goBinaryOpToSchemeOp(node.Op.String()))
	c.emitExpr(node.X)
	c.emit(" ")
	c.emitExpr(node.Y)
	c.emit(")")
}

func (c *Compiler) emitBlockStmt(node *ast.BlockStmt) {
	if node.List == nil { return }
	for _, stmt := range node.List {
		c.emit(" ")
		c.emitStmt(stmt)
	}
}

func (c *Compiler) emitBranchStmt(node *ast.BranchStmt) {
	// (break), (continue), (goto label), (fallthrough)
	c.emit("(%s%s", prefix, node.Tok.String())
	if node.Label != nil {
		c.emit(" %s", node.Label.String())
	}
	c.emit(")")
}

func (c *Compiler) emitCallExpr(node *ast.CallExpr) {
	c.emit("(")
	if node.Ellipsis != 0 {
		c.emit("%sapply... ", prefix)
	}
	c.emitExpr(node.Fun)
	for _, arg := range node.Args {
		c.emit(" ")
		c.emitExpr(arg)
	}
	//if node.Ellipsis != 0 {
	//	c.emit(" ...")
	//}
	c.emit(")")
}

func (c *Compiler) emitCaseClause(node *ast.CaseClause, cond bool) {
	if node.List == nil || len(node.List) == 0 {
		c.emit("(%selse ", prefix)
		for _, stmt := range node.Body {
			c.emit(" ")
			c.emitStmt(stmt)
		}
		c.emit(")")
		return
	}
	c.emit("(")
	if cond {
		c.emitExpr(node.List[0])
	} else {
		sep := "("
		for _, expr := range node.List {
			c.emit(sep)
			c.emitExpr(expr)
			sep = " "
		}
		c.emit(")")
	} 
	c.emit(" ")
	for _, stmt := range node.Body {
		c.emit(" ")
		c.emitStmt(stmt)
	}
	c.emit(")")
}

// ChanDir

func (c *Compiler) emitChanType(node *ast.ChanType) {
	c.emit("(%schan", prefix)
	switch node.Dir {
	case ast.RECV:
		c.emit("<-")
	case ast.SEND:
		c.emit("<-!")
	}
	c.emit(" ")
	c.emitType(node.Value)
	c.emit(")")
}

func (c *Compiler) emitCommClause(node *ast.CommClause) {
	if node.Comm == nil {
		c.emit("(%selse", prefix)
	} else {
		c.emit("(")
		c.emitStmt(node.Comm)
	}
	if node.Body != nil {
		for _, stmt := range node.Body {
			c.emit(" ")
			c.emitStmt(stmt)
		}
	}
	c.emit(")")
}

func (c *Compiler) emitComment(node *ast.Comment) {
}

func (c *Compiler) emitCommentGroup(node *ast.CommentGroup) {
}

func (c *Compiler) emitCompositeLit(node *ast.CompositeLit) {
	c.emit("(%smake: ", prefix)
	c.emitType(node.Type)
	for _, arg := range node.Elts {
		c.emit(" ")
		c.emitExpr(arg)
	}
	c.emit(")")
}

func (c *Compiler) emitCompositePtr(node *ast.CompositeLit) {
	c.emit("(%snew: ", prefix)
	c.emitType(node.Type)
	for _, arg := range node.Elts {
		c.emit(" ")
		c.emitExpr(arg)
	}
	c.emit(")")
}

func (c *Compiler) emitDecl(node ast.Decl) {
	switch a := node.(type) {
	case *ast.GenDecl:
		c.emitGenDecl(a)
	case *ast.FuncDecl:
		c.emitFuncDecl(a)
	}
}

// DeclStmt

func (c *Compiler) emitDeferStmt(node *ast.DeferStmt) {
	c.emit("(%sdefer ", prefix)
	c.emitCallExpr(node.Call)
	c.emit(")")
}

func (c *Compiler) emitEmptyStmt(node *ast.EmptyStmt) {
	c.emit("#f")
}

func (c *Compiler) emitExpr(node ast.Expr) {
	if node == nil {

		return
	}
	switch a := node.(type) {
	case *ast.BasicLit:       c.emitBasicLit(a)
	case *ast.CompositeLit:   c.emitCompositeLit(a)
	case *ast.Ellipsis:       c.emitExpr(a.Elt)
	case *ast.FuncLit:        c.emitFuncLit(a)
	case *ast.Ident:          c.emitIdent(a)

	// Expr
	case *ast.BinaryExpr:     c.emitBinaryExpr(a)
	case *ast.CallExpr:       c.emitCallExpr(a)
	case *ast.IndexExpr:      c.emitIndexExpr(a)
	case *ast.KeyValueExpr:   c.emitKeyValueExpr(a)
	case *ast.ParenExpr:      c.emitExpr(a.X)
	case *ast.SelectorExpr:   c.emitSelectorExpr(a)
	case *ast.SliceExpr:      c.emitSliceExpr(a)
	case *ast.StarExpr:       c.emitStarExpr(a)
	case *ast.TypeAssertExpr: c.emitTypeAssertExpr(a)
	case *ast.UnaryExpr:      c.emitUnaryExpr(a)

	// Type
	case *ast.ArrayType:      c.emitArrayType(a)
	case *ast.ChanType:       c.emitChanType(a)
	case *ast.FuncType:       c.emitFuncType(a)
	case *ast.InterfaceType:  c.emitInterfaceType(a)
	case *ast.MapType:        c.emitMapType(a)
	case *ast.StructType:     c.emitStructType(a)

	default:
		c.emit("<expr:%v>", node)
	}
}

// ExprStmt

func (c *Compiler) emitField(node *ast.Field) {
	if len(node.Names) == 0 {
		c.emitType(node.Type)
		return
	}
	c.emit("#(")
	for _, name := range node.Names {
		c.emit("%s ", goIdToSchemeId(name.Name))
	}
	c.emitType(node.Type)
	c.emit(")")
}

// FieldFilter

func (c *Compiler) emitFieldList(node *ast.FieldList) {
	for _, field := range node.List {
		c.emit(" ")
		c.emitField(field)
	}
}

func (c *Compiler) emitFile(node *ast.File) {
	c.emit("(%spackage ", prefix)
	c.emitIdent(node.Name)
	//c.emit(" ")
	//c.emitImports(node.Imports) // this is in .Decls
	for _, decl := range node.Decls {
		c.emit(" ")
		c.emitDecl(decl)
	}
	c.emit(")")
}

func (c *Compiler) emitForStmt(node *ast.ForStmt) {
	if node.Init == nil && node.Post == nil {
		c.emit("(%swhile ", prefix)
		if node.Cond == nil {
			c.emit("#t")
		} else {
			c.emitExpr(node.Cond)
		}
		c.emit(" ")
		c.emitBlockStmt(node.Body)
		c.emit(")")
		return
	}

	c.emit("(%sfor ", prefix)
	if node.Init == nil {
		c.emit("#f")
	} else {
		c.emitStmt(node.Init)
	}
	c.emit(" ")
	if node.Cond == nil {
		c.emit("#t")
	} else {
		c.emitExpr(node.Cond)
	}
	c.emit(" ")
	if node.Post == nil {
		c.emit("#f")
	} else {
		c.emitStmt(node.Post)
	}
	c.emit(" ")
	c.emitBlockStmt(node.Body)
	c.emit(")")
}

func (c *Compiler) emitFuncDecl(node *ast.FuncDecl) {
	ellipsis := false
	if pars := node.Type.Params.List;
	   pars != nil && len(pars) > 0 {
		last := pars[len(pars) - 1]
		if _, ok := last.Type.(*ast.Ellipsis); ok {
			ellipsis = true
		}
	}
	// "(define-func (%s %s) %s)", name, type, body
	c.emit("(%sfunc", prefix)
	if ellipsis {
		c.emit("...")
	}
	if node.Recv != nil {
		c.emit(" ")
		c.emitFieldList(node.Recv)
	}
	c.emit(" ")
	c.emitIdent(node.Name)
	c.emit(" ")
	c.emitFuncTypes(node.Type, false)
	c.emit(" ")
	if node.Body != nil {
		c.emitBlockStmt(node.Body)
    }
	c.emit(")")
}

func (c *Compiler) emitFuncLit(node *ast.FuncLit) {
	c.emit("(%sfunc ", prefix)
	c.emitFuncTypes(node.Type, false)
	c.emit(" ")
	c.emitBlockStmt(node.Body)
	c.emit(")")
}

func (c *Compiler) emitFuncType(node *ast.FuncType) {
	c.emitFuncTypes(node, true)
}
func (c *Compiler) emitFuncTypes(node *ast.FuncType, external bool) {
	// It is the responsibility of the caller to
	// write "func " or whatever is appropriate
	// because we have no idea at the point if this
	// is being called from a Decl/Stmt/Expr, etc.
	if external {
		c.emit("(%sfunc ", prefix)
	}
	c.emit("(")
	c.emitFieldList(node.Params)
	c.emit(")")
	c.emitFuncResults(node.Results)
	if external {
		c.emit(")")
	}
}

func (c *Compiler) emitFuncResults(node *ast.FieldList) {
	c.emit(" ")
	if node == nil || len(node.List) == 0 {
		c.emit("%s&void", prefix)
		return
	}

	if len(node.List) == 1 {
		c.emitField(node.List[0])
		return
	}

	c.emit("(values")
	for _, field := range node.List {
		c.emit(" ")
		c.emitField(field)
	}
	c.emit(")")
}

//type emitter func (c *Compiler, node ast.Node)
//func (c *Compiler) emitList(nodes []ast.Node, emit emitter) {
//    sep := ""
//    for _, node := range nodes {
//        c.emit(sep)
//        emit(c, node)
//		sep = " "
//    }
//}

func (c *Compiler) emitGenDecl(node *ast.GenDecl) {
	if node.Tok == token.IMPORT {
		// "(import \"%s\")", path
		// "(import (as %s \"%s\"))", name, path
		// "(import (dot \"%s\"))", path
		c.emitImports(node.Specs)
		return
	}

	// otherwise
	c.emit("(%s%s", prefix, node.Tok.String())
	switch node.Tok {
	case token.TYPE:
		// "(define-type %s %s)", name, type
		for _, spec := range node.Specs {
			c.emit(" ")
			c.emitTypeSpec(spec.(*ast.TypeSpec))
		}
	case token.CONST:
		// "(define-const %s)", name
		// "(define-const (= %s %s))", name, value
		// "(define-const (= #(%s %s) %s))", name, type, value
		fallthrough
	case token.VAR:
		// "(define-var (= %s %s))", name, value
		// "(define-var (= (%s) %s))", name(s), value(s)
		// "(define-var (= #(%s %s) %s))", name(s), type, value(s)
		// "(define-var #(%s %s))", name(s), type
		for _, spec := range node.Specs {
			c.emit(" ")
			c.emitValueSpec(spec.(*ast.ValueSpec))
		}
	}
	c.emit(")")
}

func (c *Compiler) emitGoStmt(node *ast.GoStmt) {
	c.emit("(%sgo ", prefix)
	c.emitCallExpr(node.Call)
	c.emit(")")
}

func (c *Compiler) emitIdent(node *ast.Ident) {
	c.emitRaw(goIdToSchemeId(node.Name))
}

func (c *Compiler) emitIfStmt(node *ast.IfStmt) {
	unless := false
	if un, ok := node.Cond.(*ast.UnaryExpr); ok {
		if un.Op.String() == "!" {
			unless = true
		}
	}
	c.emit("(%s", prefix)
	if unless {
		c.emit("unless")
	} else {
		c.emit("when")
	}
	if node.Init != nil {
		c.emit("* ")
		c.emitStmt(node.Init)
	}
	c.emit(" ")
	if unless {
		c.emitExpr(node.Cond.(*ast.UnaryExpr).X)
	} else {
		c.emitExpr(node.Cond)
	}
	c.emit(" ")
	c.emitBlockStmt(node.Body)
	if node.Else != nil {
		c.emit("(%selse ", prefix)
		switch a := node.Else.(type) {
		case *ast.BlockStmt:
			c.emitBlockStmt(a)
		default:
			c.emitStmt(node.Else)
		}
		c.emit(")")
	}
	c.emit(")")
	// TODO: else
}

func (c *Compiler) emitImports(any interface{}) {
	c.emit("(%simport", prefix)
	switch specs := any.(type) {

	// from (*ast.GenDecl).Specs
	case []ast.Spec:
		for _, spec := range specs {
			c.emit(" ")
			c.emitImportSpec(spec.(*ast.ImportSpec))
		}

	// from (*ast.File).Imports
	case []*ast.ImportSpec:
		for _, spec := range specs {
			c.emit(" ")
			c.emitImportSpec(spec)
		}
	}
	c.emit(")")
}

func (c *Compiler) emitImportSpec(node *ast.ImportSpec) {
	if node.Name != nil {
		if node.Name.Name == "." {
			c.emit("(dot ")
		} else {
			c.emit("(as ")
			c.emitIdent(node.Name)
			c.emit(" ")
		}
		c.emitBasicLit(node.Path)
		c.emit(")")
		return
	}
	c.emitBasicLit(node.Path)
}

// Importer

func (c *Compiler) emitIncDecStmt(node *ast.IncDecStmt) {
	c.emit("(%s%s ", prefix, node.Tok.String())
	c.emitExpr(node.X)
	c.emit(")")
}

func (c *Compiler) emitIndexExpr(node *ast.IndexExpr) {
	c.emit("(%sindex ", prefix)
	c.emitExpr(node.X)
	c.emit(" ")
	c.emitExpr(node.Index)
	c.emit(")")
}

func (c *Compiler) emitInterfaceMethod(node *ast.Field) {
	if len(node.Names) == 0 {
		c.emitType(node.Type)
		return
	}
	if len(node.Names) != 1 {
		panic("expected single method name")
	}
	c.emit("(%sfunc ", prefix)
	c.emitIdent(node.Names[0])
	c.emit(" ")
	c.emitFuncTypes(node.Type.(*ast.FuncType), false)
	c.emit(")")
}

func (c *Compiler) emitInterfaceMethodList(node *ast.FieldList) {
	for _, field := range node.List {
		c.emit(" ")
		c.emitInterfaceMethod(field)
	}
}

func (c *Compiler) emitInterfaceType(node *ast.InterfaceType) {
	c.emit("(%sinterface ", prefix)
	c.emitInterfaceMethodList(node.Methods)
	c.emit(")")
}

func (c *Compiler) emitKeyValueExpr(node *ast.KeyValueExpr) {
	c.emit("(%s: ", prefix)
	if key, ok := node.Key.(*ast.BasicLit); ok {
		c.emitBasicLit(key)
		c.emit(" ")
	} else if key, ok := node.Key.(*ast.Ident); ok {
		c.emit("%s ", goIdToSchemeId(key.Name))
	} else {
		c.emitExpr(node.Key)
		c.emit(" ")
	}
	c.emitExpr(node.Value)
	c.emit(")")
	//return
	//if key, ok := node.Key.(*ast.BasicLit); ok {
	//	c.emit("(: %s ", key.Value)
	//	c.emitExpr(node.Value)
	//	c.emit(")")
	//	return
	//} else if key, ok := node.Key.(*ast.Ident); ok {
	//	c.emit("#:%s ", key.Name)
	//} else {
	//	panic("emitKeyValueExpr: expected string or identifier")
	//}
	//c.emitExpr(node.Value)
}

func (c *Compiler) emitLabeledStmt(node *ast.LabeledStmt) {
	c.emit("(%slabel %s ", prefix, node.Label.String())
	c.emitStmt(node.Stmt)
	c.emit(")")
}

func (c *Compiler) emitMapType(node *ast.MapType) {
	c.emit("(%smap: ", prefix)
	c.emitType(node.Key)
	c.emit(" ")
	c.emitType(node.Value)
	c.emit(")")
}

// MergeMode
// Node
// ObjKind
// Object
// Package
// ParenExpr

func (c *Compiler) emitRangeStmt(node *ast.RangeStmt) {
	c.emit("(%srange (%s ", prefix, node.Tok.String())
	if node.Value == nil {
		c.emitExpr(node.Key)
	} else {
		c.emit("(")
		c.emitExpr(node.Key)
		c.emit(" ")
		c.emitExpr(node.Value)
		c.emit(")")
	}
	c.emit(" ")
	c.emitExpr(node.X)
	c.emit(")")
	c.emitBlockStmt(node.Body)
	c.emit(")")
}

func (c *Compiler) emitReturnStmt(node *ast.ReturnStmt) {
	c.emit("(%sreturn", prefix)
	for _, arg := range node.Results {
		c.emit(" ")
		c.emitExpr(arg)
	}
	c.emit(")")
	
}

// Scope

func (c *Compiler) emitSelectStmt(node *ast.SelectStmt) {
	c.emit("(%scomm!", prefix)
	for _, stmt := range node.Body.List {
		c.emit(" ")
		c.emitCommClause(stmt.(*ast.CommClause))
	}
	c.emit(")")
}

func (c *Compiler) emitSelectorExpr(node *ast.SelectorExpr) {
	if id, ok := node.X.(*ast.Ident); ok {
		c.emit("%s.%s", goIdToSchemeId(id.Name), goIdToSchemeId(node.Sel.Name))
		return
	}
	c.emit("(%sdot ", prefix)
	c.emitExpr(node.X)
	c.emit(" %s)", goIdToSchemeId(node.Sel.Name))
}

func (c *Compiler) emitSendStmt(node *ast.SendStmt) {
	c.emit("(%s<-! ", prefix)
	c.emitExpr(node.Chan)
	c.emit(" ")
	c.emitExpr(node.Value)
	c.emit(")")
}

func (c *Compiler) emitSliceExpr(node *ast.SliceExpr) {
	c.emit("(%sindex ", prefix)
	c.emitExpr(node.X)
	c.emit(" ")
	if node.Low == nil {
		c.emit("#f")
	} else {
		c.emitExpr(node.Low)
	}
	c.emit(" ")
	if node.High == nil {
		c.emit("#f")
	} else {
		c.emitExpr(node.High)
	}
	c.emit(")")
}

// Spec

func (c *Compiler) emitStarExpr(node *ast.StarExpr) {
	//if id, ok := node.X.(*ast.Ident); ok {
	//	c.emit("*%s", id.Name)
	//	return
	//}
	//if sel, ok := node.X.(*ast.SelectorExpr); ok {
	//	c.emit("*")
	//	c.emitSelectorExpr(sel)
	//	return
	//}
	c.emit("(%sptr ", prefix)
	c.emitType(node.X)
	c.emit(")")
}

func (c *Compiler) emitStmt(node ast.Stmt) {
	switch a := node.(type) {

	// Stmt
	case *ast.AssignStmt:     c.emitAssignStmt(a)
	case *ast.BlockStmt:      c.emitBlockStmt(a)
	case *ast.BranchStmt:     c.emitBranchStmt(a)
	case *ast.DeclStmt:       c.emitDecl(a.Decl)
	case *ast.DeferStmt:      c.emitDeferStmt(a)
	case *ast.EmptyStmt:      c.emitEmptyStmt(a)
	case *ast.ExprStmt:       c.emitExpr(a.X)
	case *ast.ForStmt:        c.emitForStmt(a)
	case *ast.GoStmt:         c.emitGoStmt(a)
	case *ast.IfStmt:         c.emitIfStmt(a)
	case *ast.IncDecStmt:     c.emitIncDecStmt(a)
	case *ast.LabeledStmt:    c.emitLabeledStmt(a)
	case *ast.RangeStmt:      c.emitRangeStmt(a)
	case *ast.ReturnStmt:     c.emitReturnStmt(a)
	case *ast.SelectStmt:     c.emitSelectStmt(a)
	case *ast.SendStmt:       c.emitSendStmt(a)
	case *ast.SwitchStmt:     c.emitSwitchStmt(a)
	case *ast.TypeSwitchStmt: c.emitTypeSwitchStmt(a)

	default:
		c.emit("<stmt:%v>", node)
	}
}

func (c *Compiler) emitStructType(node *ast.StructType) {
	c.emit("(%sstruct", prefix)
	c.emitFieldList(node.Fields)
	c.emit(")")
}

func (c *Compiler) emitSwitchStmt(node *ast.SwitchStmt) {
	cond := node.Tag == nil
	if cond {
		c.emit("(%scond!", prefix)
	} else {
		c.emit("(%scase!", prefix)
	}
	if node.Init != nil {
		c.emit("* ")
		c.emitStmt(node.Init)
	}
	if !cond {
		c.emit(" ")
		c.emitExpr(node.Tag)
	}
	for _, stmt := range node.Body.List {
		c.emit(" ")
		c.emitCaseClause(stmt.(*ast.CaseClause), cond)
	}
	c.emit(")")
}

func (c *Compiler) emitType(node ast.Expr) {
	if id, ok := node.(*ast.Ident); ok {
		c.emitRaw(goIdToSchemeId(id.Name))
		//c.emit(id)
		return
	}
	c.emitExpr(node)
}

func (c *Compiler) emitTypeAssertExpr(node *ast.TypeAssertExpr) {
	c.emit("(%sas ", prefix)
	c.emitExpr(node.X)
	c.emit(" ")
	if node.Type == nil {
		c.emit("type")
	} else {
		c.emitType(node.Type)
	}
	c.emit(")")
}

func (c *Compiler) emitTypeSpec(node *ast.TypeSpec) {
	c.emitIdent(node.Name)
	c.emit(" ")
	c.emitType(node.Type)
}

func (c *Compiler) emitTypeSwitchStmt(node *ast.TypeSwitchStmt) {
	c.emit("(%stype!", prefix)
	if node.Init != nil {
		c.emit("* ")
		c.emitStmt(node.Init)
	}
	c.emit(" ")
	c.emitStmt(node.Assign)
	for _, stmt := range node.Body.List {
		c.emit(" ")
		c.emitCaseClause(stmt.(*ast.CaseClause), false)
	}
	c.emit(")")
}

func (c *Compiler) emitUnaryExpr(node *ast.UnaryExpr) {
	if node.Op.String() == "&" {
		if lit, ok := node.X.(*ast.CompositeLit); ok {
			c.emitCompositePtr(lit)
			return
		}
	}
	c.emit("(%s%s ", prefix, goUnaryOpToSchemeOp(node.Op.String()))
	c.emitExpr(node.X)
	c.emit(")")
}

// helper function
func (c *Compiler) emitValueNames(ids []*ast.Ident) {
	buffer := []byte{}
	for _, id := range ids {
		buffer = append(buffer, ' ')
		buffer = append(buffer, goIdToSchemeId(id.Name)...)
	}
	if len(ids) == 1 {
		c.emit("%s", string(buffer[1:]))
	} else {
		c.emit("(%s)", string(buffer[1:]))
	}
}

func (c *Compiler) emitValueTypedNames(ids []*ast.Ident, t ast.Expr) {
	buffer := []byte{}
	for _, id := range ids {
		buffer = append(buffer, goIdToSchemeId(id.Name)...)
		buffer = append(buffer, ' ')
	}
	c.emit("#(%s", string(buffer))
	c.emitType(t)
	c.emit(")")
}

func (c *Compiler) emitValueSpec(node *ast.ValueSpec) {
	if node.Type != nil {
		if node.Values != nil {
			// "(define-const (= #(%s %s) %s))", name, type, value
			// "(define-var (= #(%s %s) %s))", name(s), type, value(s)
			c.emit("(%s= ", prefix)
			c.emitValueTypedNames(node.Names, node.Type)
			for _, arg := range node.Values {
				c.emit(" ")
				c.emitExpr(arg)
			}
			c.emit(")")
		} else {
			// "(define-var #(%s %s))", name(s), type
			c.emitValueTypedNames(node.Names, node.Type)
		}
	} else if node.Values != nil {
		// "(define-const (= %s %s))", name, value
		// "(define-var (= %s %s))", name, value
		// "(define-var (= (%s) %s))", name(s), value(s)
		c.emit("(%s= ", prefix)
		c.emitValueNames(node.Names)
		for _, arg := range node.Values {
			c.emit(" ")
			c.emitExpr(arg)
		}
		c.emit(")")
	} else {
		// "(define-const %s)", name
		c.emitValueNames(node.Names)
	}
}

// Visitor