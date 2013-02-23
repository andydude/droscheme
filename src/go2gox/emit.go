package main

import (
	"fmt"
	"go/ast"
	"go/token"
)

func (c *Compiler) emit(format string, params ...interface{}) {
	fmt.Fprintf(c.wr, format, params...)
}

func (c *Compiler) emitRaw(output string) {
	fmt.Fprint(c.wr, output)
}

func (c *Compiler) emitArrayType(node *ast.ArrayType) {
	if node.Len == nil {
		c.emit("<go:type><go:slice/> ")
	} else if _, ok := node.Len.(*ast.Ellipsis); ok {
		c.emit("<go:array-ellipsis/>")
	} else {
		c.emit("<go:array/>")
		c.emitExpr(node.Len)
		c.emit(" ")
	}
	c.emitType(node.Elt)
	c.emit("</go:type>")
}

func (c *Compiler) emitAssignStmt(node *ast.AssignStmt) {
	c.emit("<m:apply>%s ", goBinaryOpToXmlOp(node.Tok.String()))
	sep := "<go:values>"
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
	c.emit("</go:values>")
right:
	for _, expr := range node.Rhs {
		c.emit(" ")
		c.emitExpr(expr)
	}
	c.emit("</m:apply>")
}

func (c *Compiler) emitBasicLit(node *ast.BasicLit) {
	switch node.Kind {
	case token.CHAR:
		c.emit("<m:cs type=\"char\">%s</m:cs>", goCharToXmlChar(node))
	case token.STRING:
		// TODO newlines
		c.emit("<m:cs>%s</m:cs>", goStringToXmlString(node))
	default:
		// avoid printf's (MISSING):
		c.emit("<m:cn>%s</m:cn>", goStringToXmlString(node))
	}
}

func (c *Compiler) emitBasicLitWithAttr(node *ast.BasicLit, attrName string, attrValue string) {
	switch node.Kind {
	case token.STRING:
		// TODO newlines
		c.emit("<m:cs %s=\"%s\">%s</m:cs>", attrName, attrValue, goStringToXmlString(node))
	default:
		// avoid printf's (MISSING):
		c.emitRaw(goStringToXmlString(node))
	}
}

func (c *Compiler) emitBinaryExpr(node *ast.BinaryExpr) {
	c.emit("<m:apply>%s ", goBinaryOpToXmlOp(node.Op.String()))
	c.emitExpr(node.X)
	c.emit(" ")
	c.emitExpr(node.Y)
	c.emit("</m:apply>")
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
	c.emit("<m:apply><go:%s/>", node.Tok.String())
	if node.Label != nil {
		c.emit(" %s", node.Label.String())
	}
	c.emit("</m:apply>")
}

func (c *Compiler) emitCallExpr(node *ast.CallExpr) {
	if node.Ellipsis != 0 {
		c.emit("<m:apply rest='*'>")
	} else {
		c.emit("<m:apply>")
	}
	c.emitExpr(node.Fun)
	for _, arg := range node.Args {
		c.emit(" ")
		c.emitExpr(arg)
	}
	//if node.Ellipsis != 0 {
	//	c.emit(" ...")
	//}
	c.emit("</m:apply>")
}

func (c *Compiler) emitCaseClause(node *ast.CaseClause, cond bool) {
	if node.List == nil || len(node.List) == 0 {
		c.emit("<go:else>")
		for _, stmt := range node.Body {
			c.emit(" ")
			c.emitStmt(stmt)
		}
		c.emit("</go:else>")
		return
	}
	c.emit("<go:di>")
	if cond {
		c.emitExpr(node.List[0])
	} else {
		sep := "<go:dt>"
		for _, expr := range node.List {
			c.emit(sep)
			c.emitExpr(expr)
			sep = " "
		}
		c.emit("</go:dt>")
	} 
	c.emit(" ")
	for _, stmt := range node.Body {
		c.emit(" ")
		c.emitStmt(stmt)
	}
	c.emit("</go:di>")
}

// ChanDir

func (c *Compiler) emitChanType(node *ast.ChanType) {
	c.emit("(chan")
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
		c.emit("<go:else>")
	} else {
		c.emit("<go:di>")
		c.emitStmt(node.Comm)
	}
	if node.Body != nil {
		for _, stmt := range node.Body {
			c.emit(" ")
			c.emitStmt(stmt)
		}
	}
	if node.Comm == nil {
		c.emit("</go:else>")
	} else {
		c.emit("</go:di>")
	}
}

func (c *Compiler) emitComment(node *ast.Comment) {
}

func (c *Compiler) emitCommentGroup(node *ast.CommentGroup) {
}

func (c *Compiler) emitCompositeLit(node *ast.CompositeLit) {
	c.emit("#(")
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
	c.emit("(defer ")
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
	for _, name := range node.Names {
		c.emit("<go:field>")
		c.emitIdent(name)
		c.emitType(node.Type)
		c.emit("</go:field>")
	}
}

// should be == emitField
func (c *Compiler) emitFuncParam(node *ast.Field) {
	if len(node.Names) == 0 {
		c.emitType(node.Type)
		return
	}
	for _, name := range node.Names {
		c.emit("<m:bvar><m:ci>")
		c.emit(goIdToXmlId(name.Name))
		c.emit("<m:sep/>")
		c.emitType(node.Type)
		c.emit("</m:ci></m:bvar>")
	}
}

// should be == emitFieldList
func (c *Compiler) emitFuncParamList(node *ast.FieldList) {
	for _, field := range node.List {
		c.emit(" ")
		c.emitFuncParam(field)
	}
}

// FieldFilter

func (c *Compiler) emitFieldList(node *ast.FieldList) {
	for _, field := range node.List {
		c.emit(" ")
		c.emitField(field)
	}
}

func (c *Compiler) emitFile(node *ast.File) {
	c.emit("<go:package")
	c.emit(" xmlns:go='http://drosoft.org/ns/drosera/goxml#'")
	c.emit(" xmlns:m='http://www.w3.org/1998/Math/MathML'")
	c.emit(" xmlns:xsd='http://www.w3.org/2001/XMLSchema-datatypes'")
	c.emit(">")
	c.emitIdent(node.Name)
	for _, decl := range node.Decls {
		c.emit(" ")
		c.emitDecl(decl)
	}
	c.emit("</go:package>")
}

func (c *Compiler) emitForStmt(node *ast.ForStmt) {
	if node.Init == nil && node.Post == nil {
		c.emit("<m:apply><go:while/>")
		if node.Cond == nil {
			c.emit("#t")
		} else {
			c.emitExpr(node.Cond)
		}
		c.emit(" ")
		c.emitBlockStmt(node.Body)
		c.emit("</m:apply>")
		return
	}

	c.emit("<m:apply><go:for/> ")
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
	c.emit("</m:apply>")
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
	c.emit("<go:function")
	if ellipsis {
		c.emit(" with='rest'")
	}
	c.emit(">")
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
	c.emit("</go:function>")
}

func (c *Compiler) emitFuncLit(node *ast.FuncLit) {
	c.emit("<go:function>")
	c.emitFuncTypes(node.Type, false)
	c.emit(" ")
	c.emitBlockStmt(node.Body)
	c.emit("</go:function>")
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
		c.emit("<go:function>")
	}
	c.emitFuncParamList(node.Params)
	c.emitFuncResults(node.Results)
	if external {
		c.emit("</go:function>")
	}
}

func (c *Compiler) emitFuncResults(node *ast.FieldList) {
	c.emit(" ")
	if node == nil || len(node.List) == 0 {
		c.emit("<go:void/>")
		return
	}

	if len(node.List) == 1 {
		c.emitField(node.List[0])
		return
	}

	c.emit("<go:values>")
	for _, field := range node.List {
		c.emit(" ")
		c.emitField(field)
	}
	c.emit("</go:values>")
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
	c.emit("<go:%s>", node.Tok.String())
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
	c.emit("</go:%s>", node.Tok.String())
}

func (c *Compiler) emitGoStmt(node *ast.GoStmt) {
	c.emit("<m:apply><go:go/>")
	c.emitCallExpr(node.Call)
	c.emit("</m:apply>")
}

func (c *Compiler) emitIdent(node *ast.Ident) {
	id := goIdToXmlId(node.Name)
	if id[0] == '<' {
		c.emit(id)
	} else {
		c.emit("<m:ci>%s</m:ci>", id)
	}
}

func (c *Compiler) emitIfStmt(node *ast.IfStmt) {
	unless := false
	if un, ok := node.Cond.(*ast.UnaryExpr); ok {
		if un.Op.String() == "!" {
			unless = true
		}
	}
	c.emit("<m:apply>")
	if unless {
		c.emit("<go:unless")
	} else {
		c.emit("<go:when")
	}
	if node.Init != nil {
		c.emit("-init")
		c.emitStmt(node.Init)
	}
	c.emit("/>")
	if unless {
		c.emitExpr(node.Cond.(*ast.UnaryExpr).X)
	} else {
		c.emitExpr(node.Cond)
	}
	c.emit(" ")
	c.emitBlockStmt(node.Body)
	if node.Else != nil {
		c.emit("<go:else>")
		switch a := node.Else.(type) {
		case *ast.BlockStmt:
			c.emitBlockStmt(a)
		default:
			c.emitStmt(node.Else)
		}
		c.emit("</go:else>")
	}
	c.emit("</m:apply>")
}

func (c *Compiler) emitImports(any interface{}) {
	c.emit("<go:import>")
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
	c.emit("</go:import>")
}

func (c *Compiler) emitImportSpec(node *ast.ImportSpec) {
	if node.Name != nil {
		c.emitBasicLitWithAttr(node.Path, "as", node.Name.Name)
		return
	}
	c.emitBasicLit(node.Path)
}

// Importer

func (c *Compiler) emitIncDecStmt(node *ast.IncDecStmt) {
	c.emit("<m:apply>%s ", goUnaryOpToXmlOp(node.Tok.String()))
	c.emitExpr(node.X)
	c.emit("</m:apply>")
}

func (c *Compiler) emitIndexExpr(node *ast.IndexExpr) {
	c.emit("<m:apply><go:selector/>")
	c.emitExpr(node.X)
	c.emit(" ")
	c.emitExpr(node.Index)
	c.emit("</m:apply>")
}

func (c *Compiler) emitInterfaceMethod(node *ast.Field) {
	if len(node.Names) == 0 {
		c.emitType(node.Type)
		return
	}
	if len(node.Names) != 1 {
		panic("expected single method name")
	}
	c.emit("<go:function>")
	c.emitIdent(node.Names[0])
	c.emit(" ")
	c.emitFuncTypes(node.Type.(*ast.FuncType), false)
	c.emit("</go:function>")
}

func (c *Compiler) emitInterfaceMethodList(node *ast.FieldList) {
	for _, field := range node.List {
		c.emit(" ")
		c.emitInterfaceMethod(field)
	}
}

func (c *Compiler) emitInterfaceType(node *ast.InterfaceType) {
	c.emit("<go:interface>")
	c.emitInterfaceMethodList(node.Methods)
	c.emit("</go:interface>")
}

func (c *Compiler) emitKeyValueExpr(node *ast.KeyValueExpr) {
	c.emit("(: ")
	if key, ok := node.Key.(*ast.BasicLit); ok {
		c.emitBasicLit(key)
		c.emit(" ")
	} else if key, ok := node.Key.(*ast.Ident); ok {
		c.emit("%s ", goIdToXmlId(key.Name))
	} else {
		panic("emitKeyValueExpr: expected string or identifier")
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
	c.emit("<label>%s", node.Label.String())
	c.emitStmt(node.Stmt)
	c.emit("</label>")
}

func (c *Compiler) emitMapType(node *ast.MapType) {
	c.emit("<go:map>")
	c.emitType(node.Key)
	c.emit(" ")
	c.emitType(node.Value)
	c.emit("</go:map>")
}

// MergeMode
// Node
// ObjKind
// Object
// Package
// ParenExpr

func (c *Compiler) emitRangeStmt(node *ast.RangeStmt) {
	c.emit("<m:apply><go:range/> (%s ", node.Tok.String())
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
	c.emit("</m:apply>")
}

func (c *Compiler) emitReturnStmt(node *ast.ReturnStmt) {
	c.emit("<m:apply><go:return/>")
	for _, arg := range node.Results {
		c.emit(" ")
		c.emitExpr(arg)
	}
	c.emit("</m:apply>")
	
}

// Scope

func (c *Compiler) emitSelectStmt(node *ast.SelectStmt) {
	c.emit("(comm!")
	for _, stmt := range node.Body.List {
		c.emit(" ")
		c.emitCommClause(stmt.(*ast.CommClause))
	}
	c.emit(")")
}

func (c *Compiler) emitSelectorExpr(node *ast.SelectorExpr) {
	//if id, ok := node.X.(*ast.Ident); ok {
	//	c.emit("%s.%s", goIdToXmlId(id.Name), goIdToXmlId(node.Sel.Name))
	//	return
	//}
	c.emit("<m:apply><go:dot/>")
	c.emitExpr(node.X)
	c.emitIdent(node.Sel)
	c.emit("</m:apply>")
}

func (c *Compiler) emitSendStmt(node *ast.SendStmt) {
	c.emit("(<-! ")
	c.emitExpr(node.Chan)
	c.emit(" ")
	c.emitExpr(node.Value)
	c.emit(")")
}

func (c *Compiler) emitSliceExpr(node *ast.SliceExpr) {
	c.emit("<m:apply><go:selector/>")
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
	c.emit("</m:apply>")
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
	c.emit("<m:apply><go:ptr/>")
	c.emitType(node.X)
	c.emit("</m:apply>")
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
	c.emit("<m:apply><go:struct/>")
	c.emitFieldList(node.Fields)
	c.emit("</m:apply>")
}

func (c *Compiler) emitSwitchStmt(node *ast.SwitchStmt) {
	cond := node.Tag == nil
	if cond {
		c.emit("<m:apply><go:cond")
	} else {
		c.emit("<m:apply><go:case")
	}
	if node.Init != nil {
		c.emit("-init")
		c.emitStmt(node.Init)
	}
	c.emit("/>")
	if !cond {
		c.emit(" ")
		c.emitExpr(node.Tag)
	}
	for _, stmt := range node.Body.List {
		c.emit(" ")
		c.emitCaseClause(stmt.(*ast.CaseClause), cond)
	}
	c.emit("</m:apply>")
}

func (c *Compiler) emitType(node ast.Expr) {
	if typeid, ok := node.(*ast.Ident); ok {
		c.emitIdent(typeid)
		//c.emitRaw(goIdToXmlId(id.Name))
		//c.emit(id)
		return
	}
	c.emitExpr(node)
}

func (c *Compiler) emitTypeAssertExpr(node *ast.TypeAssertExpr) {
	c.emit("<m:apply><go:as/> ")
	c.emitExpr(node.X)
	c.emit(" ")
	if node.Type == nil {
		c.emit("type")
	} else {
		c.emitType(node.Type)
	}
	c.emit("</m:apply>")
}

func (c *Compiler) emitTypeSpec(node *ast.TypeSpec) {
	c.emitIdent(node.Name)
	c.emit(" ")
	c.emitType(node.Type)
}

func (c *Compiler) emitTypeSwitchStmt(node *ast.TypeSwitchStmt) {
	c.emit("<m:apply><go:type-case/>")
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
	c.emit("</m:apply>")
}

func (c *Compiler) emitUnaryExpr(node *ast.UnaryExpr) {
	c.emit("<m:apply>%s ", goUnaryOpToXmlOp(node.Op.String()))
	c.emitExpr(node.X)
	c.emit("</m:apply>")
}

// helper function
func (c *Compiler) emitValueNames(ids []*ast.Ident) {
	buffer := []byte{}
	for _, id := range ids {
		buffer = append(buffer, ' ')
		buffer = append(buffer, "<m:ci>"...)
		buffer = append(buffer, goIdToXmlId(id.Name)...)
		buffer = append(buffer, "</m:ci>"...)
	}
	if len(ids) == 1 {
		c.emit("%s", string(buffer[1:]))
	} else {
		c.emit("<go:tuple>%s</go:tuple>", string(buffer[1:]))
	}
}

func (c *Compiler) emitValueTypedNames(ids []*ast.Ident, t ast.Expr) {
	buffer := []byte{}
	for _, id := range ids {
		buffer = append(buffer, "<m:ci>"...)
		buffer = append(buffer, goIdToXmlId(id.Name)...)
		buffer = append(buffer, "</m:ci>"...)
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
			c.emit("<go:is>")
			c.emitValueTypedNames(node.Names, node.Type)
			for _, arg := range node.Values {
				c.emit(" ")
				c.emitExpr(arg)
			}
			c.emit("</go:is>")
		} else {
			// "(define-var #(%s %s))", name(s), type
			c.emitValueTypedNames(node.Names, node.Type)
		}
	} else if node.Values != nil {
		// "(define-const (= %s %s))", name, value
		// "(define-var (= %s %s))", name, value
		// "(define-var (= (%s) %s))", name(s), value(s)
		c.emit("<go:is>")
		c.emitValueNames(node.Names)
		for _, arg := range node.Values {
			c.emit(" ")
			c.emitExpr(arg)
		}
		c.emit("</go:is>")
	} else {
		// "(define-const %s)", name
		c.emitValueNames(node.Names)
	}
}

// Visitor