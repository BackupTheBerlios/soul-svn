/***************************************************************************
 *   Copyright (C) 2004 by Benjamin Piwowarski                             *
 *   bpiwowar@dcc.uchile.cl                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/


#ifndef XQUERYGRAMMAR_H
#define XQUERYGRAMMAR_H

#include "parser.h"

using namespace Soul;

struct XQueryGrammar : public Grammar {

   GrammarRule Module, VersionDecl, MainModule, LibraryModule, ModuleDecl, Prolog, Setter, Import, Separator, NamespaceDecl, XMLSpaceDecl, DefaultNamespaceDecl, OrderingModeDecl, EmptyOrderingDecl, InheritNamespacesDecl, DefaultCollationDecl, BaseURIDecl, SchemaImport, SchemaPrefix, ModuleImport, VarDecl, ConstructionDecl, FunctionDecl, ParamList, Param, EnclosedExpr, QueryBody, Expr, ExprSingle, FLWORExpr, ForClause, PositionalVar, LetClause, WhereClause, OrderByClause, OrderSpecList, OrderSpec, OrderModifier, QuantifiedExpr, TypeswitchExpr, CaseClause, IfExpr, OrExpr, AndExpr, ComparisonExpr, RangeExpr, AdditiveExpr, MultiplicativeExpr, UnionExpr, IntersectExceptExpr, InstanceofExpr, TreatExpr, CastableExpr, CastExpr, UnaryExpr, ValueExpr, GeneralComp, ValueComp, NodeComp, ValidateExpr, PathExpr, RelativePathExpr, StepExpr, AxisStep, ForwardStep, ForwardAxis, AbbrevForwardStep, ReverseStep, ReverseAxis, AbbrevReverseStep, NodeTest, NameTest, Wildcard, FilterExpr, PredicateList, Predicate, PrimaryExpr, Literal, NumericLiteral, VarRef, ParenthesizedExpr, ParserContextItemExpr, OrderedExpr, UnorderedExpr, FunctionCall, Constructor, DirectConstructor, DirElemConstructor, QuotAttrValueContent, AposAttrValueContent, CommonContent, DirCommentConstructor, DirCommentContents, DirPIConstructor, DirPIContents, CDataSection, CDataSectionContents, ComputedConstructor, CompDocConstructor, CompElemConstructor, ContentExpr, CompAttrConstructor, CompTextConstructor, CompCommentConstructor, CompPIConstructor, SingleType, TypeDeclaration, SequenceType, OccurrenceIndicator, ItemType, AtomicType, KindTest, AnyKindTest, DocumentTest, TextTest, CommentTest, PITest, AttributeTest, AttribNameOrWildcard, SchemaAttributeTest, AttributeDeclaration, ElementTest, ElementNameOrWildcard, SchemaElementTest, ElementDeclaration, AttributeName, ElementName, TypeName, IntegerLiteral, DecimalLiteral, DoubleLiteral, StringLiteral, PITarget, VarName, ValidationMode, Digits, PredefinedEntityRef, CharRef, EscapeQuot, EscapeApos, ElementContentChar, QuotAttrContentChar, AposAttrContentChar, Pragma, MUExtension, ExtensionContents, Comment, CommentContents, Char, QName, LocalPart,  Prefix, NCNameChar,  Letter,  DirAttributeList,  S,  DirElemContent,  DirAttributeValue,  NCName;
   
   Is_a keyword;
   
//    static const Token t_for, t_let, t_NCName, t_axis, t_return, t_where;
        
   XQueryGrammar();
   const Rule &start() const;
};

struct XQuerySkip : public Grammar {
   GrammarRule Comment, CommentContents, Skip;
   XQuerySkip();
   const Rule &start() const;
};

struct XQueryWalker : public Grammar {
   GrammarRule XQuery;
   XQueryWalker();
   const Rule &start() const;
};

#endif

