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



#include "parser.h"

#include <iostream>
#include <vector>
#include <string>

#define NO_DEBUG_NODE(x) x
#define DEBUG_NODE(x) x.set_name(#x); x.set_debug(true); x
// #define DEBUG_NODE(x) x

#include "xquerygrammar.h"

ARule strlit(const std::string &s) {
    return ARule(s);
}
ARule ch_p(char c) {
    return ARule(c);
}

/** Skip spaces and comments in XQuery
 * <code>
    * [150]       Comment     ::=      "(:" (CommentContents | Comment)* ":)"
    * [151]       CommentContents      ::=      (Char+ - (Char* ':)' Char*))
    * </code>
 */
XQuerySkip::XQuerySkip() {
    NO_DEBUG_NODE(Skip) = Comment | space_p;
    NO_DEBUG_NODE(Comment) = lexeme_d["(:" >> (CommentContents | Comment) >> ":)"];
    NO_DEBUG_NODE(CommentContents) =  *((anychar_p - ':') | (':' >> (anychar_p - ')')));
}

const ARule &XQuerySkip::start() const {
    return Skip;
}



namespace {
#define TOKEN(x) const Token t_ ## x(#x)
TOKEN(for)
;
TOKEN(let);
TOKEN(NCName);
TOKEN(where);
TOKEN(return);
TOKEN(axis);
TOKEN(stable);
TOKEN(order_by);
TOKEN(step);
TOKEN(attribute);
TOKEN(predicate);
TOKEN(QName);

// Hierarchical operators
TOKEN(document);
TOKEN(any);
}

XQueryGrammar::XQueryGrammar()
        : keyword(NCName) {
    // [1]       Module      ::=      VersionDecl? (MainModule | LibraryModule)
    DEBUG_NODE(Module) = !VersionDecl >> (MainModule | LibraryModule);

    // [2]      VersionDecl       ::=      <"xquery" "version"> StringLiteral ("encoding" StringLiteral)? Separator
    DEBUG_NODE(VersionDecl) = keyword("xquery") >> keyword("version") >> StringLiteral >> !("encoding" >> StringLiteral) >> Separator;

    // [3]      MainModule     ::=      Prolog QueryBody
    DEBUG_NODE(MainModule) = Prolog >> QueryBody;

    // [4]      LibraryModule     ::=      ModuleDecl Prolog
    DEBUG_NODE(LibraryModule) = ModuleDecl >> Prolog;

    // [5]      ModuleDecl     ::=      <"module" "namespace"> NCName "=" StringLiteral Separator

    // [6]      Prolog      ::=      (Setter Separator)* ((Import | NamespaceDecl | DefaultNamespaceDecl) Separator)* ((VarDecl | FunctionDecl) Separator)*
    DEBUG_NODE(Prolog) =
        *(Setter >> Separator)
        >> *((Import | NamespaceDecl | DefaultNamespaceDecl) >> Separator)
        >> *((VarDecl | FunctionDecl) >> Separator);

    // [7]      Setter      ::=      XMLSpaceDecl | DefaultCollationDecl | BaseURIDecl | ConstructionDecl | OrderingModeDecl | EmptyOrderingDecl | InheritNamespacesDecl
    // [8]      Import      ::=      SchemaImport | ModuleImport
    // [9]      Separator      ::=      ";"
    DEBUG_NODE(Separator) = ';';

    // [10]     NamespaceDecl     ::=      <"declare" "namespace"> NCName "=" StringLiteral
    // [11]     XMLSpaceDecl      ::=      <"declare" "xmlspace"> ("preserve" | "strip")
    // [12]     DefaultNamespaceDecl       ::=      (<"declare" "default" "element"> | <"declare" "default" "function">) "namespace" StringLiteral
    // [13]     OrderingModeDecl     ::=      <"declare" "ordering"> ("ordered" | "unordered")
    // [14]     EmptyOrderingDecl       ::=      <"declare" "default" "order"> (<"empty" "greatest"> | <"empty" "least">)
    // [15]     InheritNamespacesDecl      ::=      <"declare" "inherit-namespaces"> ("yes" | "no")
    // [16]     DefaultCollationDecl       ::=      <"declare" "default" "collation"> StringLiteral
    // [17]     BaseURIDecl       ::=      <"declare" "base-uri"> StringLiteral
    // [18]     SchemaImport      ::=      <"import" "schema"> SchemaPrefix? StringLiteral (<"at" StringLiteral> (", " StringLiteral)*)?
    // [19]     SchemaPrefix      ::=      ("namespace" NCName "=") | (<"default" "element"> "namespace")
    // [20]     ModuleImport      ::=      <"import" "module"> ("namespace" NCName "=")? StringLiteral (<"at" StringLiteral> (", " StringLiteral)*)?
    // [21]     VarDecl     ::=      <"declare" "variable" "$"> VarName TypeDeclaration? ((":=" ExprSingle) | "external")
    // [22]     ConstructionDecl     ::=      <"declare" "construction"> ("preserve" | "strip")
    // [23]     FunctionDecl      ::=      <"declare" "function"> <QName "("> ParamList? (")" | (<")" "as"> SequenceType)) (EnclosedExpr | "external")
    // [24]     ParamList      ::=      Param (", " Param)*

    // [25]     Param       ::=      "$" VarName TypeDeclaration?

    // [26]     EnclosedExpr      ::=      "{" Expr "}"
    DEBUG_NODE(EnclosedExpr) = "{" >> phrase_d[Expr] >> "}";

    // [27]     QueryBody      ::=      Expr
    DEBUG_NODE(QueryBody) = Expr ;

    // [28]     Expr     ::=      ExprSingle (", " ExprSingle)*
    DEBUG_NODE(Expr) = *ExprSingle; // >> *("," >> ExprSingle);


    // [29]     ExprSingle     ::=      FLWORExpr | QuantifiedExpr | TypeswitchExpr | IfExpr | OrExpr
    DEBUG_NODE(ExprSingle) =  FLWORExpr | QuantifiedExpr | TypeswitchExpr | IfExpr | OrExpr;

    // [30]     FLWORExpr      ::=      (ForClause | LetClause)+ WhereClause? OrderByClause? "return" ExprSingle
    DEBUG_NODE(FLWORExpr) =  +(ForClause | LetClause) >> !WhereClause >> !OrderByClause >> (t_return^ARule("return")) >> ExprSingle;

    // [31]     ForClause      ::=      <"for" "$"> VarName TypeDeclaration? PositionalVar? "in" ExprSingle (", " "$" VarName TypeDeclaration? PositionalVar? "in" ExprSingle)*
    DEBUG_NODE(ForClause) = (t_for^keyword("for"))  >> "$" >> VarName >> !TypeDeclaration >> !PositionalVar >> "in" >> ExprSingle >> *(strlit(",") >> "$" >> VarName >> !TypeDeclaration >> !PositionalVar >> "in" >> ExprSingle);


    // [32]     PositionalVar     ::=      "at" "$" VarName
    PositionalVar= keyword("at") >> "$" >> VarName;

    // [33]     LetClause      ::=      <"let" "$"> VarName TypeDeclaration? ":=" ExprSingle (", " "$" VarName TypeDeclaration? ":=" ExprSingle)*
    DEBUG_NODE(LetClause) = (t_let^keyword("let")) >> "$" >> VarName >> !TypeDeclaration >> ":=" >> ExprSingle >> *(strlit(", ") >> "$" >> VarName >> !TypeDeclaration >>":=" >> ExprSingle);

    // [34]     WhereClause       ::=      "where" ExprSingle
    WhereClause = (t_where^keyword("where")) >> ExprSingle;

    // [35]     OrderByClause     ::=      (<"order" "by"> | <"stable" "order" "by">) OrderSpecList
    OrderByClause= !t_stable[keyword("stable")]
                   >> (t_order_by^(keyword("order") >> keyword("by"))) >> OrderSpecList;

    // [36]     OrderSpecList     ::=      OrderSpec (", " OrderSpec)*
    OrderSpecList = OrderSpec >> *(strlit(",") >> OrderSpec);

    // [37]     OrderSpec      ::=      ExprSingle OrderModifier
    OrderSpec = ExprSingle >> OrderModifier;

    // [38]     OrderModifier     ::=      ("ascending" | "descending")? (<"empty" "greatest"> | <"empty" "least">)? ("collation" StringLiteral)?
    OrderModifier = !(keyword("ascending") | keyword("descending")) >> !(strlit("empty") >> "greatest" | strlit("empty") >> "least") >> !(strlit("collation") >> StringLiteral);

    // [39]     QuantifiedExpr       ::=      (<"some" "$"> | <"every" "$">) VarName TypeDeclaration? "in" ExprSingle (", " "$" VarName TypeDeclaration? "in" ExprSingle)* "satisfies" ExprSingle
    QuantifiedExpr = !(keyword("some") | keyword("every") ) >> "$" >> VarName >> !TypeDeclaration >> "in" >> ExprSingle >> *(strlit(", ") >> "$" >> VarName >> !TypeDeclaration >> "in" >> ExprSingle) >> "satisfies" >> ExprSingle;

    // [40]     TypeswitchExpr       ::=      <"typeswitch" "("> Expr ")" CaseClause+ "default" ("$" VarName)? "return" ExprSingle
    TypeswitchExpr = strlit("typeswitch") >> "(" >> Expr >> ")" >> +CaseClause >> "default" >> !(strlit("$") >> VarName)  >> "return" >> ExprSingle;

    // [41]     CaseClause     ::=      "case" ("$" VarName "as")? SequenceType "return" ExprSingle
    DEBUG_NODE(CaseClause) = strlit("case") >> !("$" >> VarName >> "as") >> SequenceType >> "return" >> ExprSingle;

    // [42]     IfExpr      ::=      <"if" "("> Expr ")" "then" ExprSingle "else" ExprSingle
    IfExpr = strlit("if") >> "(" >> Expr >> ")" >> "then" >> ExprSingle >> "else" >> ExprSingle;

    // [43]     OrExpr      ::=      AndExpr ( "or" AndExpr )*
    DEBUG_NODE(OrExpr) = AndExpr >> *( "or" >> AndExpr );

    // [44]     AndExpr     ::=      ComparisonExpr ( "and" ComparisonExpr )*
    DEBUG_NODE(AndExpr) = ComparisonExpr >> *( "and" >> ComparisonExpr );

    // [45]     ComparisonExpr       ::=      RangeExpr ( (ValueComp | GeneralComp | NodeComp) RangeExpr )?
    DEBUG_NODE(ComparisonExpr) = RangeExpr >> !( (NodeComp | ValueComp | GeneralComp) >> RangeExpr );

    // [46]     RangeExpr      ::=      AdditiveExpr ( "to" AdditiveExpr )?
    DEBUG_NODE(RangeExpr) = AdditiveExpr >> !( "to" >> AdditiveExpr );

    // [47]     AdditiveExpr      ::=      MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
    DEBUG_NODE(AdditiveExpr) = MultiplicativeExpr >> *( (strlit("+") | strlit("-")) >> MultiplicativeExpr );

    // [48]     MultiplicativeExpr      ::=      UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
    DEBUG_NODE(MultiplicativeExpr) = UnionExpr >> *( (strlit("*") | "div" | "idiv" | "mod") >> UnionExpr );

    // [49]     UnionExpr      ::=      IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*
    DEBUG_NODE(UnionExpr) = IntersectExceptExpr >> *( (strlit("union") | "|") >> IntersectExceptExpr );

    // [50]     IntersectExceptExpr     ::=      InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*
    DEBUG_NODE(IntersectExceptExpr) = InstanceofExpr >> *( (strlit("intersect") | "except") >> InstanceofExpr );

    // [51]     InstanceofExpr       ::=      TreatExpr ( <"instance" "of"> SequenceType )?
    DEBUG_NODE(InstanceofExpr) = TreatExpr >> !( strlit("instance") >> "of" >> SequenceType );

    // [52]     TreatExpr      ::=      CastableExpr ( <"treat" "as"> SequenceType )?
    DEBUG_NODE(TreatExpr) = CastableExpr >> !(strlit("treat") >> "as" >> SequenceType );

    // [53]     CastableExpr      ::=      CastExpr ( <"castable" "as"> SingleType )?
    DEBUG_NODE(CastableExpr) = CastExpr >> !(strlit("castable") >> "as" >> SingleType);

    // [54]     CastExpr       ::=      UnaryExpr ( <"cast" "as"> SingleType )?
    DEBUG_NODE(CastExpr) = UnaryExpr >> !(strlit("cast") >> "as" >> SingleType);

    // [55]     UnaryExpr      ::=      ("-" | "+")* ValueExpr
    DEBUG_NODE(UnaryExpr)  = * (ch_p('-') | ch_p('+')) >> ValueExpr;

    // [56]     ValueExpr      ::=      ValidateExpr | PathExpr
    DEBUG_NODE(ValueExpr) = ValidateExpr | PathExpr;

    // [57]     GeneralComp       ::=      "=" | "!=" | "<" | "<=" | ">" | ">="   /* gn: lt */
    DEBUG_NODE(GeneralComp) = strlit("=") | "!=" | "<=" | ">=" | "<"  | ">" ;

    // [58]     ValueComp      ::=      "eq" | "ne" | "lt" | "le" | "gt" | "ge"
    DEBUG_NODE(ValueComp) = (keyword("eq") | keyword("ne") | keyword("lt") | keyword("le") | keyword("gt") | keyword("ge"));

    // [59]     NodeComp       ::=      "is" | "<<" | ">>"
    DEBUG_NODE(NodeComp) = keyword("is") | "<<" | ">>";

    // [60]     ValidateExpr      ::=      (<"validate" "{"> | (<"validate" ValidationMode> "{")) Expr "}"   /* gn: validate */

    // [61]     PathExpr       ::=      ("/" RelativePathExpr?) | ("//" RelativePathExpr) | RelativePathExpr   /* gn: leading-lone-slash */
    DEBUG_NODE(PathExpr) =  ("//" >> RelativePathExpr) | ("/" >> !RelativePathExpr) | RelativePathExpr;

    // [62]     RelativePathExpr     ::=      StepExpr (("/" | "//") StepExpr)*
    DEBUG_NODE(RelativePathExpr)     =      StepExpr >> *((strlit("//") | (t_step^strlit("/"))) >> StepExpr);

    // [63]     StepExpr       ::=      AxisStep | FilterExpr
    DEBUG_NODE(StepExpr) =  FilterExpr | AxisStep;

    // [64]     AxisStep       ::=      (ForwardStep | ReverseStep) PredicateList
    DEBUG_NODE(AxisStep) = longest_d[ForwardStep | ReverseStep] >> PredicateList;

    // [65]     ForwardStep       ::=      (ForwardAxis NodeTest) | AbbrevForwardStep
    DEBUG_NODE(ForwardStep) = (ForwardAxis++ >> NodeTest) | AbbrevForwardStep;

    // [66]     ForwardAxis       ::=      <"child" "::"> | <"descendant" "::"> | <"attribute" "::">
    // | <"self" "::"> | <"descendant-or-self" "::"> | <"following-sibling" "::"> | <"following" "::">
    DEBUG_NODE(ForwardAxis) =
        t_axis[keyword("child")] >> strlit("::")
        | t_axis[keyword("descendant")] >> strlit("::")
        |  t_axis[keyword("attribute")] >> strlit("::")
        |  t_axis[keyword("self")] >> strlit("::")
        | t_axis[keyword("descendant-or-self")] >> strlit("::")
        | t_axis[keyword("following-sibling")] >> strlit("::")
        | t_axis[keyword("following")] >> strlit("::")
        ;

    // [67]     AbbrevForwardStep       ::=      "@"? NodeTest
    DEBUG_NODE(AbbrevForwardStep) = !(t_attribute^ch_p('@')) >> NodeTest;

    // [68]     ReverseStep       ::=      (ReverseAxis NodeTest) | AbbrevReverseStep
    DEBUG_NODE(ReverseStep) =    (ReverseAxis++ >> NodeTest) | AbbrevReverseStep;

    // [69]     ReverseAxis       ::=      <"parent" "::"> | <"ancestor" "::"> | <"preceding-sibling" "::"> | <"preceding" "::"> | <"ancestor-or-self" "::">
    DEBUG_NODE(ReverseAxis) =
        t_axis[keyword("parent")] >> strlit("::")
        | t_axis[keyword("ancestor")] >> strlit("::")
        | t_axis[keyword("preceding-sibling")] >> strlit("::")
        | t_axis[keyword("preceding")] >> strlit("::")
        | t_axis[keyword("ancestor-or-self")] >> strlit("::");

    // [70]     AbbrevReverseStep       ::=      ".."
    DEBUG_NODE(AbbrevReverseStep) = strlit("..");

    // [71]     NodeTest       ::=      KindTest | NameTest
    DEBUG_NODE(NodeTest) = KindTest | NameTest;

    // [72]     NameTest       ::=      QName | Wildcard
    NameTest = QName | Wildcard;


    // [73]     Wildcard       ::=      "*"| <NCName ":" "*"> | <"*" ":" NCName>   /* ws: explicit */
    Wildcard = ch_p('*') | (NCName >> ch_p(':') >> ch_p('*')) | (ch_p('*') >> NCName);

    // [74]     FilterExpr     ::=      PrimaryExpr PredicateList
    DEBUG_NODE(FilterExpr) = PrimaryExpr >> PredicateList;

    // [75]     PredicateList     ::=      Predicate*
    DEBUG_NODE(PredicateList) = *(Predicate >> t_predicate[true_p]++);


    // [76]     Predicate      ::=      "[" Expr "]"
    DEBUG_NODE(Predicate) = ch_p('[') >> Expr >> ']';

    // [77]     PrimaryExpr       ::=      Literal | VarRef | ParenthesizedExpr | ContextItemExpr | FunctionCall | Constructor | OrderedExpr | UnorderedExpr
    DEBUG_NODE(PrimaryExpr) =      Literal | VarRef | ParenthesizedExpr | ContextItemExpr | FunctionCall | Constructor | OrderedExpr | UnorderedExpr;

    // [78]     Literal     ::=      NumericLiteral | StringLiteral
    DEBUG_NODE(Literal)     =      NumericLiteral | StringLiteral;

    // [79]     NumericLiteral       ::=      IntegerLiteral | DecimalLiteral | DoubleLiteral
    DEBUG_NODE(NumericLiteral) = IntegerLiteral | DecimalLiteral | DoubleLiteral;

    // [80]     VarRef      ::=      "$" VarName
    DEBUG_NODE(VarRef) = strlit("$") >> VarName;

    // [81]     ParenthesizedExpr       ::=      "(" Expr? ")"
    ParenthesizedExpr = strlit("(") >> !Expr >> ")";

    // [82]     ContextItemExpr      ::=      "."
    ContextItemExpr = strlit(".");

    // [83]     OrderedExpr       ::=      <"ordered" "{"> Expr "}"

    // [84]     UnorderedExpr     ::=      <"unordered" "{"> Expr "}"

    // [85]     FunctionCall      ::=      <QName "("> (ExprSingle (", " ExprSingle)*)? ")"
    // /* gn: parens */
    // /* gn: reserved-function-names */
    DEBUG_NODE(FunctionCall) = QName >> "(" >> !(ExprSingle >> *(strlit(",") >> ExprSingle)) >> ")";

    // [86]     Constructor       ::=      DirectConstructor | ComputedConstructor
    Constructor = DirectConstructor | ComputedConstructor;

    // [87]     DirectConstructor       ::=      DirElemConstructor | DirCommentConstructor | DirPIConstructor
    DirectConstructor = DirElemConstructor | DirCommentConstructor | DirPIConstructor;

    // [88]     DirElemConstructor      ::=      "<" QName DirAttributeList ("/>" | (">" DirElemContent* "</" QName S? ">"))   /* ws: explicit */
    DEBUG_NODE(DirElemConstructor) = lexeme_d[strlit("<") >> QName >> DirAttributeList
                                     >> (strlit("/>") | (strlit(">") >> *DirElemContent >> "</" >> QName >> !S >> ">"))]
                                     ;

    // /* gn: lt */
    // [89]     DirAttributeList     ::=      (S (QName S? "=" S? DirAttributeValue)?)*    /* ws: explicit */
    DirAttributeList = *(S >> !(QName >> !S >> "=" >> !S >> DirAttributeValue));

    // [90]     DirAttributeValue       ::=      ('"' (EscapeQuot | QuotAttrValueContent)* '"') | ("'" (EscapeApos | AposAttrValueContent)* "'")   /* ws: explicit */
    DirAttributeValue = ('"' >> *(EscapeQuot | QuotAttrValueContent) >> '"') | ("'" >> *(EscapeApos | AposAttrValueContent) >> "'");

    // [91]     QuotAttrValueContent       ::=      QuotAttrContentChar | CommonContent
    QuotAttrValueContent =  QuotAttrContentChar | CommonContent;

    // [92]     AposAttrValueContent       ::=      AposAttrContentChar  | CommonContent
    AposAttrValueContent = AposAttrContentChar  | CommonContent;

    // [93]     DirElemContent       ::=      DirectConstructor | ElementContentChar | CDataSection | CommonContent
    DEBUG_NODE(DirElemContent) = DirectConstructor | ElementContentChar | CDataSection | CommonContent;

    // [94]     CommonContent     ::=      PredefinedEntityRef | CharRef | "{{" | "}}" | EnclosedExpr
    DEBUG_NODE(CommonContent) = PredefinedEntityRef | CharRef | "{{" | "}}" | EnclosedExpr;

    // [95]     DirCommentConstructor      ::=      "<!--" DirCommentContents "-->"  /* ws: explicit */

    // [96]     DirCommentContents      ::=      ((Char - '-') | <'-' (Char - '-')>)*   /* ws: explicit */
    // [97]     DirPIConstructor     ::=      "<?" PITarget (S DirPIContents)? "?>"  /* ws: explicit */
    // [98]     DirPIContents     ::=      (Char* - (Char* '?>' Char*))  /* ws: explicit */
    // [99]     CDataSection      ::=      "<![CDATA[" CDataSectionContents "]]>"    /* ws: explicit */
    // [100]       CDataSectionContents       ::=      (Char* - (Char* ']]>' Char*))    /* ws: explicit */
    // [101]       ComputedConstructor     ::=      CompDocConstructor
    // | CompElemConstructor
    // | CompAttrConstructor
    // | CompTextConstructor
    // | CompCommentConstructor
    // | CompPIConstructor
    // [102]       CompDocConstructor      ::=      <"document" "{"> Expr "}"
    // [103]       CompElemConstructor     ::=      (<"element" QName "{"> | (<"element" "{"> Expr "}" "{")) ContentExpr? "}"

    // [104]       ContentExpr       ::=      Expr
    ContentExpr = Expr;

    // [105]       CompAttrConstructor     ::=      (<"attribute" QName "{"> | (<"attribute" "{"> Expr "}" "{")) Expr? "}"

    // [106]       CompTextConstructor     ::=      <"text" "{"> Expr "}"
    // [107]       CompCommentConstructor     ::=      <"comment" "{"> Expr "}"
    // [108]       CompPIConstructor       ::=      (<"processing-instruction" NCName "{"> | (<"processing-instruction" "{"> Expr "}" "{")) Expr? "}"
    // [109]       SingleType     ::=      AtomicType "?"?
    // [110]       TypeDeclaration      ::=      "as" SequenceType
    // [111]       SequenceType      ::=      (ItemType OccurrenceIndicator?)
    // | <"empty" "(" ")">
    // [112]       OccurrenceIndicator     ::=      "?" | "*" | "+"   /* gn: occurrence-indicators */
    // [113]       ItemType       ::=      AtomicType | KindTest | <"item" "(" ")">
    // [114]       AtomicType     ::=      QName
    AtomicType = QName;

    // [115]       KindTest       ::=      DocumentTest  | ElementTest | AttributeTest | SchemaElementTest | SchemaAttributeTest | PITest | CommentTest | TextTest | AnyKindTest
    KindTest = DocumentTest  | ElementTest | AttributeTest | SchemaElementTest | SchemaAttributeTest | PITest | CommentTest | TextTest | AnyKindTest;

    // [116]       AnyKindTest       ::=      <"node" "("> ")"
    // [117]       DocumentTest      ::=      <"document-node" "("> (ElementTest | SchemaElementTest)? ")"
    // [118]       TextTest       ::=      <"text" "("> ")"
    // [119]       CommentTest       ::=      <"comment" "("> ")"
    // [120]       PITest      ::=      <"processing-instruction" "("> (NCName | StringLiteral)? ")"
    // [121]       AttributeTest     ::=      <"attribute" "("> (AttribNameOrWildcard (", " TypeName)?)? ")"
    // [122]       AttribNameOrWildcard       ::=      AttributeName | "*"
    // [123]       SchemaAttributeTest     ::=      <"schema-attribute" "("> AttributeDeclaration ")"
    // [124]       AttributeDeclaration       ::=      AttributeName
    // [125]       ElementTest       ::=      <"element" "("> (ElementNameOrWildcard (", " TypeName "?"?)?)? ")"
    // [126]       ElementNameOrWildcard      ::=      ElementName | "*"
    // [127]       SchemaElementTest       ::=      <"schema-element" "("> ElementDeclaration ")"
    // [128]       ElementDeclaration      ::=      ElementName
    // [129]       AttributeName     ::=      QName
    // [130]       ElementName       ::=      QName
    // [131]       TypeName       ::=      QName

    // [132]       IntegerLiteral       ::=      Digits
    IntegerLiteral = Digits;

    // [133]       DecimalLiteral       ::=      ("." Digits) | (Digits "." [0-9]*)  /* ws: explicit */
    DecimalLiteral = ("." >> Digits) | (Digits >> "." >> *digit_p);

    // [134]       DoubleLiteral     ::=      (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits   /* ws: explicit */

    // [135]       StringLiteral     ::=      ('"' (PredefinedEntityRef | CharRef | ('"' '"') | [^"&])* '"') | ("'" (PredefinedEntityRef | CharRef | ("'" "'") | [^'&])* "'")  /* ws: explicit */
    DEBUG_NODE(StringLiteral) =
        lexeme_d[
            (ch_p('"') >> *(PredefinedEntityRef | CharRef | "\"\"" | ~(ch_p('"') | '&')) >> '"')
            | (ch_p('\'') >> *(PredefinedEntityRef | CharRef | "''" | ~(ch_p('"') | '&')) >> '\'')
        ];

    // [136]       PITarget       ::=      [http://www.w3.org/TR/REC-xml#NT-PITarget]XML   /* gn: xml-version */

    // [137]       VarName     ::=      QName
    VarName = QName;

    // [138]       ValidationMode       ::=      "lax" | "strict"
    ValidationMode = strlit("lax") | "strict";

    // [139]       Digits      ::=      [0-9]+
    Digits = lexeme_d[+digit_p];

    // [140]       PredefinedEntityRef     ::=      "&" ("lt" | "gt" | "amp" | "quot" | "apos") ";"    /* ws: explicit */
    DEBUG_NODE(PredefinedEntityRef) = '&' >> (strlit("lt") | "gt" | "amp" | "quot" | "apos");

    // [141]       CharRef     ::=      [http://www.w3.org/TR/REC-xml#NT-CharRef]XML    /* gn: xml-version */
    CharRef = "&#" >> +digit_p >> ';' | "&#x" >>  +xdigit_p >> ';';

    // [142]       EscapeQuot     ::=      '""'
    EscapeQuot = "\"\"";

    // [143]       EscapeApos     ::=      "''"
    EscapeApos = "''";

    // [144]       ElementContentChar      ::=      Char - [{}<&]
    DEBUG_NODE(ElementContentChar) = Char - (ch_p('{') | '}' | '<' | '&');

    // [145]       QuotAttrContentChar     ::=      Char - ["{}<&]
    // [146]       AposAttrContentChar     ::=      Char - ['{}<&]
    // [147]       Pragma      ::=      "(::" S? "pragma" S QName (S ExtensionContents)? "::)"   /* ws: explicit */
    //             Pragma = "(::" S? "pragma" S QName (S ExtensionContents)? "::)";

    // [148]       MUExtension       ::=      "(::" S? "extension" S QName (S ExtensionContents)? "::)"   /* ws: explicit */
    // [149]       ExtensionContents       ::=      (Char* - (Char* '::)' Char*))

    
    // [152]       QName       ::=      [http://www.w3.org/TR/REC-xml-names/#NT-QName]Names   /* gn: xml-version */
    //      = [6]   QName     ::=  (Prefix ':')? LocalPart
    //      = [7]   Prefix   ::=   NCName
    //      = [8]   LocalPart   ::=   NCName
    QName = NCName >> !(':' >> NCName) >> t_QName[true_p]++;

    // [153]       NCName      ::=      [http://www.w3.org/TR/REC-xml-names/#NT-NCName]Names  /* gn: xml-version */
    // [4]   NCName    ::=  (Letter | '_') (NCNameChar)*   /*   An XML Name,  minus the ":" */
    // [5]   NCNameChar  ::=   Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
    DEBUG_NODE(NCName) =   t_NCName[lexeme_d[(Letter | '_')  >> *NCNameChar]];

    // FIXME: NCNameChar should be extended to unicode
    NCNameChar  =   alpha_p | digit_p | '.' | '-' | '_';
    Letter = alpha_p;

    // [154]       S     ::=      [http://www.w3.org/TR/REC-xml#NT-S]XML    /* gn: xml-version */
    S = space_p;

    // [155]       Char     ::=      [http://www.w3.org/TR/REC-xml#NT-Char]XML    /* gn: xml-version */
    Char = anychar_p;


}


const ARule &XQueryGrammar::start() const {
    return  Module;
}



XQueryWalker::XQueryWalker() {
   DEBUG_NODE(XQuery) = tree_p[t_NCName];
}

const ARule &XQueryWalker::start() const {
   return  XQuery;
}
