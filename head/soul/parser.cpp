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

#include <sstream>
#include <cstdlib> 
#include "parser.h"

namespace Soul {

UnaryOperator::UnaryOperator(const Rule &_rule) : rule(_rule) {}

class Positive : public UnaryOperator {
   public:
      Positive(const Rule &_rule) : UnaryOperator(_rule) {}
       bool match(Scanner &s) const {
       if (!(rule.parse(s))) return false;
       Scanner::Iterator last = s.get_pos();
       while (rule.parse(s)) last = s.get_pos();   
       s.move(last);
       return true;
    }
};

class KleeneStar : public UnaryOperator {
public:
    KleeneStar(const Rule &_rule) : UnaryOperator(_rule) {}
    bool match(Scanner &s) const {
       Scanner::Iterator last = s.get_pos();
       while (rule.parse(s)) last = s.get_pos();
       s.move(last);
       return true;
    }
};

class Optional : public UnaryOperator {
public:
    Optional(const Rule &_rule) : UnaryOperator(_rule) {}
    bool match(Scanner &s) const {
       Scanner::Iterator last = s.get_pos();
       if (!rule.parse(s)) s.move(last);
       return true;
    }
};

class Not : public UnaryOperator {
   public:
      Not(const Rule &_rule) : UnaryOperator(_rule) {}
      bool match(Scanner &s) const {
         return !rule.parse(s);
      }
};



struct BinaryOperator : public RuleImpl {
   const Rule left;
   const Rule right;
   BinaryOperator(const Rule &l, const Rule &r) : left(l), right(r) {}
};


struct Alternative : public BinaryOperator {
   /** Return the longest alternative?*/
   enum { normal, longest, shortest } mode;
   Alternative(const Rule &l, const Rule &r) : BinaryOperator(l,r), mode(normal) {}
   
   bool match(Scanner &s) const {
      switch(mode) { 
         case normal: 
         {
            Scanner::Iterator start = s.get_pos();
            if (left.parse(s)) return true;
            s.move(start); // rewind
            return right.parse(s); 
         }
         
         case longest: case shortest: {
            Scanner::Iterator start = s.get_pos();
            if (!left.parse(s)) {
               s.move(start);
               return right.parse(s);
            }
            Scanner::Iterator left_pos = s.get_pos();
            s.move(start);
            if (!right.parse(s)) { s.move(left_pos); return true; }
            
            // both are true
            if (mode == longest) {
               if ((*left_pos - *s.get_pos()) > 0) s.move(left_pos);
            } else {
               if ((*left_pos - *s.get_pos()) < 0) s.move(left_pos);
            }
            return true;
            break;
         }
         default: assert(false); // FIXME
      }
      return false;
   }
};

struct Sequence : public BinaryOperator {
   Sequence(const Rule &l, const Rule &r) : BinaryOperator(l,r) {}
   bool match(Scanner &s) const {
      return left.parse(s) && right.parse(s);
   }
};

struct ButNode : public BinaryOperator {
   ButNode(const Rule &l, const Rule &r) : BinaryOperator(l,r) {}
   bool match(Scanner &s) const {
      Scanner::Iterator start = s.get_pos();
      if (!left.parse(s)) return false;
      Scanner::Iterator save = s.get_pos();
      s.move(start);
      bool b = right.parse(s);
      s.move(save);
      return !b;
   }
};

Rule Rule::operator!() {
   return Rule((RuleImpl*)new Optional(*this));
}

struct Literal : public RuleImpl {
   std::string value;
   Literal(const std::string &s) : value(s) {/* std::cerr << "[CREATING LITERAL " << s << "]\n";*/ }
   Literal(char c) : value(1,c) { /*std::cerr << "[CREATING LITERAL " << c << "]\n";*/  }
   
   bool match(Scanner &s) const {
      if (s.at_end()) return false;
//       std::cerr << std::string(context.level+1,' ') <<  " [?: MATCH " << value << "] : ";
      std::string::const_iterator i;
      for(i = value.begin(); !s.at_end() && i != value.end(); i++) {
         char c = s.get_char();
         if (*i != c) {
//             std::cerr << *i << " != " << c << " => NO MATCH\n";
            return false;
         } 
//          else { std::cerr << "[" << *i << "=" << c << "]"; }
      }
//       std::cerr << "MATCH" << std::endl;
      return i == value.end();
   }
};



// ---*--- Token parser ---*---

Token::Information::Information(const Token &_token, const std::string &_content) : token(_token), text(_content) {}

struct Token_p : public RuleImpl, public SemanticFunction {
   const Token &token;
   Token_p(const Token &_token) : token(_token) {}
   
   bool match(Scanner &s) const {
      Token::Information info = s.get_token();
      bool r = (info.token == token);
      if (r) s.context.add_action(*this, new Token::Information(token, info.text));
      return r;
   }
   
   virtual void execute(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {
      stack.push(c);
   }
   
   virtual void cleanup(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {
      stack.pop();
   }

};

// ------------------------------------ Predefined parsers ---

struct Digit : public RuleImpl {
   bool match(Scanner &s) const {
      return !s.at_end() && isdigit(s.get_char());
   }
};

struct XDigit : public RuleImpl {
   bool match(Scanner &s) const {
      return !s.at_end() && isxdigit(s.get_char());
   }
};

struct Alpha : public RuleImpl {
   bool match(Scanner &s) const {
      return !s.at_end() && isalpha(s.get_char());
   }
};

struct Space : public RuleImpl {
   bool match(Scanner &s) const {
      return !s.at_end() && isspace(s.get_char());
   }
};

struct Anychar : public RuleImpl {
   bool match(Scanner &s) const {
      if (s.at_end()) return false;
      s.get_char();
      return true;
   }
};

struct True_p : public RuleImpl{
   bool match(Scanner &s) const {
      return true;
   }
};


Rule digit(new Digit());
Rule digit_p(new Digit());
Rule xdigit_p(new XDigit());
Rule alpha_p(new Alpha());
Rule space_p(new Space());
Rule anychar_p(new Anychar());
Rule true_p(new True_p());

// ------------------------------------ Parser modifiers ---

struct _Epsilon_p : public UnaryOperator {
   public:
      _Epsilon_p(const Rule &_rule) : UnaryOperator(_rule) {}
      bool match(Scanner &s) const {
         Scanner::Iterator start = s.get_pos();
         bool b = rule.parse(s);
         s.move(start);
         return b;
      }
};

Rule Epsilon_p::operator()(const Rule &x) {
   return Rule(new _Epsilon_p(x));
}

Epsilon_p epsilon_p;

// ------------------------------------ Rule ---

Rule Rule::operator-(const Rule &x) { return Rule(new ButNode(*this,x));}

Rule Rule::operator|(const Rule &x) { return Rule(new Alternative(*this, x)); }
Rule Rule::operator+() { return Rule(new Positive(*this)); }
Rule Rule::operator>>(const Rule &x) { return Rule(new Sequence(*this,x)); }
Rule Rule::operator*() { return Rule(new KleeneStar(*this)); }
Rule Rule::operator~() { return Rule(new Not(*this)); }


Rule::~Rule() { }
Rule::Rule() {}
Rule::Rule(const Rule &_other) : rule(_other.rule) {}
Rule::Rule(RuleImpl *_rule) : rule(_rule) {}

Rule::Rule(const Token &_token) : rule(new Token_p(_token)) {}
Rule::Rule(const char *s) : rule(new Literal(s)) {}
Rule::Rule(const std::string &s) : rule(new Literal(s)) {}
Rule::Rule(char c) : rule(new Literal(c)) {}

RuleImpl *Rule::operator->() { return rule.get(); }
const RuleImpl *Rule::operator->() const { return rule.get(); }

Rule operator>>(char c, const Rule &x) {
   return Rule(new Sequence(Rule(new Literal(c)),x));
}

Rule operator>>(const std::string &s, const Rule &x) {
   return Rule(new Sequence(Rule(new Literal(s)),x));
}

bool Rule::parse(Scanner &s) const {
   if (rule.isValid()) {
      Scanner::Iterator start = s.get_pos();
      bool generate = s.context.do_generate_AST(false);
      s.skip();
      s.context.do_generate_AST(generate);
      bool r = rule->parse(s);
      return r;
   }
   return false;
}

Rule &Rule::operator=(const Rule &other) {
   rule = other.rule;
   return *this;
}


// -*- Semantic action


ParserContext::ActionTree::Node::Node(const SemanticFunction &r) : rule(r) {
}
       
SmartPointer<ParserContext::ActionTree::Node> ParserContext::add_action(const SemanticFunction &r, const SmartPointer<SemanticContext> &c) {
   if (!is_generating_action_tree()) return 0;
   
   SmartPointer<ParserContext::ActionTree::Node> node = new ActionTree::Node(r);
   node->context = c;
         
   if (action_tree.last.isValid()) {
         assert(!action_tree.last->next.isValid());
         action_tree.last->next = node;
         action_tree.last = action_tree.last->next;
   } else {
         assert(!action_tree.root.isValid());
         action_tree.last = node;
         action_tree.root = action_tree.last;
   }
   return node;
}

void ParserContext::add_action(const SemanticFunction &r, const SmartPointer<SemanticContext> &c, const ActionTree &t) {
   if (!is_generating_action_tree()) return;
   ActionTree v = remove_action_tree();
   action_tree = t;
   SmartPointer<ParserContext::ActionTree::Node>  node = add_action(r,c);
   node->context = c;
   node->first_child = v.root;
}


ParserContext::ActionTree ParserContext::remove_action_tree() {
   if (!is_generating_action_tree()) return ParserContext::ActionTree();
   ActionTree t = action_tree;
   action_tree  = ActionTree();
   return t;
}

void ParserContext::restore_action_tree(const ActionTree &t) { 
   if (!is_generating_action_tree()) return;
   action_tree = t; 
}

void ParserContext::execute_actions() {
   if (action_tree.root.isValid()) {
      SemanticContext::Stack stack;
      action_tree.root->execute(stack);
   }
}
       
void ParserContext::ActionTree::Node::execute(SemanticContext::Stack &stack) {
//    std::cerr << std::string(tttt++,' ')  << "Executing action " << &rule << ", " << typeid(rule).name() << std::endl;
   rule.execute(stack, context);
   if (first_child.isValid()) first_child->execute(stack);
   rule.cleanup(stack, context);
   if (next.isValid()) next->execute(stack);
}

SemanticContext::~SemanticContext() {}
SmartPointer<SemanticContext> SemanticContext::Stack::get_last() { return last; }
SmartPointer<SemanticContext> SemanticContext::Stack::pop() { 
   last =  top();
   SemanticContextStack::pop(); 
   return last; 
}

struct SemanticContextConstructionRule : public UnaryOperator, public SemanticFunction {
   SemanticContext::Constructor constructor;
   SemanticContextConstructionRule(const Rule &_rule, SemanticContext::Constructor c) : UnaryOperator(_rule), constructor(c) {}
   bool match(Scanner &s) const {
      ParserContext::ActionTree t = s.context.remove_action_tree();
      bool r = rule.parse(s);
      if (r) 
         s.context.add_action(*this, 0, t);
      else
         s.context.restore_action_tree(t);
      
      return r;
   }
   virtual void execute(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {
      stack.push(constructor());
   }
   virtual void cleanup(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {
      stack.pop();
   }
};
Rule operator<<(SemanticContext::Constructor c, const Rule &r) {
   return Rule(new SemanticContextConstructionRule(r,c));
}








// ------------------------------------ GrammarRule ---

AST::Node::Node(const Token &_token) : token(_token), parent(0) {
}

namespace {
   void tabulate(ParserContext &c) {
      std::cerr << std::string(c.level,' ');
   };
};

std::string RuleContainer::get_name() const {
   return name;
}


bool RuleContainer::match(Scanner &s) const {   
   Soul::ParserContext &c = s.context;
   uint on = c.operation_number + 1;
   if (debug) {
      c.level++;
      c.operation_number++;
      tabulate(c);
      std::cerr << "\\ [" << c.operation_number << "] " << (rule.isValid() ? "" : "[Empty] ") << name << " ";
      s.print_context(std::cerr, s.get_pos(), 10);
      std::cerr << std::endl;
   }
   bool r = false;
   Scanner::Iterator begin = s.get_pos();
   AST ast = c.current; c.current = AST();
   if (rule.isValid()) r = rule->parse(s);
   
   if (r && c.is_generating_AST()) {
      if (ast.root) {
         if (ast.is_root) ast.root->add_as_last_child(c.current.root);
         else ast.root->add_as_last_sibling(c.current.root);
         c.current = ast;
      }
   } else c.current = ast;
      
   if (debug) {
      Scanner::Iterator end = s.get_pos();
      tabulate(c);
      std::cerr <<  (r ? '/' : '#') << " [" << on << "] " << name << " ";
      s.print_context(std::cerr,begin);
      std::cerr << std::endl;
      c.level--;
   }
   
   
   return r;
}


RuleContainer::RuleContainer() : debug(false) {}

void RuleContainer::set_debug(bool b) {
   debug = b;
}
void RuleContainer::set_name(const std::string &s) {
   name = s;
}

GrammarRule::GrammarRule() : Rule(new RuleContainer) {}

Rule &GrammarRule::operator=(const Rule &other) {
   dynamic_cast<RuleContainer&>(*rule).rule = other.rule;
   return *this;
}
Rule &GrammarRule::operator=(const GrammarRule &other) {
   dynamic_cast<RuleContainer&>(*rule).rule = other.rule;
   return *this;
}


void GrammarRule::set_name(const std::string &s) {
   dynamic_cast<RuleContainer&>(*rule).set_name(s);
}
void GrammarRule::set_debug(bool b) {
   dynamic_cast<RuleContainer&>(*rule).set_debug(b);
}

// ------------------------------------ RuleImpl ---

std::string RuleImpl::get_name() const { return "[anonymous]"; }
RuleImpl::RuleImpl() {}
RuleImpl::~RuleImpl() {}

bool RuleImpl::match(Scanner &s) const { return false; }


/** Parse */
bool RuleImpl::parse(Scanner &s) const {
/*   std::cerr << std::string(c.level+1,' ') << " " << typeid(*this).name() << " ";
   s.print_context(std::cerr);
   std::cerr << std::endl;*/
   
   return match(s);
};

SemanticFunction::~SemanticFunction() {}

// ------------------------------------ Grammar ---

Grammar::Grammar() {}

// ------------------------------------ Scanner ---

ScannerIteratorImpl::~ScannerIteratorImpl() {}
Scanner::Scanner() : is_skipping(false) {}
Scanner::~Scanner() {}

bool Scanner::set_do_skip(bool b) {
   bool saved = is_skipping;
   is_skipping = b;
   return saved;
}
bool Scanner::do_skip() const {
   return is_skipping;
}


void Scanner::set_skipper(const Rule &rule) {
   skipper = rule;
}

void Scanner::skip() {
   if (do_skip()) {
      bool saved = set_do_skip(false);
      Scanner::Iterator last = get_pos();
      while (skipper.parse(*this)) last = get_pos();
      move(last);
      set_do_skip(saved);
   }
}

char Scanner::get_char() {
   throw std::logic_error("This parser is not a stream of char");
}

wchar_t Scanner::get_wchar() {
   throw std::logic_error("This parser is not a stream of wchar");
}
    
const Token::Information Scanner::get_token() {
   throw std::logic_error("This parser is not a stream of tokens");
}

// ------------------------------------ AST Scanner ---

Rule ASTMatch::operator()(const Token &t, const Rule &r) {
   return ASTScanner::tree_start >> t >> r >> ASTScanner::tree_end;
}

Rule ASTMatch::operator()(const Token &t) {
   return (ASTScanner::tree_start >> t >> ASTScanner::tree_end) | t;
}

Rule ASTMatch::operator()(const Rule &r) {
   return (ASTScanner::tree_start >> r >> ASTScanner::tree_end);
}

ASTMatch tree_p;

struct ASTScanner::Iterator : public ScannerIteratorImpl {
   ASTScanner::Position position;
   Iterator(const Position &p) : position(p) {}
   long operator-(const ScannerIteratorImpl &other) {
      return position.rank - dynamic_cast<const Iterator &>(other).position.rank;
   }

};

const Token ASTScanner::tree_start("<tree>"), ASTScanner::tree_end("</tree>");

ASTScanner::Position::Position(const AST::Node *p, State s, long r) :
      node(p), state(s), rank(r)
{}

ASTScanner::ASTScanner(const AST &_ast) : ast(_ast), position(ast.root.get(), Position::start, 0) {
}

Scanner::Iterator ASTScanner::get_pos() const {
   return Scanner::Iterator(new Iterator(position));
}

bool ASTScanner::at_end(const Position &p) const { 
   return p.node == 0 && p.state == Position::normal;
}

bool ASTScanner::at_end() const { 
   return at_end(position);
}

void ASTScanner::move(const Scanner::Iterator &to) {
   position = dynamic_cast<const Iterator&>(*to.get()).position;
}

ASTScanner::Position ASTScanner::get_next(const Position &p) const {
   Position q = p;
   q.rank++;
   switch(q.state) {
      case Position::start: 
         q.state = Position::normal; break;
      case Position::end:
         if (!q.node) { q.state = Position::normal; break; }
         q.node = q.node->parent;
         if (!q.node) break;
         if (q.node->next.isValid()) { q.node = q.node->next.get(); q.state = Position::normal; }
         break;
      case Position::normal:
         assert(q.node != 0);
         if (q.node->first_child.isValid()) {
            q.node = q.node->first_child.get();
            q.state = Position::start;
         } else if (q.node->next.isValid()) {
            q.node = q.node->next.get();
         } else {
            // Nothing is valid, close & go to parent
            q.node = q.node->parent;
            q.state = Position::end;
         }
         break;
   }
   return q;
}

void ASTScanner::print_context(std::ostream &out, Scanner::Iterator begin, size_t size) const {
   Position i = dynamic_cast<const Iterator&>(*begin.get()).position;
   size_t n = 0;
   for(; !at_end(i) && (size == 0 || n < size); i = get_next(i), n++) 
      out  << i.get_token().get_name() << " ";
   if (i.node != 0)
      out << "...";
}

void ASTScanner::print_context(std::ostream &out, Scanner::Iterator begin, Scanner::Iterator _end) const {
   const Position & p = dynamic_cast<const Iterator&>(*_end.get()).position;
   for(Position i = dynamic_cast<const Iterator&>(*begin.get()).position; p.rank > i.rank ; i = get_next(i))
      out  << i.get_token().get_name() << " ";
}

const Token::Information ASTScanner::get_token() {
   if (at_end()) throw std::runtime_error("End of stream");
   const Token::Information information(position.get_token(), position.node ? position.node->content : "");
   position = get_next(position);
   return information;
}


// ------------------------------------ Directive ---

Rule Directive::operator[](const Rule &x) { return x; }
Directive root_node_d;

struct _Lexeme_d : public UnaryOperator {
   _Lexeme_d(const Rule &_rule) : UnaryOperator(_rule) {}
   bool match(Scanner &s) const {
      bool old_skip = s.set_do_skip(false);
      bool b = rule.parse(s);
      s.set_do_skip(old_skip);
      return b;
   }
};

Rule Lexeme_d::operator[](const Rule &x) {
   return Rule(new _Lexeme_d(x)); 
}
Lexeme_d lexeme_d;

// -*- phrase_d

struct _Phrase_d : public UnaryOperator {
   _Phrase_d(const Rule &_rule) : UnaryOperator(_rule) {}
   bool match(Scanner &s) const {
      bool old_skip = s.set_do_skip(true);
      bool b = rule.parse(s);
      s.set_do_skip(old_skip);
      return b;
   }
};

Rule Phrase_d::operator[](const Rule &x) {
   return Rule(new _Phrase_d(x)); 
}
Phrase_d phrase_d;


// -*- phrase_d

namespace {
   void longest_d_r(const Rule &x) {
      if (Alternative *a = dynamic_cast<Alternative*>(x.rule.get())) {
         a->mode = Alternative::longest;
         longest_d_r(a->left);
         longest_d_r(a->right);
      }
   }
}

Rule Longest_d::operator[](const Rule &x) {
   longest_d_r(x);
   return x;
}
Longest_d longest_d;


// ------------------------------------ ParserContext ---

AST::AST() : is_root(false) {}
void AST::prepare() const {
   root->set_parent(0);
}
void AST::Node::set_parent(const Node *p) const {
   parent = p;
   if (next.isValid()) next->set_parent(p);
   if (first_child.isValid()) first_child->set_parent(this);
}

ParserContext::ParserContext() : generate_AST(false), generate_action_tree(true), level(0), operation_number(0)  {}

void ParserContext::print_parse_tree(std::ostream &out, const Scanner &s) {   
   print_parse_tree(out, s, level, current.root);
}

void ParserContext::print_parse_tree(std::ostream &out, const Scanner &s, uint level, const SmartPointer<AST::Node> &t) {
   if (!t.isValid()) { out << "EMPTY!\n"; return; }
   out << std::string(level, ' ') << "[" << t->token.get_name() << "," << t.get()<<  "] " << t->content <<  std::endl;
   if (t->first_child.isValid()) print_parse_tree(out, s, level+1, t->first_child);
   if (t->next.isValid()) print_parse_tree(out, s, level, t->next);
}


// ------------------------------------ Is a ---

struct Is_a_p : public UnaryOperator {
   Rule is_a_rule;
   Is_a_p(const Rule &_mrule, const Rule &_rule) : UnaryOperator(_rule), is_a_rule(_mrule) {}
   bool match(Scanner &s) const {
      // Try to match the "is a"
      Scanner::Iterator start = s.get_pos();
      if (!rule.parse(s)) return false;
      Scanner::Iterator after = s.get_pos();

      s.move(start);
      if (!is_a_rule.parse(s)) { s.move(after); return false; }
      if ((*s.get_pos() - *after) != 0) { s.move(after); return false; }
      
      return true;
   }
};

Is_a::Is_a(const Rule &_rule) : rule(_rule) {}
Rule Is_a::operator()(const Rule &x) {
   return Rule(new Is_a_p(rule,x));
}




// ------------------------------------ AST, tokens ---

namespace {
   // Add a node to the AST tree
   void construct_tree(const std::string &content, const AST &saved_tree, ParserContext &c, const Token &r, bool root_node) {
      // Build ourselves
      SmartPointer<AST::Node> selftree = new AST::Node(r);
      selftree->content = content;
      
      if (root_node) {
         selftree->add_as_last_child(saved_tree.root);
         c.current.root = selftree;
         c.current.is_root = root_node;
      } else {
         if (saved_tree.root.isValid()) {
            selftree->next = c.current.root;
            if (saved_tree.is_root) saved_tree.root->add_as_last_child(selftree);
            else saved_tree.root->add_as_last_sibling(selftree);
            c.current = saved_tree;
         } else {
            c.current.root = selftree;
            c.current.is_root = root_node;
         }
      }
   }
}

/** Makes a node become the root in an AST tree */
class MakeRoot: public UnaryOperator {
   public:
      MakeRoot(const Rule &_rule) : UnaryOperator(_rule) {}
      bool match(Scanner &s) const {
         if (!s.context.is_generating_AST()) return rule.parse(s);
            AST saved_tree = s.context.current; 
            s.context.current = AST();
            bool r = rule.parse(s);
            if (r) { 
               // The generated AST tree must be a token
               if (s.context.current.root->next.isValid() || s.context.current.root->first_child.isValid()) 
                  throw std::logic_error("Trying to make a non-token the root of an AST tree");
               AST ast = s.context.current;
               s.context.current = AST();
               construct_tree(ast.root->content, saved_tree, s.context, ast.root->token, true);
            } else s.context.current = saved_tree;
         return r;
      }
};
Rule Rule::operator++(int) { return Rule(new MakeRoot(*this)); }


struct _Token : public UnaryOperator {
   const Token &token;
   Rule is_a_rule;
   bool root_node;
   
   _Token(const Token &_token, const Rule &_rule, bool _root_node) : UnaryOperator(_rule), token(_token), root_node(_root_node) {}
   bool match(Scanner &s) const {
      if (!s.context.is_generating_AST()) return rule.parse(s);
      
      bool b = s.context.do_generate_AST(false);
      Scanner::Iterator begin = s.get_pos();
      AST saved_tree = s.context.current; 
      s.context.current = AST();
      bool r = rule.parse(s);
      Scanner::Iterator end = s.get_pos();
      
      s.context.do_generate_AST(b);
      std::ostringstream out;s.print_context(out,begin,end);
      if (r) construct_tree(std::string(out.str()), saved_tree, s.context, token, root_node);
      else s.context.current = saved_tree;
      return r;
   }
};

Token::Token(const std::string &_name) : name(_name) {}
Token::~Token() {}
Rule Token::operator()() const { 
   return Rule(new _Token(*this, true_p, false));
}

std::string Token::get_name() const { 
   return name;
}

Rule Token::operator[](const Rule &x) const {
   return Rule(new _Token(*this, x, false));
}
Rule Token::operator^(const Rule &x) const {
   return Rule(new _Token(*this, x, true));
}

}
