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

#include <cstdlib> 
#include "parser.h"

namespace Soul {

struct UnaryOperator : public Rule {
    const ARule rule;
    UnaryOperator(const ARule &_rule) : rule(_rule) {}
};

class Positive : public UnaryOperator {
   public:
      Positive(const ARule &_rule) : UnaryOperator(_rule) {}
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
    KleeneStar(const ARule &_rule) : UnaryOperator(_rule) {}
    bool match(Scanner &s) const {
       Scanner::Iterator last = s.get_pos();
       while (rule.parse(s)) last = s.get_pos();
       s.move(last);
       return true;
    }
};

class Optional : public UnaryOperator {
public:
    Optional(const ARule &_rule) : UnaryOperator(_rule) {}
    bool match(Scanner &s) const {
       Scanner::Iterator last = s.get_pos();
       if (!rule.parse(s)) s.move(last);
       return true;
    }
};

class Not : public UnaryOperator {
   public:
      Not(const ARule &_rule) : UnaryOperator(_rule) {}
      bool match(Scanner &s) const {
         return !rule.parse(s);
      }
};



struct BinaryOperator : public Rule {
   const ARule left;
   const ARule right;
   BinaryOperator(const ARule &l, const ARule &r) : left(l), right(r) {}
};


struct Alternative : public BinaryOperator {
   /** Return the longest alternative?*/
   enum { normal, longest, shortest } mode;
   Alternative(const ARule &l, const ARule &r) : BinaryOperator(l,r), mode(normal) {}
   
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
   Sequence(const ARule &l, const ARule &r) : BinaryOperator(l,r) {}
   bool match(Scanner &s) const {
      return left.parse(s) && right.parse(s);
   }
};

struct ButNode : public BinaryOperator {
   ButNode(const ARule &l, const ARule &r) : BinaryOperator(l,r) {}
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

ARule ARule::operator!() {
    return ARule((Rule*)new Optional(*this));
}

struct Literal : public Rule {
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

/**
 * Match a token
 */
struct Token_p : public Rule {
   const Token &token;
   Token_p(const Token &_token) : token(_token) {}
   bool match(Scanner &s) const {
      return s.get_token() == token;
   }
};

// ------------------------------------ Predefined parsers ---

struct Digit : public Rule {
   bool match(Scanner &s) const {
      return !s.at_end() && isdigit(s.get_char());
   }
};

struct XDigit : public Rule {
   bool match(Scanner &s) const {
      return !s.at_end() && isxdigit(s.get_char());
   }
};

struct Alpha : public Rule {
   bool match(Scanner &s) const {
      return !s.at_end() && isalpha(s.get_char());
   }
};

struct Space : public Rule {
   bool match(Scanner &s) const {
      return !s.at_end() && isspace(s.get_char());
   }
};

struct Anychar : public Rule {
   bool match(Scanner &s) const {
      if (s.at_end()) return false;
      s.get_char();
      return true;
   }
};

struct True_p : public Rule{
   bool match(Scanner &s) const {
      return true;
   }
};


ARule digit(new Digit());
ARule digit_p(new Digit());
ARule xdigit_p(new XDigit());
ARule alpha_p(new Alpha());
ARule space_p(new Space());
ARule anychar_p(new Anychar());
ARule true_p(new True_p());

// ------------------------------------ Parser modifiers ---

struct _Epsilon_p : public UnaryOperator {
   public:
      _Epsilon_p(const ARule &_rule) : UnaryOperator(_rule) {}
      bool match(Scanner &s) const {
         Scanner::Iterator start = s.get_pos();
         bool b = rule.parse(s);
         s.move(start);
         return b;
      }
};

ARule Epsilon_p::operator()(const ARule &x) {
      return ARule(new _Epsilon_p(x));
}

Epsilon_p epsilon_p;

// ------------------------------------ ARule ---

ARule ARule::operator-(const ARule &x) { return ARule(new ButNode(*this,x));}

ARule ARule::operator|(const ARule &x) { return ARule(new Alternative(*this, x)); }
ARule ARule::operator+() { return ARule(new Positive(*this)); }
ARule ARule::operator>>(const ARule &x) { return ARule(new Sequence(*this,x)); }
ARule ARule::operator*() { return ARule(new KleeneStar(*this)); }
ARule ARule::operator~() { return ARule(new Not(*this)); }


ARule::~ARule() { }
ARule::ARule() {}
ARule::ARule(const ARule &_other) : rule(_other.rule) {}
ARule::ARule(Rule *_rule) : rule(_rule) {}

ARule::ARule(const Token &_token) : rule(new Token_p(_token)) {}
ARule::ARule(const char *s) : rule(new Literal(s)) {}
ARule::ARule(const std::string &s) : rule(new Literal(s)) {}
ARule::ARule(char c) : rule(new Literal(c)) {}

Rule *ARule::operator->() { return rule.get(); }
const Rule *ARule::operator->() const { return rule.get(); }

ARule operator>>(char c, const ARule &x) {
   return ARule(new Sequence(ARule(new Literal(c)),x));
}

ARule operator>>(const std::string &s, const ARule &x) {
   return ARule(new Sequence(ARule(new Literal(s)),x));
}

bool ARule::parse(Scanner &s) const {
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

ARule &ARule::operator=(const ARule &other) {
   rule = other.rule;
   return *this;
}


// ------------------------------------ GrammarRule ---

AST::Node::Node(const Token &_token) : token(_token) {
}

namespace {
   void tabulate(Context &c) {
      std::cerr << std::string(c.level,' ');
   };
};

std::string RuleContainer::get_name() const {
   return name;
}


bool RuleContainer::match(Scanner &s) const {   
   Context &c = s.context;
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

GrammarRule::GrammarRule() : ARule(new RuleContainer) {}

ARule &GrammarRule::operator=(const ARule &other) {
   dynamic_cast<RuleContainer&>(*rule).rule = other.rule;
   return *this;
}
ARule &GrammarRule::operator=(const GrammarRule &other) {
   dynamic_cast<RuleContainer&>(*rule).rule = other.rule;
   return *this;
}


void GrammarRule::set_name(const std::string &s) {
   dynamic_cast<RuleContainer&>(*rule).set_name(s);
}
void GrammarRule::set_debug(bool b) {
   dynamic_cast<RuleContainer&>(*rule).set_debug(b);
}

// ------------------------------------ Rule ---

std::string Rule::get_name() const { return "[anonymous]"; }
Rule::Rule() {}
Rule::~Rule() {}

bool Rule::match(Scanner &s) const { return false; }

/** Parse */
bool Rule::parse(Scanner &s) const {
/*   std::cerr << std::string(c.level+1,' ') << " " << typeid(*this).name() << " ";
   s.print_context(std::cerr);
   std::cerr << std::endl;*/
   return match(s);
};



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


void Scanner::set_skipper(const ARule &rule) {
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
    
const Token &Scanner::get_token() {
   throw std::logic_error("This parser is not a stream of tokens");
}

// ------------------------------------ AST Scanner ---

ARule ASTMatch::operator[](const ARule &r) {
   return ASTScanner::tree_start >> r >> ASTScanner::tree_end;
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

const Token &ASTScanner::get_token() {
   if (at_end()) throw std::runtime_error("End of stream");
   const Token &token = position.get_token();
   position = get_next(position);
   return token;
}


// ------------------------------------ Directive ---

ARule Directive::operator[](const ARule &x) { return x; }
Directive root_node_d;

struct _Lexeme_d : public UnaryOperator {
   _Lexeme_d(const ARule &_rule) : UnaryOperator(_rule) {}
   bool match(Scanner &s) const {
      bool old_skip = s.set_do_skip(false);
      bool b = rule.parse(s);
      s.set_do_skip(old_skip);
      return b;
   }
};

ARule Lexeme_d::operator[](const ARule &x) {
   return ARule(new _Lexeme_d(x)); 
}
Lexeme_d lexeme_d;

// -*- phrase_d

struct _Phrase_d : public UnaryOperator {
   _Phrase_d(const ARule &_rule) : UnaryOperator(_rule) {}
   bool match(Scanner &s) const {
      bool old_skip = s.set_do_skip(true);
      bool b = rule.parse(s);
      s.set_do_skip(old_skip);
      return b;
   }
};

ARule Phrase_d::operator[](const ARule &x) {
   return ARule(new _Phrase_d(x)); 
}
Phrase_d phrase_d;


// -*- phrase_d

namespace {
   void longest_d_r(const ARule &x) {
      if (Alternative *a = dynamic_cast<Alternative*>(x.rule.get())) {
         a->mode = Alternative::longest;
         longest_d_r(a->left);
         longest_d_r(a->right);
      }
   }
}

ARule Longest_d::operator[](const ARule &x) {
   longest_d_r(x);
   return x;
}
Longest_d longest_d;


// ------------------------------------ Context ---

AST::AST() : is_root(false) {}

Context::Context() : generate_AST(false), level(0), operation_number(0)  {}

void Context::print_parse_tree(std::ostream &out, const Scanner &s) {   
   print_parse_tree(out, s, level, current.root);
}

void Context::print_parse_tree(std::ostream &out, const Scanner &s, uint level, const SmartPointer<AST::Node> &t) {
   if (!t.isValid()) { out << "EMPTY!\n"; return; }
   out << std::string(level, ' ') << "[" << t->token.get_name() <<  "] ";
   s.print_context(out, t->start,t->end);
   out << std::endl;
   if (t->first_child.isValid()) print_parse_tree(out, s, level+1, t->first_child);
   if (t->next.isValid()) print_parse_tree(out, s, level, t->next);
}


// ------------------------------------ Is a ---

struct Is_a_p : public UnaryOperator {
   ARule is_a_rule;
   Is_a_p(const ARule &_mrule, const ARule &_rule) : UnaryOperator(_rule), is_a_rule(_mrule) {}
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

Is_a::Is_a(const ARule &_rule) : rule(_rule) {}
ARule Is_a::operator()(const ARule &x) {
   return ARule(new Is_a_p(rule,x));
}




// ------------------------------------ AST, tokens ---

namespace {
   // Add a node to the AST tree
   void construct_tree(const ScannerIterator &begin, const ScannerIterator &end, const AST &saved_tree, Context &c, const Token &r, bool root_node) {
      // Build ourselves
      SmartPointer<AST::Node> selftree = new AST::Node(r);
      selftree->start = begin;
      selftree->end = end;
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
      MakeRoot(const ARule &_rule) : UnaryOperator(_rule) {}
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
               construct_tree(ast.root->start, ast.root->end, saved_tree, s.context, ast.root->token, true);
            } else s.context.current = saved_tree;
         return r;
      }
};
ARule ARule::operator++(int) { return ARule(new MakeRoot(*this)); }


struct _Token : public UnaryOperator {
   const Token &token;
   ARule is_a_rule;
   bool root_node;
   
   _Token(const Token &_token, const ARule &_rule, bool _root_node) : UnaryOperator(_rule), token(_token), root_node(_root_node) {}
   bool match(Scanner &s) const {
      if (!s.context.is_generating_AST()) return rule.parse(s);
      
      bool b = s.context.do_generate_AST(false);
      Scanner::Iterator begin = s.get_pos();
      AST saved_tree = s.context.current; 
      s.context.current = AST();
      bool r = rule.parse(s);
      Scanner::Iterator end = s.get_pos();
      
      s.context.do_generate_AST(b);
      if (r) construct_tree(begin, end, saved_tree, s.context, token, root_node);
      else s.context.current = saved_tree;
      return r;
   }
};

Token::Token(const std::string &_name) : name(_name) {}
Token::~Token() {}

std::string Token::get_name() const { return name; }
ARule Token::operator[](const ARule &x) const {
   return ARule(new _Token(*this, x, false));
}
ARule Token::operator^(const ARule &x) const {
   return ARule(new _Token(*this, x, true));
}

}
