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


#ifndef PARSER_H
#define PARSER_H

#include <iostream>
#include <string>
#include "smartpointer.h"

namespace Soul {


class Scanner;


// forward declarations
class ARule;


/** Tokens */
class Token {
   std::string name;
   public:
      Token(const std::string &_name);
      virtual ~Token();
      std::string get_name() const;
      ARule operator[](const ARule &) const;
      ARule operator^(const ARule &) const;
      inline bool operator==(const Token &other) const { return &other == this; }
};


/**
 * A rule
 */
class Rule {
public:
    Rule();
    virtual ~Rule();
    friend class ARule;
    virtual bool match(Scanner &) const;
    bool parse(Scanner &) const;
    virtual std::string get_name() const;
};



/**
 * A rule
 */
class ARule {
    Rule *operator->();
    const Rule *operator->() const;
public:
    SmartPointer<Rule> rule;
    ARule(const ARule &);
    explicit ARule(Rule *);
    virtual ~ARule();

    ARule(const std::string &);
    ARule(const char *);
    ARule(char);
    ARule(const Token &);

    ARule();

    ARule operator!();
    ARule operator+();

    ARule operator-(const ARule&);
    ARule operator|(const ARule&);
    ARule operator>>(const ARule&);
    ARule operator*();
    ARule operator~();

    ARule operator++(int);

    virtual ARule &operator=(const ARule &);

    bool parse(Scanner &) const;
};

/**
 * A rule that contains... a rule
 */
struct RuleContainer : public Rule {
    bool debug;
    std::string name;
public:
    SmartPointer<Rule> rule;
    RuleContainer();
    bool match(Scanner &) const;

    void set_name(const std::string &s);
    virtual std::string get_name() const;
    void set_debug(bool);
};

/**
 * A rule that can be redefined (aka grammar rules)
 */
class GrammarRule : public ARule {
public:
    GrammarRule();
    ARule &operator=(const ARule &);
    ARule &operator=(const GrammarRule &);
    void set_name(const std::string &s);
    void set_debug(bool);
};

// Predefined parsers
extern ARule digit;
extern ARule digit_p;
extern ARule xdigit_p;
extern ARule alpha_p;
extern ARule space_p;
extern ARule anychar_p;

/** A parser that is always true and do not consume input */
extern ARule true_p;

ARule operator>>(char, const ARule &);
ARule operator>>(const std::string &, const ARule &);
inline ARule operator>>(const Token &token, const ARule &r) { return ARule(token) >> r; }

class Directive {
public:
    ARule operator[](const ARule &);
};
extern Directive root_node_d;

struct Lexeme_d {
    ARule operator[](const ARule &);
};
extern Lexeme_d lexeme_d;

struct Phrase_d {
    ARule operator[](const ARule &);
};
extern Phrase_d phrase_d;

struct Longest_d {
    ARule operator[](const ARule &);
};
extern Longest_d longest_d;




// ---- Null parsers ----

struct Epsilon_p {
    ARule operator()(const ARule &);
};
extern Epsilon_p epsilon_p;

// ---- "is a" ----

class Is_a {
    ARule rule;
public:
    Is_a(const ARule &_rule);
    ARule operator()(const ARule &);
};


class Grammar : public Rule {
public:
    Grammar();
    virtual const ARule &start() const = 0;
    inline bool parse(Scanner &s) const {
        return start().parse(s);
    };
};


struct ScannerIteratorImpl {
    virtual ~ScannerIteratorImpl();
    virtual long operator-(const ScannerIteratorImpl &) = 0;
};

typedef SmartPointer<ScannerIteratorImpl> ScannerIterator;

/** AST Tree */

struct AST {
   struct Node {
      SmartPointer<Node> next, first_child;
      const Node *parent;
      const Token &token;
      ScannerIterator start, end;
      Node(const Token&);
        
      inline void add_as_last_child(const SmartPointer<Node> &x) {
         if (!x.isValid()) return;
         if (!first_child) first_child = x;
         else {
            Node * i = first_child.get();
            while (i->next.isValid()) i = i->next.get();
            i->next = x;
         }
         x->parent = this;
      }
        
      inline void add_as_last_sibling(const SmartPointer<Node> &x) {
         if (!x.isValid()) return;
         Node * i = this;
         while (i->next.isValid()) i = i->next.get();
         i->next = x;
         x->parent = parent;
      }
        
        
   };
   
   SmartPointer<Node> root;
   
   /** Is this node a root? */
   bool is_root;
   AST();
};
    
/** The context of the parse */
class Context {
public:

private:
   void print_parse_tree(std::ostream &, const Scanner &, uint level, const SmartPointer<AST::Node> &);
    /** Do we generate an AST tree */
    bool generate_AST;
public:
    uint level;
    uint operation_number;

    inline bool is_generating_AST() const { return generate_AST; }
    
    inline bool do_generate_AST(bool b) { 
       bool c = generate_AST;
       generate_AST = b;
       return c;
    }
    

    AST current;

    Context();
    void print_parse_tree(std::ostream &, const Scanner &);
};


/**
 * The scanner
 */
class Scanner {
protected:
    ARule skipper;
    /** Are we using the skipping grammar before each new char? */
    bool is_skipping;
public:
    Context context;
    typedef ScannerIterator Iterator;

    Scanner();
    virtual ~Scanner();

    
    /**
     * Test wether the stream is finished
     * @return a boolean which is true if it is the end of the stream
     */
    virtual bool at_end() const = 0;

    /** Get a char and move forward */
    virtual char get_char();

    /** Get a wide char and move forward */
    virtual wchar_t get_wchar();
    
    /** Get a char and move forward */
    virtual const Token &get_token();

    /** Get the current position */
    virtual Iterator get_pos() const = 0;
    
    /** Move to a previously saved position */
    virtual void move(const Iterator &) = 0;

    
    /** For debuging */
    virtual void print_context(std::ostream &out, Scanner::Iterator begin, size_t number = 0) const = 0;
    virtual void print_context(std::ostream &out, Scanner::Iterator begin, Scanner::Iterator end) const = 0;


    /** \name Skipping */
    //@{
    /** Set the no skipping level */
    bool set_do_skip(bool);
    
    /** Set the no skipping level */
    bool do_skip() const;

    /** Set skipper */
    void set_skipper(const ARule &_rule);

    /** Skip */
    void skip();
    //@}
};

/** 
 * An AST scanner
 */
class ASTScanner : public Scanner {
      const AST &ast;
      struct Iterator;
      
      struct Position {
         enum State { start, end, normal };
         const AST::Node* node;
         State state;
         long rank;
         inline const Token &get_token() const {
            switch(state) {
               case Position::normal: return node->token;
               case Position::start: return ASTScanner::tree_start;
               case Position::end: return ASTScanner::tree_end;
            }
            throw;
         }

         Position(const AST::Node *p, State s, long r);
      };
      Position position;
      
      bool at_end(const Position &p) const;
      Position get_next(const Position &) const;
   public:
      /** 
       * tree_start and tree_end are imaginary tokens that help delimiting the tree 
       * and are generated by the scanner
       */
      static const Token tree_start, tree_end;
      
      ASTScanner(const AST &ast);
      
      Scanner::Iterator get_pos() const;
      bool at_end() const;
      void move(const Scanner::Iterator &to);
      void print_context(std::ostream &out, Scanner::Iterator begin, size_t number = 0) const;
      void print_context(std::ostream &out, Scanner::Iterator begin, Scanner::Iterator end) const;
      const Token &get_token();
};

// Parsers for AST trees
struct ASTMatch {
   ARule operator[](const ARule &);
};
extern ASTMatch tree_p;
 
/**
 * A simple iterator-based scanner
 */
template <class T>
class IteratorScanner : public Scanner {
    T position;
    T end;
struct Iterator_t : public ScannerIteratorImpl {
        T position;
        Iterator_t(const T &i) : position(i) {}
        ~Iterator_t() {}
        virtual long operator-(const ScannerIteratorImpl &other) {
            return position - dynamic_cast<const Iterator_t&>(other).position;
        }

    };
public:
    IteratorScanner(const T &_begin, const T &_end) : position(_begin), end(_end) {}
    ;
    bool at_end() const {
        return end == position;
    }

    char get_char() {
        assert(position != end);
        char c = *position;
        position++;
        return c;
    };
    wchar_t get_wchar() {
        return 0;
    };

    virtual Iterator get_pos() const {
        return new Iterator_t(position);
    }

    virtual void move(const Iterator &i) {
        const Iterator_t &x = dynamic_cast<const typename IteratorScanner<T>::Iterator_t&>(*i.get());
        position = x.position;
    }
    
    void print_context(std::ostream &out, Scanner::Iterator begin, Scanner::Iterator _end) const {
       T i= dynamic_cast<const typename IteratorScanner<T>::Iterator_t&>(*begin.get()).position;
       T end = dynamic_cast<const typename IteratorScanner<T>::Iterator_t&>(*_end.get()).position;
       while (i != end) {
          if (*i == '\n')
             out << "\\n";
          else
             out << *i;          
          i++;
       }
    }
    
    void print_context(std::ostream &out, Scanner::Iterator begin, size_t number = 0) const {
            T i = dynamic_cast<const typename IteratorScanner<T>::Iterator_t&>(*begin.get()).position;
            size_t n = 0;
            for(; i != end && (number == 0 || n < number); i++, n++) {
                if (*i == '\n')
                    out << "\\n";
                else
                    out << *i;
            }
            if (i != end)
                out << "...";
    }


};


}
#endif
