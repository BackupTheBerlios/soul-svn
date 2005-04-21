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


#ifndef SOUL_PARSER_H
#define SOUL_PARSER_H

#include <iostream>
#include <stack>
#include <vector>
#include <string>

#include "smartpointer.h"

namespace Soul {


// forward declarations
class Scanner;
class Rule;
class Token;
class ParserContext;

/** An AST tree */
struct AST {
    void print(std::ostream &, bool dot_format);
    void print(std::ostream &, bool dot_format, uint level);

    SmartPointer<AST> next, first_child;
    const Token &token;
    std::string content;
    mutable AST *parent;

    /** Node state: root node, active root node or normal */
    enum { NORMAL = 1, ROOT = 2, ACTIVE=4 } state;

    inline bool is_root() const {
        return state & (ROOT | ACTIVE);
    }
    inline bool is_active() const {
        return state & ACTIVE;
    }

    AST(const Token&);

    void set_parent(AST*) const;

    /**
     * Replace this AST node with another one
     * @param x 
     */
    inline void replace(SmartPointer<AST> &x) {
        AST *i = x.get();
        while (i->next) {
            i->parent = parent;
            i = i->next.get();
        }
        i->parent = parent;
        i->next = next;
        if (parent) {
            i = parent->first_child.get();
            if (i == this)
                parent->first_child = x;
            else {
                while (i != 0 && i->next.get() != this)
                    i = i->next.get();
                assert(i!=0); // should not be the case
                i->next = x;
            }
        }
    }

    inline void add_as_last_child(const SmartPointer<AST> &x) {
        if (!x)
            return;
        if (!first_child)
            first_child = x;
        else {
            AST * i = first_child.get();
            while (i->next)
                i = i->next.get();
            i->next = x;
        }
        x->set_parent(this);
    }

    inline void add_as_first_child(const SmartPointer<AST> &x) {
        if (!x)
            return;
        AST * i = x.get();
        i->set_parent(this);
        while (i->next) {
            i = i->next.get();
            i->set_parent(this);
        }
        i->next = first_child;
        first_child = x;
    }

    inline void add_as_last_sibling(const SmartPointer<AST> &x) {
        if (!x)
            return;
        AST * i = this;
        while (i->next)
            i = i->next.get();
        i->next = x;
        x->parent = parent;
    }

};

struct ScannerIteratorImpl {
    virtual ~ScannerIteratorImpl();
    virtual long operator-(const ScannerIteratorImpl &) = 0;
};

typedef SmartPointer<ScannerIteratorImpl> ScannerIterator;


/** What is returned by a rule after a parse */
struct ParseResult {
    bool hit;

    inline operator bool() const {
        return hit;
    }
    inline ParseResult(bool _hit = false) : hit(_hit) {}

    /**
     * Add a new result at the end of this one (AST, ...)
     * @param r 
     * @return true if both parse results have a hit
     */
    ParseResult& update(const ParseResult &r);
};

/**
 * Rule semantic context
 * The semantic contexts can be created by rules and can be accessed by subrules and the last ancestor 
 * rule that created a context
 */
struct SemanticContext {
    typedef SmartPointer<SemanticContext> Ptr;
    virtual ~SemanticContext();

    /**
     * Called when the context is closed (but before it is destroyed)
     */
    virtual void finish();

    typedef void (*Function)(SemanticContext&);
    typedef SemanticContext * (*Constructor)();

    typedef std::stack<SmartPointer<SemanticContext> > SemanticContextStack;
    
    class Stack : public  SemanticContextStack {
        SmartPointer<SemanticContext> last;
    public:
        SmartPointer<SemanticContext> get_last();
        SmartPointer<SemanticContext> pop();
    };
};

/** 
 * Global context for actions 
 * An instance of this object is the only parameter given to actions
 */
class GlobalSemanticContext {
    /** The current context stack */
    SemanticContext::Stack stack;
public:
    virtual ~GlobalSemanticContext();
    template <class T> T& current() {
        if (stack.empty()) throw std::runtime_error("Stack of context is empty");
        return dynamic_cast<T&>(*stack.top());
    }
    template <class T> T& last() {
        if (stack.get_last()) return dynamic_cast<T&>(*stack.get_last());
        throw std::runtime_error("There is no last context");
    }
    inline SmartPointer<SemanticContext>  get_last() { return stack.get_last(); }
    inline SemanticContext::Stack &getStack() { return stack; }
    
    typedef void (*Function)(GlobalSemanticContext &);
    
};


/** Tokens */
class Token {
private:
    std::string name;
    Token(const Token&);
public:

struct Information : public SemanticContext {
        const Token &token;
        std::string text;
        Information(const Token &_token, const std::string &content);
    };

    Token(const std::string &_name);
    virtual ~Token();
    std::string get_name() const;
    Rule operator[](const Rule &) const;

    Rule operator()() const;

    Rule operator^(const Rule &) const;
    inline bool operator==(const Token &other) const {
        return &other == this;
    }
};



/**
 * A rule (the implementation)
 */
class RuleImpl {
    bool do_flatten;
public:

    RuleImpl();
    virtual ~RuleImpl();
    friend class Rule;
    virtual ParseResult match(ParserContext &) const;
    ParseResult parse(ParserContext &) const;
    virtual std::string get_name() const;
    void set_flatten(bool);
    inline bool get_flatten() {
        return do_flatten;
    }
};


/** An action in an action tree */
class SemanticFunction {
public:
    virtual ~SemanticFunction();
    /** Execute a semantic function */
    virtual void execute(GlobalSemanticContext &stack, const SemanticContext::Ptr &) const = 0;
    /** Execute a semantic function */
    virtual void cleanup(GlobalSemanticContext &stack) const = 0;
};


template <class T, class U>
struct SemanticContextInterfaceRule;
template <class T>
struct ContextRule;

/**
 * A rule
 */
class Rule {
    RuleImpl *operator->();
    const RuleImpl *operator->() const;
public:
    SmartPointer<RuleImpl> rule;
    Rule(const Rule &);
    explicit Rule(RuleImpl *);
    virtual ~Rule();


    /** \name Automatic construction of elements */
    //@{
    /** A literal reckoniser */
    Rule(const std::string &);
    /** A literal reckoniser */
    Rule(const char *);
    /** A character reckoniser */
    Rule(char);
    /** A token reckoniser */
    Rule(const Token &);

    enum Way { WAY_IN, WAY_OUT, WAY_ERROR };
    template <typename T>
    Rule(ParseResult  (*f)(Rule::Way, T &)) : rule(new ContextRule<T>(f)) {}
    
    Rule(GlobalSemanticContext::Function f);
    //@}

    Rule();


    /** \name EBNF approximated syntax*/
    //@{


    /** Sequence */
    Rule operator>>(const Rule&);

    /** Alternative */
    Rule operator|(const Rule&);

    /** Optional */
    Rule operator!();
    /** One or more */
    Rule operator+();
    /** Kleene operator */
    Rule operator*();

    /** But not */
    Rule operator-(const Rule&);
    /** Negation */
    Rule operator~();

    //@}

    /** Make this node a root node */
    Rule operator++(int);

    /** Assignement */
    virtual Rule &operator=(const Rule &);

    /** Semantic operator */
    template <class T, class U>
    Rule operator[](void (*f)(T &, U &)) {
        return Rule(new SemanticContextInterfaceRule<T,U>(*this,f));
    }
    
    /** Context operator */
    Rule operator[](GlobalSemanticContext::Function);

    /** Context operator */
    template <class T>
    Rule operator[](ParseResult(*f)(Way, T &)) {
        return Rule(new ContextRule<T>(*this,f));
    }

    ParseResult parse(ParserContext &) const;

    /**
     * Modifies the generation of the AST tree
     * @param  b true if we want to "flatten" the generated AST tree
     */
    void set_flatten(bool b);
};


/** Template to help the building of new contexts */
template <class T>
SemanticContext *cconstructor() {
    return new T();
}
/** Construct a new context */
Rule operator>>(SemanticContext::Constructor c, const Rule &r);




/**
 * A rule that contains... a rule
 */
struct RuleContainer : public RuleImpl {
    bool debug;
    std::string name;
public:
    SmartPointer<RuleImpl> rule;
    RuleContainer();
    RuleContainer(const SmartPointer<RuleImpl> &rule);
    ParseResult match(ParserContext &) const;

    void set_name(const std::string &s);
    virtual std::string get_name() const;
    void set_debug(bool);

};

/**
 * A rule that can be redefined (aka grammar rules)
 */
class GrammarRule : public Rule {
public:

    GrammarRule();
    Rule &operator=(const Rule &);
    Rule &operator=(const GrammarRule &);
    void set_name(const std::string &s);
    void set_debug(bool);
};

// Predefined parsers

extern Rule digit;

/** Match decimal digits */
extern Rule digit_p;

/** Match hexadecimal digits, ie 0..9 A..F a..f */
extern Rule xdigit_p;

/** Match alphanumeric characters */
extern Rule alpha_p;

/** Match newlines, tabulations and spaces */
extern Rule space_p;

/** Match any character */
extern Rule anychar_p;

/** A parser that is always true and do not consume input */
extern Rule true_p;

Rule operator>>(char, const Rule &);
Rule operator>>(const std::string &, const Rule &);

inline Rule operator>>(const Token &token, const Rule &r) {
    return Rule(token) >> r;
}


/** \name Directives */

class Directive {
public:
    Rule operator[](const Rule &);
};
extern Directive root_node_d;

struct no_node_d {
    Rule operator[](const Rule &);
};

class Flatten_d {
public:
    Rule operator[](const Rule &);
};
extern Flatten_d flatten_d;

struct Lexeme_d {
    Rule operator[](const Rule &);
};
extern Lexeme_d lexeme_d;

struct Phrase_d {
    Rule operator[](const Rule &);
};
extern Phrase_d phrase_d;

struct Longest_d {
    Rule operator[](const Rule &);
};
extern Longest_d longest_d;




// ---- Null parsers ----

struct Epsilon_p {
    Rule operator()(const Rule &);
};
extern Epsilon_p epsilon_p;



// ---- if / then / else ----

class RuleThenElse : public Rule {
public:
    RuleThenElse(const Rule &, const Rule &);
    Rule else_p(const Rule &);
};

class RuleThen {
    Rule condition;
public:
    RuleThen(const Rule &_condition);
    RuleThenElse operator[](const Rule &);
};

struct If_p {
    RuleThen operator()(const Rule &);
};
extern If_p if_p;


// ---- "is a" ----

class Is_a {
    Rule rule;
public:
    Is_a(const Rule &_rule);
    Rule operator()(const Rule &);
};


class Grammar : public RuleImpl {
public:
    Grammar();
    virtual const Rule &start() const = 0;
    inline ParseResult parse(ParserContext &s) const {
        return start().parse(s);
    };
};




/**
 * The scanner
 */
class Scanner {
protected:
    Rule skipper;
    /** Are we using the skipping grammar before each new char? */
    bool is_skipping;
public:
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
    virtual const Token::Information get_token();

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
    void set_skipper(const Rule &_rule);

    /** Skip */
    void skip(ParserContext &);
    //@}
};


/** The parser context */
class ParserContext {
public:

private:
    /** Our AST tree (generated on demand) */
    mutable SmartPointer<AST> m_ast;
    /** Do we generate an AST tree? */
    bool generate_AST;
    /** Do we generatic an action tree? */
    bool generate_action_tree;
    /** Scanner */
    Scanner *scanner;
public:
    uint level;
    uint operation_number;

    inline Scanner &get_scanner() {
        return *scanner;
    }

    inline bool is_generating_AST() const {
        return generate_AST;
    }

    inline bool do_generate_AST(bool b) {
        bool c = generate_AST;
        generate_AST = b;
        return c;
    }


    class ASTAction {
    public:
        class Context;
        virtual ~ASTAction() = 0;
        virtual void run(Context &) = 0;
        virtual void print(std::ostream &out) const = 0;
    };


    std::vector<ASTAction*> m_actions;

    SmartPointer<AST> get_AST();


    inline bool is_generating_action_tree() const {
        return generate_action_tree;
    }
    inline bool do_generate_action_tree(bool b) {
        bool c = generate_action_tree;
        generate_action_tree = b;
        return c;
    }

    struct ActionTree {
        struct Node {
            SmartPointer<Node> next, first_child;
            SmartPointer<SemanticContext> context;
            const SemanticFunction &rule;
            Node(const SemanticFunction &r);
            void execute(GlobalSemanticContext &context);
        };
        SmartPointer<Node> root, last;
    };

    ActionTree action_tree;
    SmartPointer<SemanticContext> execute_actions(GlobalSemanticContext &);

    /**
     * Add an action associated with rule r
     * @param r the sematantic function
     * @param c the current semantic context
     * @return a node
     */
    SmartPointer<ActionTree::Node> add_action(const SemanticFunction &r, const SmartPointer<SemanticContext> &c);

    /**
     * Add the action associated with rule r
     * The current tree becomes the child of rule r
     * The rule r is added to the action tree t
     * @param r 
     * @param t 
     */
    void add_action(const SemanticFunction &r, const SmartPointer<SemanticContext> &c, const ActionTree &t);

    ActionTree remove_action_tree();
    void restore_action_tree(const ActionTree &);

    ParserContext(Scanner &);
    ParserContext();
    virtual ~ParserContext();
};



/**
 * An AST scanner
 */
class ASTScanner : public Scanner {
    const AST &ast;
    struct Iterator;

    struct Position {
        enum State { start, end, normal };
        const AST* node;
        State state;
        long rank;
        inline const Token &get_token() const {
            switch(state) {
            case Position::normal:
                return node->token;
            case Position::start:
                return ASTScanner::tree_start;
            case Position::end:
                return ASTScanner::tree_end;
            }
            throw;
        }

        Position(const AST *p, State s, long r);
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
    const Token::Information get_token();
};

// Parsers for AST trees
struct ASTMatch {
    Rule operator()(const Token &token);
    Rule operator()(const Token &token, const Rule &);
    Rule operator()(const Rule &);
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


struct UnaryOperator : public RuleImpl {
    const Rule rule;
    UnaryOperator(const Rule &_rule);
};


template <class T, class U>
struct SemanticContextInterfaceRule : public UnaryOperator, public SemanticFunction {

    typedef void (*Function)(T &, U &);
    Function function;

    SemanticContextInterfaceRule(const Rule &_rule, Function f) : UnaryOperator(_rule), function(f) {}
    ParseResult match(ParserContext &s) const {
        ParseResult  r = rule.parse(s);
        if (r) {
            //          std::cerr << "Adding semantic interface action " << this << std::endl;
            s.add_action(*this, 0);
        }
        return r;
    }

    virtual void execute(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {
        if (stack.empty() || !stack.get_last())
            throw std::logic_error("Missing context");
        try {
            function(dynamic_cast<T&>(*stack.top()), dynamic_cast<U&>(*stack.get_last()));
        } catch (const std::bad_cast &) {
            if (dynamic_cast<T*>(stack.top().get()) == 0)
                std::cerr << "Bad cast: can't convert " << typeid(stack.top().get()).name() <<" to " << typeid(T*).name() << std::endl;
            if (dynamic_cast<U*>(stack.get_last().get()) == 0)
                std::cerr << "Bad cast: can't convert " << typeid(stack.get_last().get()).name() <<" to " << typeid(U*).name() << std::endl;
            throw;
        }
    }
virtual void cleanup(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {}
}
;

template <class T>
struct SemanticContextRule : public UnaryOperator, public SemanticFunction {
    typedef void (*Function)(T &);
    Function function;

    SemanticContextRule(const Rule &_rule, Function f) : UnaryOperator(_rule), function(f) {}
    ParseResult match(ParserContext &s) const {
        ParseResult r = rule.parse(s);
        return r;
    }

    virtual void execute(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {
        function(dynamic_cast<T&>(*stack.top()));
    }
    virtual void cleanup(SemanticContext::Stack &stack, const SmartPointer<SemanticContext>& c) const {}
}
;


/**
 * Add a semantic function in the processing stream
 * @param r 
 * @param (* f)( T & ) 
 * @return 
 */
template<class T>
Rule operator<<(const Rule &r, void (*f)(T &)) {
    return Rule(new SemanticContextRule<T>(r,f));
}




template <class T>
struct ContextRule : public RuleImpl {
    Rule rule;
    typedef ParseResult (*Function)(Rule::Way, T &);
    Function function;

    ContextRule(const Rule &_rule, Function f) : rule(_rule), function(f) {}
    ContextRule(Function f) : function(f) {}

    ParseResult match(ParserContext &s) const {
        try {
            if (!rule.rule)
                return function(Rule::WAY_IN, dynamic_cast<T&>(s));

            function(Rule::WAY_IN,dynamic_cast<T&>(s));
            ParseResult r = rule.parse(s);
            if (r)
                function(Rule::WAY_OUT,dynamic_cast<T&>(s));
            else
                function(Rule::WAY_ERROR,dynamic_cast<T&>(s));
            return r;
        } catch(...) {
            std::cerr << "Error (type of s is " << typeid(s).name() << ")" << std::endl;
        }
    }

};



}
#endif
