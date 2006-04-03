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

/**
 * The class Rule is just a simple container (contains a shared pointer to RuleImpl)
 */

#include <sstream>
#include <cstdlib>
#include "parser.h"

namespace Soul {


// ------------------------------------ AST Actions ---------------------------

ParserContext::ASTAction::~ASTAction() {}

class ParserContext::ASTAction::Context {
public:
    std::stack<SmartPointer<AST> > stack;
    Context() {
        stack.push(0);
    }
};

class AST_Push : public ParserContext::ASTAction {
public:
    void run(ParserContext::ASTAction::Context &c) {
        c.stack.push(0);
    }
    virtual void print(std::ostream &out) const {
        out << "Push";
    }
};
class AST_Pop : public ParserContext::ASTAction {
    bool m_flatten;
public:
    AST_Pop(bool flatten) : m_flatten(flatten) {}
    void run(ParserContext::ASTAction::Context &c) {
        SmartPointer<AST> top = c.stack.top();
        c.stack.pop();
        if (top) {
            if (m_flatten && top->state == AST::ACTIVE) {
                top->state = AST::ROOT;
            }
            if (c.stack.top()) {
                if (top->is_active()) {
                    top->add_as_last_child(c.stack.top());
                    c.stack.top() = top;
                } else if (c.stack.top()->is_active())
                    c.stack.top()->add_as_last_child(top);
                else
                    c.stack.top()->add_as_last_sibling(top);
            } else
                c.stack.top() = top;
        }
    }
    virtual void print(std::ostream &out) const {
        out << "Pop" << (m_flatten ? " (flattening)" : " (no flattening)");
    }
};

class AST_Add : public ParserContext::ASTAction {
public:
    SmartPointer<AST> m_astNode;
    AST_Add(const SmartPointer<AST> &x) : m_astNode(x) {}
    void run(ParserContext::ASTAction::Context &c) {
        if (m_astNode) {
            if (c.stack.top()) {
                if (m_astNode->is_active()) {
                    m_astNode->add_as_last_child(c.stack.top());
                    c.stack.top() = m_astNode;
                } else {
                    if (c.stack.top()->is_active())
                        c.stack.top()->add_as_last_child(m_astNode);
                    else
                        c.stack.top()->add_as_last_sibling(m_astNode);
                }
            } else
                c.stack.top() = m_astNode;
        }
    }
    virtual void print(std::ostream &out) const {
        out << "Add node: ";
        m_astNode->print(out,false);
    }

};




// ------------------------------------ ParseResult ---------------------------

ParseResult &ParseResult::update(const ParseResult &other) {
    hit &= other.hit;
    return *this;
}

// ------------------------------------ Operators -----------------------------

UnaryOperator::UnaryOperator(const Rule &_rule) : rule(_rule) {}


/** One or more */
class Positive : public UnaryOperator {
public:
    Positive(const Rule &_rule) : UnaryOperator(_rule) {}
    ParseResult match(ParserContext &s) const {
        ParseResult r = rule.parse(s);
        if (!r)
            return ParseResult(false);
        Scanner::Iterator last = s.get_scanner().get_pos();
        while (ParseResult _r = rule.parse(s)) {
            last = s.get_scanner().get_pos();
            r.update(_r);
        }
        s.get_scanner().move(last);
        return r;
    }
};

class KleeneStar : public UnaryOperator {
public:
    KleeneStar(const Rule &_rule) : UnaryOperator(_rule) {}
    ParseResult match(ParserContext &s) const {
        Scanner::Iterator last = s.get_scanner().get_pos();
        ParseResult r(true);
        while (ParseResult _r = rule.parse(s)) {
            last = s.get_scanner().get_pos();
            r.update(_r);
        }
        s.get_scanner().move(last);
        return r;
    }
};

class Optional : public UnaryOperator {
public:
    Optional(const Rule &_rule) : UnaryOperator(_rule) {}
    ParseResult match(ParserContext &s) const {
        Scanner::Iterator last = s.get_scanner().get_pos();
        if (ParseResult r = rule.parse(s))
            return r;
        s.get_scanner().move(last);
        return ParseResult(true);
    }
};

class Not : public UnaryOperator {
public:
    Not(const Rule &_rule) : UnaryOperator(_rule) {}
    ParseResult match(ParserContext &s) const {
        ParseResult r = rule.parse(s);
        r.hit = !r.hit;
        return r;
    }
};



struct BinaryOperator : public RuleImpl {
    const Rule left;
    const Rule right;
    BinaryOperator(const Rule &l, const Rule &r) : left(l), right(r) {}
}
;


struct Alternative : public BinaryOperator {
    /** Return the longest alternative?*/
    enum { normal, longest, shortest } mode;
    Alternative(const Rule &l, const Rule &r) : BinaryOperator(l,r), mode(normal) {}

    ParseResult match(ParserContext &s) const {
        switch(mode) {
        case normal: {
                Scanner::Iterator start = s.get_scanner().get_pos();
                if (ParseResult r = left.parse(s))
                    return r;
                s.get_scanner().move(start); // rewind
                return right.parse(s);
            }

        case longest:
        case shortest: {
                size_t N = s.m_actions.size();
                Scanner::Iterator start = s.get_scanner().get_pos();
                ParseResult a;
                if (!(a = left.parse(s))) {
                    s.get_scanner().move(start);
                    return right.parse(s);
                }
                Scanner::Iterator left_pos = s.get_scanner().get_pos();
                size_t left_N = s.m_actions.size();

                s.get_scanner().move(start);
                ParseResult b;
                if (!(b = right.parse(s))) {
                    s.get_scanner().move(left_pos);
                    return a;
                }
//                 size_t right_N = s.m_actions.size();

                // both are hits
                ParseResult *c = &b;
                long d = *left_pos - *s.get_scanner().get_pos();
                if ((mode == longest && d > 0) || (mode == shortest && d < 0))  {
                    // We chose the left alternative
                    s.get_scanner().move(left_pos);
                    c = &a;
                    s.m_actions.erase(s.m_actions.begin() + left_N, s.m_actions.end());
                } else {
                    // Right alternative
                    s.m_actions.erase(s.m_actions.begin() + N, s.m_actions.begin() + left_N - 1);
                }
                return *c;
                break;
            }
        default:
            assert(false); // FIXME
        }
        return ParseResult(false);
    }
};

struct Sequence : public BinaryOperator {
    Sequence(const Rule &l, const Rule &r) : BinaryOperator(l,r) {}
    ParseResult match(ParserContext &s) const {
        ParseResult r1 = left.parse(s);
        if (!r1)
            return false;
        ParseResult r2 = right.parse(s);
        if (r2)
            return r1.update(r2);

        return false;
    }
};

struct ButNode : public BinaryOperator {
    ButNode(const Rule &l, const Rule &r) : BinaryOperator(l,r) {}
    ParseResult match(ParserContext &s) const {
        Scanner::Iterator start = s.get_scanner().get_pos();
        ParseResult r = left.parse(s);
        if (!r)
            return ParseResult(false);
        Scanner::Iterator save = s.get_scanner().get_pos();
        s.get_scanner().move(start);
        bool b = right.parse(s);
        s.get_scanner().move(save);
        r.hit = r.hit && !b;
        return r;
    }
};


struct Literal : public RuleImpl {
    std::string value;
    Literal(const std::string &s) : value(s) {/* std::cerr << "[CREATING LITERAL " << s << "]\n";*/
    }
    Literal(char c) : value(1,c) { /*std::cerr << "[CREATING LITERAL " << c << "]\n";*/
    }

    ParseResult match(ParserContext &s) const {
        if (s.get_scanner().at_end())
            return ParseResult(false);
        //       std::cerr << std::string(context.level+1,' ') <<  " [?: MATCH " << value << "] : ";
        std::string::const_iterator i;
        for(i = value.begin(); !s.get_scanner().at_end() && i != value.end(); i++) {
            char c = s.get_scanner().get_char();
            if (*i != c) {
                //             std::cerr << *i << " != " << c << " => NO MATCH\n";
                return ParseResult(false);
            }
            //          else { std::cerr << "[" << *i << "=" << c << "]"; }
        }
        //       std::cerr << "MATCH" << std::endl;
        return ParseResult(i == value.end());
    }
};



// ---*--- Token parser ---*---

Token::Information::Information(const Token &_token, const std::string &_content) : token(_token), text(_content) {}

struct Token_p : public RuleImpl, public SemanticFunction {
    const Token &token;
    Token_p(const Token &_token) : token(_token) {}

    ParseResult match(ParserContext &s) const {
        Token::Information info = s.get_scanner().get_token();
        bool r = (info.token == token);
        //         std::cerr << "COMPARING " <<  info.token.get_name() << " (" << &info.token << ") AND " << token.get_name() << " (" << &token << ") => " << r << std::endl;
        if (r)
            s.add_action(*this, SmartPointer<SemanticContext>(new Token::Information(token, info.text)));
        return ParseResult(r);
    }

    virtual void execute(GlobalSemanticContext &context, const SmartPointer<SemanticContext> &c) const {
        context.getStack().push(c);
    }

    virtual void cleanup(GlobalSemanticContext &context) const {
        context.getStack().pop(context);
    }

};

// ------------------------------------ Predefined parsers ---

struct Digit : public RuleImpl {
    ParseResult match(ParserContext &s) const {
        return ParseResult(!s.get_scanner().at_end() && isdigit(s.get_scanner().get_char()));
    }
};

struct XDigit : public RuleImpl {
    ParseResult match(ParserContext &s) const {
        return ParseResult(!s.get_scanner().at_end() && isxdigit(s.get_scanner().get_char()));
    }
};

struct Alpha : public RuleImpl {
    ParseResult match(ParserContext &s) const {
        return ParseResult(!s.get_scanner().at_end() && isalpha(s.get_scanner().get_char()));
    }
};

struct Space : public RuleImpl {
    ParseResult match(ParserContext &s) const {
        return ParseResult(!s.get_scanner().at_end() && isspace(s.get_scanner().get_char()));
    }
};

struct Anychar : public RuleImpl {
    ParseResult match(ParserContext &s) const {
        if (s.get_scanner().at_end())
            return false;
        s.get_scanner().get_char();
        return true;
    }
};

struct True_p : public RuleImpl {
    ParseResult match(ParserContext &s) const {
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
    ParseResult match(ParserContext &s) const {
        Scanner::Iterator start = s.get_scanner().get_pos();
        bool b = rule.parse(s);
        s.get_scanner().move(start);
        return b;
    }
};

Rule Epsilon_p::operator()(const Rule &x) {
    return Rule(new _Epsilon_p(x));
}

Epsilon_p epsilon_p;


// -----*----- Not_p

struct _Not_p : public UnaryOperator {
    public:
        _Not_p(const Rule &_rule) : UnaryOperator(_rule) {}
        ParseResult match(ParserContext &s) const {
            Scanner::Iterator start = s.get_scanner().get_pos();
            ParseResult r = rule.parse(s);
            s.get_scanner().move(start);
            return !r;
        }
};

Rule Not_p::operator()(const Rule &x) {
    return Rule(new _Not_p(x));
}


Not_p not_p;

// ------------------------------------ If then else parser


struct _IfThenElse : public RuleImpl {
    const Rule m_condition, m_then, m_else;
    _IfThenElse(const Rule &_condition, const Rule &_then)
            : m_condition(_condition), m_then(_then) {}
    _IfThenElse(const Rule &_condition, const Rule &_then, const Rule &_else)
            : m_condition(_condition), m_then(_then), m_else(_else) {}

    ParseResult match(ParserContext &c) const {
        Scanner &s = c.get_scanner();
        Scanner::Iterator start = s.get_pos();
        bool b = m_condition.parse(c);
        bool old_ast_flag = c.do_generate_AST(false);
        s.move(start);
        c.do_generate_AST(old_ast_flag);
        if (b)
            return m_then.parse(c);
        else if (m_else.rule)
            return m_else.parse(c);
        return true;
    }
};

RuleThenElse::RuleThenElse(const Rule &_condition, const Rule &_then) : Rule(new _IfThenElse(_condition, _then)) {}
Rule RuleThenElse::else_p(const Rule &y) {
    SmartPointer<_IfThenElse> ite = rule; // throws an exception if not castable
    return Rule(new _IfThenElse(ite->m_condition, ite->m_then, ite->m_else));
}

RuleThen::RuleThen(const Rule &_condition) : condition(_condition) {}
RuleThenElse RuleThen::operator[](const Rule &x) {
    return RuleThenElse(condition, x);
}

RuleThen If_p::operator()(const Rule &x) {
    return RuleThen(x);
}
If_p if_p;


// ------------------------------------ Context rule --------------------------

struct _ContextRule : public RuleImpl, public SemanticFunction {
    
    Rule rule;
    GlobalSemanticContext::Function function;
    
    _ContextRule(GlobalSemanticContext::Function f) : function(f) {}
    _ContextRule(const Rule &_rule, GlobalSemanticContext::Function f) : rule(_rule), function(f) {}
            
    ParseResult match(ParserContext &s) const {
        if (rule.rule) {
            ParseResult r = rule.parse(s);
            if (!r) return r;
            s.add_action(*this,0);
            return r;
        }
        s.add_action(*this, 0);
        return true;
    }
    
    virtual void execute(GlobalSemanticContext &gc, const SmartPointer<SemanticContext>& c) const {
        function(gc);
    }
    virtual void cleanup(GlobalSemanticContext &gc) const {}

};

// ------------------------------------ Rule ---

Rule Rule::operator-(const Rule &x) {
    return Rule(new ButNode(*this,x));
}

Rule Rule::operator!() {
    return Rule((RuleImpl*)new Optional(*this));
}

Rule Rule::operator|(const Rule &x) {
    return Rule(new Alternative(*this, x));
}
Rule Rule::operator+() {
    return Rule(new Positive(*this));
}
Rule Rule::operator>>(const Rule &x) {
    return Rule(new Sequence(*this,x));
}
Rule Rule::operator*() {
    return Rule(new KleeneStar(*this));
}
Rule Rule::operator~() {
    return Rule(new Not(*this));
}


Rule::~Rule() { }
Rule::Rule() {}
Rule::Rule(const Rule &_other) : rule(_other.rule) {}
Rule::Rule(RuleImpl *_rule) : rule(_rule) {}

Rule::Rule(const Token &_token) : rule(new Token_p(_token)) {}
Rule::Rule(const char *s) : rule(new Literal(s)) {}
Rule::Rule(const std::string &s) : rule(new Literal(s)) {}
Rule::Rule(char c) : rule(new Literal(c)) {}
Rule::Rule(GlobalSemanticContext::Function f) : rule(new _ContextRule(f)) {}

Rule Rule::operator[](GlobalSemanticContext::Function f) {
    return Rule(new _ContextRule(*this, f));
}

RuleImpl *Rule::operator->() {
    return rule.get();
}
const RuleImpl *Rule::operator->() const {
    return rule.get();
}

Rule operator>>(char c, const Rule &x) {
    return Rule(new Sequence(Rule(new Literal(c)),x));
}

Rule operator>>(const std::string &s, const Rule &x) {
    return Rule(new Sequence(Rule(new Literal(s)),x));
}

Token dummy_token("dummy");
ParseResult Rule::parse(ParserContext &s) const {
    if (rule) {
        // Skip if necessary
        bool generate = s.do_generate_AST(false);
        s.get_scanner().skip(s);
        s.do_generate_AST(generate);

        size_t N = s.m_actions.size();
        if (generate && rule->get_flatten())
            s.m_actions.push_back(new AST_Push);
        ParseResult r = rule->parse(s);
        if (r) {
            if (generate && rule->get_flatten())
                s.m_actions.push_back(new AST_Pop(true));
        } else {
            assert(s.m_actions.size() >= N);
            s.m_actions.erase(s.m_actions.begin() + N, s.m_actions.end());
        }

        return r;
    }
    return false;
}
void Rule::set_flatten(bool b) {
    if (rule)
        rule->set_flatten(b);
}

Rule &Rule::operator=(const Rule &other) {
    rule = other.rule;
    return *this;
}


// -*- Semantic action




// -- Parser context
SmartPointer<AST> ParserContext::get_AST() {
    if (!m_ast) {

        ASTAction::Context context;
        for(size_t i = 0; i < m_actions.size(); i++) {

            m_actions[i]->run(context);

        }
        assert(context.stack.size() == 1);
        m_ast = context.stack.top();
    }
    return m_ast;
}

ParserContext::ActionTree::Node::Node(const SemanticFunction &r) : rule(r) {}
ParserContext::~ParserContext() {
    while (!m_actions.empty()) {
        delete m_actions.back();
        m_actions.pop_back();
    }
}

SmartPointer<ParserContext::ActionTree::Node> ParserContext::add_action(const SemanticFunction &r, const SmartPointer<SemanticContext> &c) {
    if (!is_generating_action_tree())
        return 0;

    SmartPointer<ParserContext::ActionTree::Node> node(new ActionTree::Node(r));
    node->context = c;

    if (action_tree.last) {
        assert(!action_tree.last->next);
        action_tree.last->next = node;
        action_tree.last = action_tree.last->next;
    } else {
        assert(!action_tree.root);
        action_tree.last = node;
        action_tree.root = action_tree.last;
    }
    return node;
}

void ParserContext::add_action(const SemanticFunction &r, const SmartPointer<SemanticContext> &c, const ActionTree &t) {
    if (!is_generating_action_tree())
        return;
    ActionTree v = remove_action_tree();
    action_tree = t;
    SmartPointer<ParserContext::ActionTree::Node>  node = add_action(r,c);
    node->context = c;
    node->first_child = v.root;
}


ParserContext::ActionTree ParserContext::remove_action_tree() {
    if (!is_generating_action_tree())
        return ParserContext::ActionTree();
    ActionTree t = action_tree;
    action_tree  = ActionTree();
    return t;
}

void ParserContext::restore_action_tree(const ActionTree &t) {
    if (!is_generating_action_tree())
        return;
    action_tree = t;
}

SmartPointer<SemanticContext> ParserContext::execute_actions(GlobalSemanticContext &context) {
    if (action_tree.root) {
        action_tree.root->execute(context);
        return context.getLast();
    }
    return 0;
}

void ParserContext::ActionTree::Node::execute(GlobalSemanticContext &gc) {
//        std::cerr << std::string(/*tttt++*/3,' ')  << "Executing action " << &rule << ", " << DEMANGLE(rule) << std::endl;
    rule.execute(gc, context);
    if (first_child)
        first_child->execute(gc);
    rule.cleanup(gc);
    if (next)
        next->execute(gc);
}


// ------------------------------------ Global semantic context ---------------

GlobalSemanticContext::~GlobalSemanticContext() {}



// ------------------------------------ Semantic context ----------------------

SemanticContext::~SemanticContext() {}
void SemanticContext::finish(GlobalSemanticContext &context) {}

SmartPointer<SemanticContext> SemanticContext::Stack::get_last() {
    return last;
}


SmartPointer<SemanticContext> SemanticContext::Stack::pop(GlobalSemanticContext &context) {
    last =  top();
    last->finish(context);
    SemanticContextStack::pop();
    return last;
}

struct SemanticContextConstructionRule : public UnaryOperator, public SemanticFunction {
    SemanticContext::Constructor constructor;
    SemanticContextConstructionRule(const Rule &_rule, SemanticContext::Constructor c) : UnaryOperator(_rule), constructor(c) {}
    ParseResult match(ParserContext &s) const {
        ParserContext::ActionTree t = s.remove_action_tree();
        ParseResult r = rule.parse(s);
        if (r)
            s.add_action(*this, 0, t);
        else
            s.restore_action_tree(t);

        return r;
    }
    virtual void execute(GlobalSemanticContext &context, const SemanticContext::Ptr &) const {
        context.getStack().push(SmartPointer<SemanticContext>(constructor()));
    }
    virtual void cleanup(GlobalSemanticContext &context) const {
        context.getStack().pop(context);
    }
};
Rule operator>>(SemanticContext::Constructor c, const Rule &r) {
    return Rule(new SemanticContextConstructionRule(r,c));
}








// ------------------------------------ GrammarRule ---

AST::AST(const Token &_token) : token(_token), parent(0), state(NORMAL) {}

namespace {
void tabulate(ParserContext &c) {
    std::cerr << std::string(c.level,' ');
};
};


std::string RuleContainer::get_name() const {
    return name;
}
RuleContainer::RuleContainer() : debug(false) {
    set_flatten(true);
}
RuleContainer::RuleContainer(const SmartPointer<RuleImpl> &_rule) :
debug(false), rule(_rule) {
    set_flatten(true);
}

void RuleContainer::set_debug(bool b) {
    debug = b;
}
void RuleContainer::set_name(const std::string &s) {
    name = s;
}


ParseResult RuleContainer::match(ParserContext &s) const {
    Soul::ParserContext &c = s;
    uint on = c.operation_number + 1;
    ParseResult r(false);
    //     if (rule && rule->get_flatten()) saved_ast = c.remove_ast();

    if (debug) {
        c.level++;
        c.operation_number++;
        tabulate(c);
        std::cerr << "\\ [" << c.operation_number << "] ";
        std::cerr << (rule ? "" : "[Empty] ") << name << " ";
        s.get_scanner().print_context(std::cerr, s.get_scanner().get_pos(), 10);
        std::cerr << std::endl;
    }

    Scanner::Iterator begin = s.get_scanner().get_pos();

    if (rule)
        try {
            r = rule->parse(s);
        } catch(std::exception &e) {
            std::cerr << "Caught exception in rule " << name << std::endl;
            throw;
        }

    if (debug) {
        Scanner::Iterator end = s.get_scanner().get_pos();
        tabulate(c);
        std::cerr <<  (r ? '/' : '#') << " [" << on << "] " << name << " ";
        s.get_scanner().print_context(std::cerr,begin,end);
        std::cerr << std::endl;
        c.level--;
    }

    return r;
}



GrammarRule::GrammarRule() : Rule(new RuleContainer) {  }

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


std::string RuleImpl::get_name() const {
    return "[anonymous]";
}
RuleImpl::RuleImpl() : do_flatten(false) {}
RuleImpl::~RuleImpl() {}
void RuleImpl::set_flatten(bool b) {
    do_flatten = b;
}

ParseResult RuleImpl::match(ParserContext &s) const {
    return false;
}


/** Parse */
ParseResult RuleImpl::parse(ParserContext &s) const {
    //         ParserContext &c = s;
    //            std::cerr << std::string(c.level++,' ') << " \\ " << DEMANGLE(*this) << " ";
    //            s.print_context(std::cerr,s.get_scanner().get_pos());
    //            std::cerr << std::endl;

    ParseResult r = match(s);
    //         c.level--;
    //         std::cerr << std::string(c.level,' ') << " " << (r ? "/ " : "# " ) << DEMANGLE(*this) << std::endl;
    return r;
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
    is_skipping = true;
}

void Scanner::skip(ParserContext &c) {
    if (do_skip()) {
        bool saved = set_do_skip(false);
        Scanner::Iterator last = get_pos();
        while (skipper.parse(c))
            last = get_pos();
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

/**
 *  The scanner iterator (implementation)
 */
struct ASTScanner::Iterator : public ScannerIteratorImpl {
    ASTScanner::Position position;
    Iterator(const Position &p) : position(p) {}
    long operator-(const ScannerIteratorImpl &other) {
        return position.rank - dynamic_cast<const Iterator &>(other).position.rank;
    }

};

const Token ASTScanner::tree_start("<tree>"), ASTScanner::tree_end("</tree>");

ASTScanner::Position::Position(const AST *p, State s, long r) :
node(p), state(s), rank(r) {}

ASTScanner::ASTScanner(const AST &_ast) : ast(_ast), position(&ast, Position::start, 0) {}

Scanner::Iterator ASTScanner::get_pos() const {
    return Scanner::Iterator(new Iterator(position));
}

bool ASTScanner::at_end(const Position &p) const {
    return p.node == 0;
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
        q.state = Position::normal;
        break;

    case Position::end:
        if (!q.node) {
            q.state = Position::normal;
            break;
        }
        if (q.node->next) {
            q.node = q.node->next.get();
            q.state = q.node->is_root() ? Position::start : Position::normal;
        } else {
            q.node = q.node->parent;
            q.state = Position::end;
            assert(!q.node || q.node->is_root());
        }
        break;

    case Position::normal:
        assert(q.node != 0);
        if (q.node->first_child) {
            q.node = q.node->first_child.get();
            if (q.node->is_root()) {
                q.state = Position::start;
            }
        } else if (q.node->next) {
            q.node = q.node->next.get();
            if (q.node->is_root()) {
                q.state = Position::start;
            }
        } else {
            // Nothing is valid
            q.state = Position::end;
            if (!q.node->is_root()) {
                assert(q.node || q.node->parent->is_root());
                q.node = q.node->parent;
            }

        }
        break;
    }
    return q;
}

void ASTScanner::print_context(std::ostream &out, Scanner::Iterator begin, size_t size) const {
    Position i = dynamic_cast<const Iterator&>(*begin.get()).position;
    size_t n = 0;
    for(; !at_end(i) && (size == 0 || n < size); i = get_next(i), n++)
        out  << i.get_token().get_name() << (i.node && i.node->is_root() && i.state == Position::normal ? "^" : "")
        //                     << ", " << i.node << ", " << i.state << ", " << (i.node ?  i.node->parent : 0) << "\n"
        << " ";
    if (i.node != 0)
        out << "...";
}

void ASTScanner::print_context(std::ostream &out, Scanner::Iterator begin, Scanner::Iterator _end) const {
    const Position & p = dynamic_cast<const Iterator&>(*_end.get()).position;
    for(Position i = dynamic_cast<const Iterator&>(*begin.get()).position; p.rank >
            i.rank ;
            i = get_next(i))
        out  << i.get_token().get_name() << " ";
}

const Token::Information ASTScanner::get_token() {
    if (at_end())
        throw std::runtime_error("End of stream");
    const Token::Information information(position.get_token(), position.node ? position.node->content : "");
    position = get_next(position);
    return information;
}

// ------------------------------------ Flatten -------------------------------
// Flatten a tree (the root node is not anymore active)

Rule Flatten_d::operator[](const Rule &x) {
    return Rule(new RuleContainer(x.rule));
}
Flatten_d flatten_d;

struct _no_node_d : public UnaryOperator {
    _no_node_d(const Rule &_rule) : UnaryOperator(_rule) {}
    ParseResult match(ParserContext &s) const {
        bool b = s.do_generate_AST(false);
        ParseResult r = rule.parse(s);
        s.do_generate_AST(b);
        return r;
    }
};

Rule no_node_d::operator[](const Rule &r) {
    return Rule(new _no_node_d(r));
}


// ------------------------------------ Directive ---

Rule Directive::operator[](const Rule &x) {
    return x;
}
Directive root_node_d;

struct _Lexeme_d : public UnaryOperator {
    _Lexeme_d(const Rule &_rule) : UnaryOperator(_rule) {}
    ParseResult match(ParserContext &s) const {
        bool old_skip = s.get_scanner().set_do_skip(false);
        bool b = rule.parse(s);
        s.get_scanner().set_do_skip(old_skip);
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
    ParseResult match(ParserContext &s) const {
        bool old_skip = s.get_scanner().set_do_skip(true);
        bool b = rule.parse(s);
        s.get_scanner().set_do_skip(old_skip);
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

void AST::set_parent(AST *p) const {
    parent = p;
    if (next)
        next->set_parent(p);
}

ParserContext::ParserContext() : generate_AST(false), generate_action_tree(true), level(0), operation_number(0)  {}
ParserContext::ParserContext(Scanner &s) : generate_AST(false), generate_action_tree(true), scanner(&s), level(0), operation_number(0)  {}

void AST::print(std::ostream &out, bool dot_format) {
    if (dot_format)
        out << "digraph G {" << std::endl << "graph[ordering=out,ratio=compress,rankdir=TB]; node [fontsize=10]; " << std::endl;

    print(out,dot_format,0);
    if (dot_format)
        out << "}" << std::endl;
}

void AST::print(std::ostream &out, bool dot_format, uint level) {
    if (dot_format) {
        out << "N" << this << "[label=\"" <<  token.get_name()  << "\\n" << content << "\"];";
        if (parent)
            out << "N" << parent << " -> " << "N" << this << ";" << std::endl;
    } else
        out << std::string(level, ' ') << "[" << token.get_name() /*<< "," << this*/ << (is_active() ? "*" : "") <<  "] " << content <<  std::endl;

    if (first_child)
        first_child->print(out, dot_format, level+1);
    if (next)
        next->print(out, dot_format, level);
}



// ------------------------------------ Is a ----------------------------------

struct Is_a_p : public UnaryOperator {
    Rule is_a_rule;
    Is_a_p(const Rule &_mrule, const Rule &_rule) : UnaryOperator(_rule), is_a_rule(_mrule) {}
    ParseResult match(ParserContext &s) const {
        // Try to match the "is a"
        Scanner::Iterator start = s.get_scanner().get_pos();
        if (!rule.parse(s))
            return false;
        Scanner::Iterator after = s.get_scanner().get_pos();

        s.get_scanner().move(start);
        if (!is_a_rule.parse(s)) {
            s.get_scanner().move(after);
            return false;
        }
        if ((*s.get_scanner().get_pos() - *after) != 0) {
            s.get_scanner().move(after);
            return false;
        }

        return true;
    }
};

Is_a::Is_a(const Rule &_rule) : rule(_rule) {}
Rule Is_a::operator()(const Rule &x) {
    return Rule(new Is_a_p(rule,x));
}




// ------------------------------------ MakeRoot ------------------------------

namespace {
// Add a node to the AST tree
// void construct_tree(const std::string &content, const SmartPointer<AST> &saved_tree, ParserContext &c, const Token &r,
//                     bool root_node) {
//     // Build ourselves
//     SmartPointer<AST> selftree(new AST(r));
//     selftree->content = content;
//
//     if (root_node) {
//         selftree->add_as_last_child(saved_tree);
//         c.ast = selftree;
//         c.ast->state = AST::ACTIVE;
//     } else {
//         if (saved_tree.isValid()) {
//             selftree->next = c.ast;
//             if (saved_tree->is_active())
//                 saved_tree->add_as_last_child(selftree);
//             else
//                 saved_tree->add_as_last_sibling(selftree);
//             c.ast = saved_tree;
//         } else {
//             c.ast = selftree;
//             c.ast->state = AST::NORMAL;
//         }
//     }
// }
// }


class AST_MakeRoot : public ParserContext::ASTAction {
public:
    void run(ParserContext::ASTAction::Context &c) {
        // The current generated AST tree must be a token
        if (c.stack.empty())
            throw std::logic_error("The AST stack is empty (make root)");
        AST *node = c.stack.top().get();
        if (!node || node->next || node->first_child) {
            throw std::logic_error("Trying to make a non-token the root of an AST tree");
        }
        node->state = AST::ACTIVE;
    }
    virtual void print(std::ostream &out) const {
        out << "Make root";
    }
};

class MakeRoot: public UnaryOperator {
public:
    MakeRoot(const Rule &_rule) : UnaryOperator(_rule) {}

    ParseResult match(ParserContext &s) const {
        if (!s.is_generating_AST())
            return rule.parse(s);
        s.m_actions.push_back(new AST_Push);
        ParseResult r = rule.parse(s);
        s.m_actions.push_back(new AST_MakeRoot);
        s.m_actions.push_back(new AST_Pop(false));
        return r;
    }
};

Rule Rule::operator++(int) {
    return Rule(new MakeRoot(*this));
}


struct Token_c : public UnaryOperator {
    const Token &token;
    Rule is_a_rule;
    bool root_node;

    Token_c(const Token &_token, const Rule &_rule, bool _root_node) : UnaryOperator(_rule), token(_token), root_node(_root_node) {}
    ParseResult match(ParserContext &s) const {
        if (!s.is_generating_AST()) {
            return rule.parse(s);
        }

        bool b = s.do_generate_AST(false);
        Scanner::Iterator begin = s.get_scanner().get_pos();
        ParseResult r = rule.parse(s);
        Scanner::Iterator end = s.get_scanner().get_pos();
        s.do_generate_AST(b);

        if (r) {
            std::ostringstream out;
            s.get_scanner().print_context(out,begin,end);
            SmartPointer<AST> node(new AST(token));
            node->content = out.str();
            if (root_node)
                node->state = AST::ACTIVE;
            s.m_actions.push_back(new AST_Add(node));
        }

        return r;
    }
};

Token::Token(const std::string &_name) : name(_name) {
    //     std::cerr << "Creating token " << _name << std::endl;
}
Token::~Token() {}
Rule Token::operator()() const {
    return Rule(new Token_c(*this, true_p, false));
}

std::string Token::get_name() const {
    return name;
}

Rule Token::operator[](GlobalSemanticContext::Function f) const {
    return Rule(new _ContextRule(Rule(*this), f));
}

Rule Token::operator()(const Rule &x) const {
    return Rule(new Token_c(*this, x, false));
}
Rule Token::operator^(const Rule &x) const {
    return Rule(new Token_c(*this, x, true));
}

}
}
