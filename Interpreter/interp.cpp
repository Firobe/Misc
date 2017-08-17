#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <iostream>
#include <stdexcept>
#include <map>

namespace qi = boost::spirit::qi;
namespace phx = boost::phoenix;
namespace asc = boost::spirit::ascii;
using namespace std;
#define eval(x) boost::apply_visitor(*this, x)

template <typename Tag> struct BinExpr;
struct Add; struct Sub; struct Mult; struct Div; struct Exp;
struct FuncCall;
using Variable = string;
using Constant = float;
using Expression = boost::variant<
	Constant,
	Variable,
	boost::recursive_wrapper<BinExpr<Add>>,
	boost::recursive_wrapper<BinExpr<Sub>>,
	boost::recursive_wrapper<BinExpr<Mult>>,
	boost::recursive_wrapper<BinExpr<Div>>,
	boost::recursive_wrapper<BinExpr<Exp>>,
	boost::recursive_wrapper<FuncCall>
	>;
using ParamList = vector<Expression>;
using VarList = vector<Variable>;

template <typename Tag>
struct BinExpr {
	BinExpr(Expression& a, Expression& b) : L(a), R(b) {}
	Expression L, R;
	static float f(float, float);
};
template<> float BinExpr<Add>::f(float a, float b) { return a + b; }
template<> float BinExpr<Sub>::f(float a, float b) { return a - b; }
template<> float BinExpr<Mult>::f(float a, float b) { return a * b; }
template<> float BinExpr<Div>::f(float a, float b) { return a / b; }
template<> float BinExpr<Exp>::f(float a, float b) { return pow(a, b); }

struct FuncCall {
	FuncCall(string& s, ParamList& p) : name(s), params(p) {}
	string name;
	ParamList params;
};
struct Function {
	Function() = default;
	Function(Expression& e) : f(e) {}
	Function(Expression& e, VarList& args) : args(args), f(e) {}
	VarList args;
	Expression f;
};
using HashT = map<string, Function>;

struct Evaluator : public boost::static_visitor<float> {
	HashT vars;
	Evaluator(HashT& t) : vars(t) {}
    float operator()(Constant& c)  { return c; }
	float operator()(Variable& s) { try {
			return eval(vars.at(s).f);
		} catch(...){ throw runtime_error("variable '" + s + "' inconnue"); }
	}
	template<typename Tag> float operator()(BinExpr<Tag>& e) {
		return BinExpr<Tag>::f(eval(e.L), eval(e.R));
	}
	float operator()(FuncCall& f) {
		ParamList backup(f.params.size());
		try {
			for(unsigned i = 0 ; i < f.params.size() ; ++i) {
				backup[i] = vars[vars.at("_f" + f.name).args[i]].f;
				vars[vars.at("_f" + f.name).args[i]].f = Constant(eval(f.params[i]));
			}
			float r = eval(vars.at("_f" + f.name).f);
			for(unsigned i = 0 ; i < f.params.size() ; ++i)
				vars.at(vars.at("_f" + f.name).args[i]).f = backup[i];
			return r;
		} catch(...) { throw runtime_error("fonction '" + f.name + "' inconnue"); }
	}
};

template <typename It = string::iterator, typename Skipper = qi::space_type>
struct ExprParser : qi::grammar<It, Expression(), Skipper> {
	ExprParser() : ExprParser::base_type(instr) {
		using namespace qi;
		instr = (identifier >> '(' >> varList >> ')' >> '=' >> expression) [_val = Constant(0), phx::bind(&ExprParser::nFunc, this, _1, _2, _3)]
			| (identifier >> '=' >> expression) [_val = _2, phx::bind(&ExprParser::nVar, this, _1, _2)] | (expression) [_val = _1];
		expression = sum.alias();
		sum = sub [_val = _1] >> *('+' >> sub [_val = phx::construct<BinExpr<Add>>(_val, _1)] );
		sub = mult [_val = _1] >> *('-' >> mult [_val = phx::construct<BinExpr<Sub>>(_val, _1)] );
		mult = div [_val = _1] >> *('*' >> div [_val = phx::construct<BinExpr<Mult>>(_val, _1)] );
		div = exp [_val = _1] >> *('/' >> exp [_val = phx::construct<BinExpr<Div>>(_val, _1)] );
		exp = paren [_val = _1] >> *('^' >> paren [_val = phx::construct<BinExpr<Exp>>(_val, _1)] );
		paren %= '(' >> expression >> ')' | funcCall;
		funcCall = (identifier >> '(' >> paramList >> ')') [_val = phx::construct<FuncCall>(_1, _2)] | var [_val = _1];
		var = identifier [_val = phx::construct<Variable>(_1)] | constante [_val = _1];
		constante = float_;
		identifier %= lexeme[asc::alpha >> *(asc::alnum)];
		paramList = expression [phx::push_back(phx::ref(_val), _1)] % ',' | eps;
		varList = identifier [phx::push_back(phx::ref(_val), _1)] % ',' | eps;
	}
	void nVar(string& s, Expression& e) { vars[s] = e; }
	void nFunc(string& n, VarList& params, Expression& v) { vars["_f" + n] = Function(v, params);}
	HashT vars;
	private:
	qi::rule<It, Expression(), Skipper> instr, expression, sum, sub, mult, div, exp,
		var, funcCall, paren, constante;
	qi::rule<It, string(), Skipper> identifier; 
	qi::rule<It, ParamList(), Skipper> paramList; 
	qi::rule<It, VarList(), Skipper> varList; 
};

int main(int, char**) {
	string line;
	ExprParser<decltype(line.begin())> p;
	while(line != "exit") {
		cout << "> ";
		getline(cin, line);
		Expression result;
		auto d = line.begin(), f = line.end();
		bool s = phrase_parse(d, f, p, qi::space, result);
		Evaluator e(p.vars);
		try {
			if(!s or d != f) cout << "Syntaxe invalide" << endl;
			else cout << boost::apply_visitor(e, result) << endl;
		} catch(exception& e) {
			cerr << "Erreur pendant l'évaluation : " << e.what() << endl;
		}
	}
	return 0;
}
