#include <iostream>
#include <vector>
#include <map>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/bron_kerbosch_all_cliques.hpp>
#include <boost/graph/graphviz.hpp> //DEBUG

using namespace std;
using namespace boost;
using Counts = map<char, int>;
struct Node {
	Node() : letter(' ') {}
	Node(char c) : letter(c) {}
	char letter;
};
using Graph = adjacency_list<listS, vecS, undirectedS, Node>;
using Vertex = graph_traits<Graph>::vertex_iterator;

////// DEBUG
struct labelWriter {
	labelWriter(const Graph& g) : g(g) {}
	template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			out << "[label=\"" << get(&Node::letter, g, v) << "\"]";
		}
	private:
	const Graph& g;
};
void dispGraph(Graph& g) {
	write_graphviz(cout, g, labelWriter(g));
}
//////

void upCount(Counts& c, string& w) {
	Counts local;
	for(char c : w)
		++local[c];
	for(auto p : local)
		if(c[p.first] < p.second)
			c[p.first] = p.second;
}

bool buildWord(const string& w, Counts& c) {
	return all_of(c.begin(), c.end(), [&w](auto && p) {
			return count(w.begin(), w.end(), p.first) <= p.second;
			});
}

bool checkNeighbor(Counts& c, char a, char b, vector<string>& words) {
	if(a == b) return false;
	return all_of(words.begin(), words.end(), [&c, a, b](const string& w) {
			--c[a];
			bool r = buildWord(w, c);
			++c[a]; if(r) return true; --c[b];
			r = buildWord(w, c);
			++c[b]; return r;
			});
}

struct Agregator {
	Agregator(vector<vector<char>>& n, Counts& c)
		: cliques(n), counts(c) {}
	template <typename Clique, typename Graph>
		void clique(const Clique& c, const Graph& g) {
			typename Clique::const_iterator i, end = c.end();
			vector<char> tmp;
			for(i = c.begin(); i != end; ++i)
			    tmp.push_back(g[*i].letter);
			sort(tmp.begin(), tmp.end(), [this](const char& a, const char& b){
					return counts[a] > counts[b];});
			cliques.push_back(tmp);
		}
	vector<vector<char>>& cliques;
	Counts& counts;
};

bool isEmpty(Counts& c) {
	for(auto && p : c) if(p.second != 0) return false;
	return true;
}

int main(int, char**) {
	int n;
	cin >> n;
	vector<string> words(n);
	Counts counts;
	cout << "Reading words..." << endl;
	for(int i = 0 ; i < n ; ++i){
		cin >> words[i];
		upCount(counts, words[i]);
	}
	Graph g;
	cout << "Building graph..." << endl;
	for(auto p : counts)
		add_vertex(Node(p.first), g);
	Vertex v1, vend1, v2;
	for(tie(v1, vend1) = vertices(g); v1 != vend1; ++v1) {
		for(v2 = v1; v2 != vend1; ++v2)
			if(checkNeighbor(counts, g[*v1].letter, g[*v2].letter, words))
				add_edge(*v1, *v2, g);
	}
	cout << "Finding cliques..." << endl;
	vector<vector<char>> cliques;
	Agregator vis(cliques, counts);
	bron_kerbosch_all_cliques(g, vis, 1);
	sort(cliques.begin(), cliques.end(), [](vector<char>&a, vector<char>&b){
			return a.size() < b.size();});
	cout << "Building blocks..." << endl;
	int N = 1;
	while(!cliques.empty()) {
		int idx = 0;
		while(cliques[idx].size() < N and idx < cliques.size()) ++idx;
		if(idx == cliques.size()) --idx;
		vector<char>& C(cliques[idx]);
		for(int i = 0 ; i < C.size() and i < N ; ++i) {
			cout << C[i] << " ";
			--counts[C[i]];
		}
		cout << endl;
		//Purge and sort cliques
		for(idx = 0 ; idx < cliques.size() ; ++idx) {
			vector<char>& C = cliques[idx];
			C.erase(remove_if(C.begin(), C.end(), [&counts](char b) {
					return counts[b] <= 0;}), C.end());
			sort(C.begin(), C.end(), [&counts](const char& a, const char& b){
					return counts[a] > counts[b];});
		}
		cliques.erase(remove_if(cliques.begin(), cliques.end(), [](vector<char>& c) {
				return c.empty();}), cliques.end());
		sort(cliques.begin(), cliques.end(), [](vector<char>&a, vector<char>&b){
				return a.size() < b.size();});
		++N;
	}
}
