#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <regex>
using namespace Rcpp;

// [[Rcpp::export]]
StringVector regmatch(std::string m, std::vector<std::string>& x, std::string delim) {
  std::string s;
  std::vector<std::string> ss;
  std::vector<std::string> res;
  // for (size_t i = 0; i < x.size(); ++i) {
  for (const auto &v : x) {
    res.clear();
    std::regex re(m);
    std::smatch match;
    // auto start = std::sregex_iterator(x[i].begin(), x[i].end(), re);
    auto start = std::sregex_iterator(v.begin(), v.end(), re);
    auto end = std::sregex_iterator();
    while (start != end) {
      res.push_back(start->str());
      ++start;
    }
    s.clear();
    for (const auto &piece : res) {
      s += piece;
      if (piece != res.back()) s += delim;
      }
    ss.push_back(s);
    }
  return wrap(ss);
}
