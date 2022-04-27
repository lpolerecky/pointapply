#include <cpp11.hpp>
#include <unordered_set>
#include <vector>

[[cpp11::register]]
cpp11::doubles mytapply_(cpp11::doubles  x, cpp11::doubles  y) {

  // tracker
  std::unordered_set<int> track;
  // out
  cpp11::writable::doubles out;

  for (int i = 0; i < x.size(); ++i) {
    // if values of index is new then save otherwise add to last value
    bool seen = track.insert(x[i]).second;
    if (seen) {
      out.push_back(y[i]);
    } else {
      // iterator to find location based on x
      int it = x[i] - 1;
      out[it] += y[i];
    }

  }
  return out;
}
