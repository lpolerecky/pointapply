#include <cpp11.hpp>
#include <vector>
using namespace cpp11;

[[cpp11::register]]
cpp11::integers aggregate_vc(cpp11::integers grd, cpp11::integers mld, cpp11::writable::integers cst) {

  // std::vector<int> grd({1, 1, 2, 2, 3, 3, 4, 4});
  // std::vector<int> mld({11, 21, 33, 67, 98, 123, 555, 2});


  // output vector (or cast) based on unique grid cells
  // std::vector<int> cst =
  // cpp11::writable::integers cst = grd; // copy original
  // auto last = std::unique(cst.begin(), cst.end()); // unique elements
  // cst.erase(last, cst.end()); // erase duplicates


  // for each loop going along grid
  int ocounter{0};
  // for each loop along output vector
  for (auto grid : cst) {

    // for each loop going along grid
    int icounter{0};
    // for saving the intermediates
    int out{0};

    for (auto element : grd) {

      // storing element that made the predicate based on the grid
      if (element == grid) {
        // simple sum of the elements
        out += mld[icounter];

      }

      // counter for indexing the original vector (inner)
      ++icounter;
    }
  // get the sum of the extracted components
  cst[ocounter] = out;
  // counter for indexing the cast (outer)
  ++ocounter;

  }

  //
  cpp11::writable::integers result = cst;

  return result;
}



