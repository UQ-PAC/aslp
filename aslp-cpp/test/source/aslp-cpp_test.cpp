#include <string>

#include "aslp-cpp/aslp-cpp.hpp"
#include <iostream> 

auto main() -> int
{
  auto s = aslp_client::start();

  try {
    std::string c;
    std::tie(std::ignore, c) = s->get_opcode(0xFD430091);
    std::cout << c << "\n";
  } catch (std::runtime_error &e) {
    std::cout << " error " << e.what() << "\n";
    return 1;
  } 


  return 0;
}
