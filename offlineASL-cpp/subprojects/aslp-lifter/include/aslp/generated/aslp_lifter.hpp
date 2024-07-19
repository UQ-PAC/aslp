/* AUTO-GENERATED LIFTER FILE */

#pragma once
#include <cassert>
#include <tuple>
#include <variant>
#include <vector>
#include <stdexcept>
#include <aslp/interface.hpp>

namespace aslp {

template <typename Traits>
class aslp_lifter {
  public: using interface = lifter_interface<Traits>;
  private: interface& iface;
  public:
  aslp_lifter(interface& iface) : iface{iface} { }
  /* generated semantics */
  void f_A64_decoder(typename Traits::bits v_enc);
  /* generated decode test conditions */
};

} // namespace aslp