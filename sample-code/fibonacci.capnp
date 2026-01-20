# Fibonacci Cap'n Proto
@0xdeadbeefdeadbeef;

using Cxx = import "c++.capnp";

struct Item {
  n @0 :Int32;
  label @1 :Text;
}
