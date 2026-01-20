#import <Foundation/Foundation.h>
#include <vector>

// Objective-C++ sample
namespace demo {
static int fib(int n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }
}

int main(void) {
  std::vector<int> v = {demo::fib(5), demo::fib(6)};
  @autoreleasepool {
    NSLog(@"%lu", (unsigned long)v.size());
  }
  return 0;
}
