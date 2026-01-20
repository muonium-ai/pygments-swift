#import <Foundation/Foundation.h>

// Objective-C Fibonacci
@interface Fib : NSObject
+ (int)fib:(int)n;
@end

@implementation Fib
+ (int)fib:(int)n {
  return n < 2 ? n : [Fib fib:n - 1] + [Fib fib:n - 2];
}
@end

int main(void) {
  @autoreleasepool {
    NSLog(@"%d", [Fib fib:10]);
  }
  return 0;
}
