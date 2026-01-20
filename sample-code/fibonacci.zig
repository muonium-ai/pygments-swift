const std = @import("std");

fn fib(n: u32) u32 {
    return if (n < 2) n else fib(n - 1) + fib(n - 2);
}

pub fn main() void {
    const n: u32 = 10;
    std.debug.print("{}\n", .{fib(n)});
}
