// Solidity Fibonacci
pragma solidity ^0.8.20;

contract Fib {
    function fib(uint n) public pure returns (uint) {
        if (n < 2) return n;
        return fib(n - 1) + fib(n - 2);
    }
}
