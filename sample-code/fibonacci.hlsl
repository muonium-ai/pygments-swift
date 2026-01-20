// Fibonacci HLSL
#define ONE 1

float fib(float n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

float4 main(float4 pos : POSITION) : SV_Position {
  return pos;
}
