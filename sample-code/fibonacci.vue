<template>
  <!-- Fibonacci (Vue SFC) -->
  <ul class="fib">
    <li v-for="n in fib" :key="n">{{ n }}</li>
  </ul>
</template>

<script setup>
const fib = [0, 1, 1, 2, 3, 5, 8, 13]
</script>

<style scoped>
.fib { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
</style>
