<!-- Fibonacci (Svelte) -->
<script>
  const fib = [0, 1, 1, 2, 3, 5, 8, 13]
</script>

<ul>
  {#each fib as n}
    <li>{n}</li>
  {/each}
</ul>
