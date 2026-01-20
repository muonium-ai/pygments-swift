-- Fibonacci (SQL)

CREATE TABLE fib_case (
  n INTEGER NOT NULL,
  expected INTEGER NOT NULL
);

INSERT INTO fib_case (n, expected) VALUES
  (0, 0),
  (1, 1),
  (10, 55),
  (11, 89);

SELECT n, expected
FROM fib_case
WHERE n >= 10
ORDER BY n;
