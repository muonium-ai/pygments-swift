Feature: Fibonacci

  Scenario: Compute fibonacci
    Given I have "10"
    When I compute fibonacci
    Then the sequence starts with "0,1,1,2,3"
