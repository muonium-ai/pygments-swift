import XCTest
@testable import PygmentsSwift

final class ElixirLexerTests: XCTestCase {
    func testElixirBasics() {
        let lexer = ElixirLexer()
        let input = """
        # comment
        defmodule Fib do
          def fib(n) when n < 2, do: n
          def fib(n), do: fib(n - 1) + fib(n - 2)
        end
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "defmodule" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Class") && $0.value == "Fib" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
