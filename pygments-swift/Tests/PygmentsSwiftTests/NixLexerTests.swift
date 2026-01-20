import XCTest
@testable import PygmentsSwift

final class NixLexerTests: XCTestCase {
    func testNixBasics() {
        let lexer = NixLexer()
        let input = """
        # comment
        let
          fib = n: if n < 2 then n else (fib (n - 1)) + (fib (n - 2));
        in
          fib 10
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "let" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "then" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
