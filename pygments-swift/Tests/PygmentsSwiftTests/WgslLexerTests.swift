import XCTest
@testable import PygmentsSwift

final class WgslLexerTests: XCTestCase {
    func testWgslBasics() {
        let lexer = WgslLexer()
        let input = """
        // comment
        fn fib(n: i32) -> i32 {
          if (n < 2) { return n; }
          return fib(n - 1) + fib(n - 2);
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "fn" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "return" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
