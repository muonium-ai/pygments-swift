import XCTest
@testable import PygmentsSwift

final class DartLexerTests: XCTestCase {
    func testDartBasics() {
        let lexer = DartLexer()
        let input = """
        // comment
        int fib(int n) {
          if (n < 2) return n;
          return fib(n - 1) + fib(n - 2);
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "return" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name && $0.value == "fib" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
