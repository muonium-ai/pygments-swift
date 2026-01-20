import XCTest
@testable import PygmentsSwift

final class WebIdlLexerTests: XCTestCase {
    func testWebIdlBasics() {
        let lexer = WebIdlLexer()
        let input = """
        // comment
        interface Fibonacci {
          attribute long n;
          long fib(long n);
        };
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "interface" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "attribute" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
