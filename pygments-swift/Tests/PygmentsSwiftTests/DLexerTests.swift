import XCTest
@testable import PygmentsSwift

final class DLexerTests: XCTestCase {
    func testDBasics() {
        let lexer = DLexer()
        let input = """
        // comment
        module main;
        import std.stdio;

        int fib(int n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "import" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
