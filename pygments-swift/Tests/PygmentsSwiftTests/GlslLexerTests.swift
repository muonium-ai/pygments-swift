import XCTest
@testable import PygmentsSwift

final class GlslLexerTests: XCTestCase {
    func testGlslBasics() {
        let lexer = GlslLexer()
        let input = """
        // comment
        #version 330 core
        int fib(int n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }
        void main() { int x = fib(10); }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) && $0.value == "void" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
