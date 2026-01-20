import XCTest
@testable import PygmentsSwift

final class GroovyLexerTests: XCTestCase {
    func testGroovyBasics() {
        let lexer = GroovyLexer()
        let input = """
        // comment
        def fib(int n) {
          if (n < 2) return n
          fib(n - 1) + fib(n - 2)
        }
        println("ok")
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "def" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
