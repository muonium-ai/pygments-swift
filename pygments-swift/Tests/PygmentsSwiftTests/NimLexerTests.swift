import XCTest
@testable import PygmentsSwift

final class NimLexerTests: XCTestCase {
    func testNimBasics() {
        let lexer = NimLexer()
        let input = """
        # comment
        proc fib(n: int): int =
          if n < 2: return n
          fib(n-1) + fib(n-2)

        let result = fib(10)
        echo result
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "proc" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "let" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
