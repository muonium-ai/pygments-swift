import XCTest
@testable import PygmentsSwift

final class CueLexerTests: XCTestCase {
    func testCueBasics() {
        let lexer = CueLexer()
        let input = """
        // comment
        package main
        fib: n: int: if n < 2 { n } else { fib(n-1) + fib(n-2) }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "package" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "if" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
