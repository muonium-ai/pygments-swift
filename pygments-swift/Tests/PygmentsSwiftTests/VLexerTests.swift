import XCTest
@testable import PygmentsSwift

final class VLexerTests: XCTestCase {
    func testVBasics() {
        let lexer = VLexer()
        let input = """
        // comment
        module main

        fn fib(n int) int {
            if n < 2 { return n }
            return fib(n-1) + fib(n-2)
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "fn" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
