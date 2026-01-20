import XCTest
@testable import PygmentsSwift

final class SmalltalkLexerTests: XCTestCase {
    func testSmalltalkBasics() {
        let lexer = SmalltalkLexer()
        let input = """
        "comment"
        fib := [:n | n < 2 ifTrue: [^ n]. ^ (n-1) + (n-2)].
        Transcript show: (fib value: 10) printString.
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == ":=" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
