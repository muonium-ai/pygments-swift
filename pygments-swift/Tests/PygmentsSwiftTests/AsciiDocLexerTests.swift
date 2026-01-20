import XCTest
@testable import PygmentsSwift

final class AsciiDocLexerTests: XCTestCase {
    func testAsciiDocBasics() {
        let lexer = AsciiDocLexer()
        let input = """
        = Title
        :toc:

        // comment
        `fib(10)`
        Link: https://example.com
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .generic) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
