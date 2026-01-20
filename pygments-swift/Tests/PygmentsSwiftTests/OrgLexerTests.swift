import XCTest
@testable import PygmentsSwift

final class OrgLexerTests: XCTestCase {
    func testOrgBasics() {
        let lexer = OrgLexer()
        let input = """
        * TODO Fibonacci
        #+TITLE: Fibonacci
        # comment
        Some =code= and ~verbatim~.
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .generic) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "TODO" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
