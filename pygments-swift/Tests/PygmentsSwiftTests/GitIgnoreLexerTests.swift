import XCTest
@testable import PygmentsSwift

final class GitIgnoreLexerTests: XCTestCase {
    func testGitIgnoreBasics() {
        let lexer = GitIgnoreLexer()
        let input = """
        # comment
        *.log
        !important.log
        node_modules/
        cache/[0-9][0-9]*/
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "!" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "*" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
