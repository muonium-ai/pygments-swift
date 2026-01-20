import XCTest
@testable import PygmentsSwift

final class GraphvizLexerTests: XCTestCase {
    func testGraphvizBasics() {
        let lexer = GraphvizLexer()
        let input = """
        // comment
        digraph G { A -> B; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "digraph" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "->" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
