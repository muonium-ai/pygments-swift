import XCTest
@testable import PygmentsSwift

final class MermaidLexerTests: XCTestCase {
    func testMermaidBasics() {
        let lexer = MermaidLexer()
        let input = """
        %% comment
        flowchart TD
          A --> B
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "flowchart" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "-->" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
