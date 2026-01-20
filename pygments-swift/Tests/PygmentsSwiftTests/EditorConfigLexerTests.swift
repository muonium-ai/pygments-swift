import XCTest
@testable import PygmentsSwift

final class EditorConfigLexerTests: XCTestCase {
    func testEditorConfigBasics() {
        let lexer = EditorConfigLexer()
        let input = """
        # comment
        root = true

        [*.swift]
        indent_size = 4
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "[*.swift]" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "indent_size" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "=" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
