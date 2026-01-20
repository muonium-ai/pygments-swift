import XCTest
@testable import PygmentsSwift

final class YamlLexerTests: XCTestCase {
    func testYamlBasics() {
        let lexer = YamlLexer()
        let input = """
        # comment
        ---
        name: fibonacci
        ok: true
        value: 55
        ...
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Attribute") && $0.value == "name" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "true" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .number && $0.value.contains("55") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
