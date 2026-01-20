import XCTest
@testable import PygmentsSwift

final class MarkdownLexerTests: XCTestCase {
    func testMarkdownBasics() {
        let lexer = MarkdownLexer()
        let input = """
        # Title

        - item 1
        - item 2

        Inline `code` and a [link](https://example.com)

        ```swift
        let x = 1
        print(x)
        ```
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(200).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "#" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("```") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("`code`") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
