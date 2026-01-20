import XCTest
@testable import PygmentsSwift

final class CodeQLLexerTests: XCTestCase {
    func testCodeQLBasics() {
        let lexer = CodeQLLexer()
        let input = #"""
        import javascript
        from Function f
        where f.getName() = "foo"
        select f
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "=" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
