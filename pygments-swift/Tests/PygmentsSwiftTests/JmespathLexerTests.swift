import XCTest
@testable import PygmentsSwift

final class JmespathLexerTests: XCTestCase {
    func testJmespathBasics() {
        let lexer = JmespathLexer()
        let input = #"""
        foo.bar[?baz == 'qux'] | [0]
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "foo" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "|" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
