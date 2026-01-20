import XCTest
@testable import PygmentsSwift

final class PrqlLexerTests: XCTestCase {
    func testPrqlBasics() {
        let lexer = PrqlLexer()
        let input = #"""
        from employees
        filter age > 30
        select {name, age}
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "from" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
