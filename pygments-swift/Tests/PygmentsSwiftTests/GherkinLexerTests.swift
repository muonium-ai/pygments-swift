import XCTest
@testable import PygmentsSwift

final class GherkinLexerTests: XCTestCase {
    func testGherkinBasics() {
        let lexer = GherkinLexer()
        let input = #"""
        @smoke
        Feature: Addition

          Scenario: Add two numbers
            Given I have "2"
            When I add 3
            Then I see 5
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
