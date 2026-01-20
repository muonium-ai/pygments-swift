import XCTest
@testable import PygmentsSwift

final class KustoLexerTests: XCTestCase {
    func testKustoBasics() {
        let lexer = KustoLexer()
        let input = #"""
        let threshold = 5;
        StormEvents
        | where DamageProperty > threshold
        | summarize Count=count() by State
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .operator) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
