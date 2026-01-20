import XCTest
@testable import PygmentsSwift

final class AsymptoteLexerTests: XCTestCase {
    func testAsymptoteBasics() {
        let lexer = AsymptoteLexer()
        let input = #"""
        import graph;
        real x = 1;
        // comment
        "hello";
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
