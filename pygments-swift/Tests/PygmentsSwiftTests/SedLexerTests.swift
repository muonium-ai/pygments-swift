import XCTest
@testable import PygmentsSwift

final class SedLexerTests: XCTestCase {
    func testSedBasics() {
        let lexer = SedLexer()
        let input = """
        # comment
        1,10p
        s/fib/FIB/g
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "s" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "10" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
