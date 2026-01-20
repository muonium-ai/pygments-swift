import XCTest
@testable import PygmentsSwift

final class BatchfileLexerTests: XCTestCase {
    func testBatchfileBasics() {
        let lexer = BatchfileLexer()
        let input = """
        @echo off
        rem comment
        set n=10
        if %n%==10 echo ten
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "set" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value.contains("%n%") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
